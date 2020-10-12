
generator_body <- function(fn) {
  walk_states(body(fn))
}

new_counter <- function(machine_depth, loop_depth = 0L) {
  i <- 1L
  function(inc = 0L) {
    i <<- i + inc
    i
  }
}
machine_depth <- function(counter) {
  env_get(fn_env(counter), "machine_depth")
}
loop_depth <- function(counter, check = TRUE) {
  depth <- env_get(fn_env(counter), "loop_depth")

  if (check && !depth) {
    abort("Must use `next` and `break` within a loop.")
  }

  depth
}

walk_states <- function(expr) {
  continue <- function(counter, last) {
    # Break if last
    if (last) 0L else counter() + 1L
  }
  states <- expr_states(expr, new_counter(1L), continue = continue, last = TRUE, return = TRUE)
  expr({
    if (killed()) {
      return(invisible(NULL))
    }
    repeat switch(state[[1L]], !!!states)
    kill()
    invisible(NULL)
  })
}
walk_loop_states <- function(body, condition, counter) {
  continue <- function(counter, last) {
    # Go back to state 1 of loop body if last
    if (last) 1L else counter() + 1L
  }

  machine_depth <- machine_depth(counter) + 1L
  nested_counter <- new_counter(machine_depth, loop_depth = machine_depth)

  if (!is_null(condition)) {
    nested_counter(inc = 1L)

    condition_block <- block(expr(
      if (!!condition) {
        set_state(!!2L)
      } else {
        break
      }
    ))

    states <- new_state(condition_block, NULL, 1L)
  } else {
    states <- NULL
  }

  nested_states <- expr_states(body, nested_counter, continue = continue, last = TRUE, return = FALSE)
  states <- node_list_poke_cdr(states, nested_states)

  expr(repeat switch(state[[!!machine_depth]], !!!states))
}
walk_branch_states <- function(body, offset, counter, continue, last, return) {
  nested_continue <- function(counter, last) {
    counter() + 1L
  }

  prev_depth <- machine_depth(counter)
  machine_depth <- prev_depth + 1L
  loop_depth <- loop_depth(counter, check = FALSE)
  nested_counter <- new_counter(machine_depth, loop_depth = loop_depth)

  states <- expr_states(
    body,
    nested_counter,
    continue = nested_continue,
    last = last,
    return = return
  )

  breaking_state <- new_state(quote({ break }), NULL, nested_counter())
  states <- node_list_poke_cdr(states, breaking_state)
  nested_counter(inc = 1L)

  # Don't add offset if state is breaking
  next_i <- continue(counter, last)
  if (next_i) {
    next_i <- next_i + offset
  }

  expr({
    repeat switch(state[[!!machine_depth]], !!!states)
    n <- depth()
    if (n < !!prev_depth) break
    if (n == !!prev_depth) { set_state(1L); next }
    set_depth(!!prev_depth)
    !!continue_call(next_i)
  })
}

expr_states <- function(expr, counter, continue, last, return) {
  switch(expr_type(expr),
    `{` = block_states(
      block = expr,
      counter = counter,
      continue = continue,
      last = last,
      return = return
    ),
    `expr` = continue_state(
      expr = expr,
      counter = counter,
      continue = continue,
      last = last,
      return = return
    ),
    `yield` = yield_state(
      expr = strip_yield(expr),
      counter = counter,
      continue = continue,
      last = last,
      return = return
    ),
    `return` = return_state(
      expr = expr,
      counter = counter
    ),
    `if` = if_states(
      preamble = NULL,
      condition = user_call(node_cadr(expr)),
      then_body = node_cadr(node_cdr(expr)),
      else_body = node_cadr(node_cddr(expr)),
      counter = counter,
      continue = continue,
      last = last,
      return = return
    ),
    `break` = break_state(
      preamble = NULL,
      counter = counter
    ),
    `next` = next_state(NULL, counter),
    `repeat` = loop_states(
      preamble = NULL,
      init = NULL,
      condition = NULL,
      body = node_cadr(expr),
      cleanup = NULL,
      counter = counter,
      continue = continue,
      last = last
    ),
    `while` = loop_states(
      preamble = NULL,
      init = NULL,
      condition = user_call(node_cadr(expr)),
      body = node_cadr(node_cdr(expr)),
      cleanup = NULL,
      counter = counter,
      continue = continue,
      last = last
    ),
    `for` = for_states(
      preamble = NULL,
      var = node_cadr(expr),
      iterator = node_cadr(node_cdr(expr)),
      body = node_cadr(node_cddr(expr)),
      counter = counter,
      continue = continue,
      last = last
    ),
    `tryCatch` = ,
    `on.exit` = stop_internal("expr_states", sprintf("Unimplemented operation `%s`", expr_type(expr))),
    stop_internal("expr_states", sprintf("Unexpected operation `%s`", expr_type(expr)))
  )
}

expr_type <- function(expr) {
  default <- "expr"

  if (!is_call(expr)) {
    return(default)
  }

  head <- node_car(expr)
  if (!is_symbol(head)) {
    if (is_call(expr, "yield", ns = c("", "flowery"))) {
      return("yield")
    } else {
      return(default)
    }
  }

  head <- as_string(head)
  switch(head,
    `{` = ,
    `yield` = ,
    `return` = ,
    `if` = ,
    `repeat` = ,
    `while` = ,
    `for` = ,
    `break` = ,
    `next` = ,
    `tryCatch` = ,
    `on.exit` = head,
    default
  )
}

block_states <- function(block, counter, continue, last, return) {
  # Only the last expression of `block` is returnable
  saved_last <- last
  last <- FALSE

  block <- duplicate(block, shallow = TRUE)
  refs <- block_refs(block)
  states <- NULL

  node <- node_cdr(block)

  # Handle to previous node so we can pop `node` from the current
  # pairlist
  prev_node <- NULL
  prev_refs <- NULL

  # Handle to the current head of pairlists of expressions. New user
  # blocks are created starting from this handle.
  curr_node <- node
  curr_refs <- refs

  accum <- function(go) {
    prev_node <<- node
    prev_refs <<- refs
    node <<- node_cdr(node)
    refs <<- node_cdr(refs)

    go
  }
  collect <- function() {
    if (is_null(curr_node)) {
      # Happens on skip
      curr_node <<- node
      curr_refs <<- refs
      return()
    }

    next_node <- node_cdr(node)
    next_refs <- node_cdr(refs)

    node <<- node_poke_cdr(node, NULL)
    refs <<- node_poke_cdr(node, NULL)

    out <- new_refd_block(curr_node, curr_refs)

    prev_node <<- NULL
    prev_refs <<- NULL
    node <<- curr_node <<- next_node
    refs <<- curr_refs <<- next_refs

    out
  }
  skip <- function() {
    if (is_null(prev_node)) {
      curr_node <<- NULL
      curr_refs <<- NULL
      node <<- node_cdr(node)
      refs <<- node_cdr(refs)
    } else {
      node_poke_cdr(prev_node, NULL)
      node_poke_cdr(prev_refs, NULL)
    }
  }

  push_states <- function(state) {
    states <<- node_list_poke_cdr(states, state)
  }

  # Collect as many user expressions as possible
  while (!is_null(node)) {
    # Set last expression of `block` as returnable
    if (is_null(node_cdr(node))) {
      last <- saved_last
    }

    expr <- node_car(node)
    ref <- node_car(refs)
    type <- expr_type(expr)

    switch(type,
      `expr` = {
        accum(next)
      },
      `yield` = {
        node_poke_car(node, strip_yield(expr))
        push_states(yield_state(
          collect(),
          counter,
          continue = continue,
          last = last,
          return = return
        ))
        next
      },
      `return` = {
        node_poke_car(node, strip_explicit_return(expr))
        push_states(return_state(collect(), counter))
        next
      },
      `if` = {
        skip()
        push_states(if_states(
          preamble = collect(),
          condition = user_call(refd_block(node_cadr(expr), ref)),
          then_body = node_cadr(node_cdr(expr)),
          else_body = node_cadr(node_cddr(expr)),
          counter = counter,
          continue = continue,
          last = last,
          return = return
        ))
        next
      },
      `repeat` = {
        node_poke_car(node, "repeat")
        push_states(loop_states(
          preamble = collect(),
          init = NULL,
          condition = NULL,
          body = node_cadr(expr),
          cleanup = NULL,
          counter = counter,
          continue = continue,
          last = last
        ))
        next
      },
      `while` = {
        skip()
        push_states(loop_states(
          preamble = collect(),
          init = NULL,
          condition = user_call(refd_block(node_cadr(expr), ref)),
          body = node_cadr(node_cdr(expr)),
          cleanup = NULL,
          counter = counter,
          continue = continue,
          last = last
        ))
        next
      },
      `for` = {
        skip()
        push_states(for_states(
          preamble = collect(),
          var = node_cadr(expr),
          iterator = node_cadr(node_cdr(expr)),
          body = node_cadr(node_cddr(expr)),
          counter = counter,
          continue = continue,
          last = last
        ))
        next
      },
      `break` = {
        node_poke_car(node, "break")
        push_states(break_state(collect(), counter))
        next
      },
      `next` = {
        node_poke_car(node, "next")
        push_states(next_state(collect(), counter))
        next
      }
    )

    abort(sprintf("TODO in `block_states()`: %s", type))
  }

  last <- saved_last

  if (!is_null(curr_node)) {
    block <- new_refd_block(curr_node, curr_refs)
    push_states(continue_state(block, counter, continue = continue, last = last, return = return))
  }

  states
}

return_state <- function(expr, counter) {
  expr <- strip_explicit_return(expr)

  block <- expr({
    !!user_call(expr)
    kill()
    return(last_value())
  })
  state <- new_state(block, NULL, tag = counter())
  counter(inc = 1L)

  state
}
strip_explicit_return <- function(expr) {
  if (is_call(expr, "return")) {
    return(node_cadr(expr))
  }

  if (is_call(expr, "{") && is_call(node_list_tail(expr), "return")) {
    expr <- duplicate(expr, shallow = TRUE)
    tail <- node_list_tail(expr)
    node_poke_car(tail, strip_explicit_return(node_car(tail)))
  }

  expr
}

continue_state <- function(expr, counter, continue, last, return) {
  if (last && return) {
    return(return_state(expr, counter))
  }

  i <- counter()
  next_i <- continue(counter, last)

  block <- expr({
    !!user_call(expr)
    !!continue_call(next_i)
  })
  state <- new_state(block, NULL, tag = i)
  counter(inc = 1L)

  state
}

yield_state <- function(expr, counter, continue, last = FALSE, return = FALSE) {
  if (last && return) {
    return(return_state(expr, counter))
  }

  i <- counter()
  next_i <- continue(counter, last)

  block <- expr({
    !!user_call(expr)
    suspend_to(!!next_i)
    return(last_value())
  })
  state <- new_state(block, NULL, tag = i)
  counter(inc = 1L)

  state
}
strip_yield <- function(expr) {
  node_cadr(expr)
}

if_states <- function(preamble, condition, then_body, else_body, counter, continue, last, return) {
  i <- counter()
  i_then <- i + 1L
  i_else <- i_then + 1L

  i_machine <- machine_depth(counter)

  if_block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    if (!!condition) {
      set_state(!!i_then)
    } else {
      set_state(!!i_else)
    }
    set_depth(!!(i_machine + 1L))
  })
  states <- new_state(if_block, NULL, i)
  counter(inc = 1L)

  offset <- if (is_null(else_body)) 0L else 1L
  then_machine <- walk_branch_states(then_body, offset, counter, continue, last, return)
  then_state <- new_state(then_machine, NULL, i_then)
  states <- node_list_poke_cdr(states, then_state)
  counter(inc = 1L)

  if (!is_null(else_body)) {
    else_machine <- walk_branch_states(else_body, 0L, counter, continue, last, return)
    else_state <- new_state(else_machine, NULL, i_else)
    states <- node_list_poke_cdr(states, else_state)
    counter(inc = 1L)
  }

  if (last) {
    after_block <- block(quote(break))
    after_state <- new_state(after_block, NULL, counter())
    states <- node_list_poke_cdr(states, after_state)
    counter(inc = 1L)
  }

  states
}

loop_states <- function(preamble, init, condition, body, cleanup, counter, continue, last) {
  states <- NULL
  i <- counter()
  next_i <- i + 1L
  depth <- machine_depth(counter)

  preamble_block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    !!!init %&&% list(init)
    set_state(!!next_i)
    set_depth(!!(depth + 1L))
  })

  preamble_state <- new_state(preamble_block, NULL, i)
  states <- node_list_poke_cdr(states, preamble_state)
  i <- counter(inc = 1L)
  next_i <- if (last) 0L else i + 1L

  nested_machine_block <- expr({
    !!walk_loop_states(body, condition, counter)
    set_depth(!!depth)
    !!!cleanup %&&% list(cleanup)
    !!continue_call(next_i)
  })
  nested_machine_state <- new_state(nested_machine_block, NULL, i)
  states <- node_list_poke_cdr(states, nested_machine_state)
  counter(inc = 1L)

  states
}

for_states <- function(preamble, var, iterator, body, counter, continue, last) {
  loop_depth <- machine_depth(counter) + 1L

  init <- expr(
    iterators[[!!loop_depth]] <- as_iterator(!!user_call(iterator))
  )
  condition <- expr({
    iterator <- iterators[[!!loop_depth]]
    if (is_null(elt <- iterator())) {
      FALSE
    } else {
      user_env[[!!as.character(var)]] <- elt
      TRUE
    }
  })
  cleanup <- expr(
    iterators[[!!loop_depth]] <- NULL
  )

  loop_states(
    preamble = preamble,
    init = init,
    condition = condition,
    body = body,
    cleanup = cleanup,
    counter = counter,
    continue = continue,
    last = last
  )
}

break_state <- function(preamble, counter) {
  loop_depth <- loop_depth(counter)

  if (loop_depth == machine_depth(counter)) {
    continue <- expr({ break })
  } else {
    continue <- expr({
      set_depth(!!(loop_depth - 1L))
      break
    })
  }

  block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    !!!continue
  })
  state <- new_state(block, NULL, counter())
  counter(inc = 1L)

  state
}

next_state <- function(preamble, counter) {
  loop_depth <- loop_depth(counter)

  if (loop_depth == machine_depth(counter)) {
    continue <- expr({ set_state(1L) })
  } else {
    continue <- expr({
      set_depth(!!loop_depth)
      break
    })
  }

  block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    !!!continue
  })
  state <- new_state(block, NULL, counter())
  counter(inc = 1L)

  state
}

# Must be trailing or before a `next` statement
continue_call <- function(next_i) {
  if (next_i) {
    expr(set_state(!!next_i))
  } else {
    quote(break)
  }
}

new_state <- function(car, cdr, tag) {
  node <- new_node(car, cdr)

  if (!is_null(tag)) {
    node_poke_tag(node, sym(as.character(tag)))
  }

  node
}

expr_parts <- function(expr, refs) {
  if (!is_call(expr)) {
    return(NULL)
  }

  head <- node_car(expr)
  if (!is_symbol(head)) {
    return(NULL)
  }

  head <- as_string(head)
  switch(head,
    `{` = block_parts(expr),
    `if` = if_parts(expr),
    `repeat` = repeat_parts(expr),
    `while` = while_parts(expr, refs),
    `for` = for_parts(expr),
    `break` = break_parts(expr),
    `next` = next_parts(expr),
    NULL
  )
}

block_push_goto <- function(block, goto_node = NULL) {
  if (is_exiting_block(block)) {
    block
  } else {
    goto_node <- goto_node %||% peek_goto_node()
    node_list_poke_cdr(block, goto_node)
  }
}

is_pause <- function(x) {
  is_call(x, peek_pause_sym())
}
