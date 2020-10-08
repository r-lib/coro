
generator_body <- function(fn) {
  walk_states(body(fn))
}

new_counter <- function(machine_i) {
  i <- 1L
  function(inc = 0L) {
    i <<- i + inc
    i
  }
}
machine_count <- function(counter) {
  env_get(fn_env(counter), "machine_i")
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

  machine_i <- machine_count(counter) + 1L
  nested_counter <- new_counter(machine_i)

  if (!is_null(condition)) {
    nested_counter(inc = 1L)

    condition_block <- block(expr(
      if (!!user_call(condition)) {
        goto(!!2L)
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

  expr(repeat switch(state[[!!machine_i]], !!!states))
}

expr_states <- function(expr, counter, continue, last, return) {
  switch(expr_type(expr),
    `{` = block_states(expr, counter, continue = continue, last = last, return = return),
    `expr` = continue_state(expr, counter, continue = continue, last = last, return = return),
    `yield` = yield_state(strip_yield(expr), counter, continue = continue, last = last, return = return),
    `return` = return_state(expr, counter),
    `break` = break_state(NULL, counter),
    `if` = ,
    `repeat` = ,
    `while` = ,
    `for` = ,
    `next` = ,
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
    if (is_null(node)) {
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
      node <<- curr_node <<- NULL
      refs <<- curr_refs <<- NULL
    } else {
      node <<- node_cdr(node)
      refs <<- node_cdr(refs)
      node_poke_cdr(prev_node, node)
      node_poke_cdr(prev_refs, refs)
    }
  }

  push_states <- function(state) {
    states <<- node_list_poke_cdr(states, state)
    counter(inc = 1L)
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
      `repeat` = {
        node_poke_car(node, "repeat")
        push_states(loop_states(
          preamble = collect(),
          body = node_cadr(expr),
          condition = NULL,
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
          body = node_cadr(node_cdr(expr)),
          condition = user_call(refd_block(node_cadr(expr), ref)),
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
  new_state(block, NULL, tag = counter())
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
  if (return) {
    return(return_state(expr, counter))
  }

  i <- counter()
  next_i <- continue(counter, last)

  block <- expr({
    !!user_call(expr)
    !!continue_call(next_i)
  })
  new_state(block, NULL, tag = i)
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
  new_state(block, NULL, tag = i)
}
strip_yield <- function(expr) {
  node_cadr(expr)
}

loop_states <- function(preamble, condition, body, counter, continue, last) {
  i <- counter()
  states <- NULL

  next_i <- counter(inc = 1L)

  preamble_block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    push_machine(loop = TRUE)
    goto(!!next_i)
  })

  states <- node_list_poke_cdr(states, new_state(preamble_block, NULL, i))

  i <- next_i
  next_i <- if (last) 0L else i + 1L

  nested_machine_block <- expr({
    !!walk_loop_states(body, condition, counter)
    pop_machine()
    !!continue_call(next_i)
  })
  nested_machine_state <- new_state(nested_machine_block, NULL, i)
  states <- node_list_poke_cdr(states, nested_machine_state)

  states
}

break_state <- function(preamble, counter) {
  # `pop_to_loop()` is a no-op if there is no intervening state
  # machines. Otherwise, it shortens the state vector up to (but not
  # including) the next loop state.

  block <- expr({
    !!!preamble %&&% list(user_call(preamble))
    pop_to_loop()
    break
  })
  new_state(block, NULL, counter())
}

continue_call <- function(next_i) {
  if (next_i) {
    expr(goto(!!next_i))
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
