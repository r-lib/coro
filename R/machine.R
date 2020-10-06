
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
  continue <- function(expr, counter, last = FALSE) {
    if (last) {
      return_state(expr, counter)
    } else {
      continue_state(expr, counter)
    }
  }
  states <- expr_states(expr, new_counter(1L), continue = continue, last = TRUE)
  expr(repeat switch(state[[1L]], !!!states, final = { return(invisible(NULL)) }))
}
walk_nested_states <- function(expr, counter) {
  continue <- function(expr, counter, last = FALSE) {
    next_state(expr, counter)
  }

  i <- machine_count(counter) + 1L
  nested_counter <- new_counter(i)

  states <- expr_states(expr, nested_counter, continue = continue, last = FALSE)
  expr(repeat switch(state[[!!i]], !!!states))
}

expr_states <- function(expr, counter, continue, last) {
  switch(expr_type(expr),
    `{` = block_states(expr, counter, continue = continue, last = last),
    `expr` = continue(expr, counter, last = last),
    `yield` = yield_state(strip_yield(expr), counter, continue = continue, last = last),
    `return` = return_state(expr, counter),
    `if` = ,
    `repeat` = ,
    `while` = ,
    `for` = ,
    `break` = ,
    `next` = ,
    `tryCatch` = ,
    `on.exit` = stop_internal("expr_states", sprintf("Unimplemented operation `%s`", type)),
    stop_internal("expr_states", sprintf("Unexpected operation `%s`", type))
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

block_states <- function(block, counter, continue, last) {
  # Only the last expression of `block` is returnable
  saved_last <- last
  last <- FALSE

  node <- duplicate(node_cdr(block), shallow = TRUE)
  refs <- block_refs(block)
  states <- NULL

  curr_node <- node
  curr_refs <- refs

  accum <- function(go) {
    node <<- node_cdr(node)
    refs <<- node_cdr(refs)
    go
  }
  collect <- function() {
    next_node <- node_cdr(node)
    next_refs <- node_cdr(refs)

    node <<- node_poke_cdr(node, NULL)
    refs <<- node_poke_cdr(node, NULL)

    out <- new_refd_block(curr_node, curr_refs)

    node <<- curr_node <<- next_node
    refs <<- curr_refs <<- next_refs

    out
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
    type <- expr_type(expr)

    switch(type,
      `expr` = {
        accum(next)
      },
      `yield` = {
        node_poke_car(node, strip_yield(expr))
        push_states(yield_state(collect(), counter, continue = continue, last = last))
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
      }
    )

    stop("TODO")
  }

  if (!is_null(curr_node)) {
    block <- new_refd_block(curr_node, curr_refs)
    push_states(continue(block, counter, last = saved_last))
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

continue_state <- function(expr, counter) {
  stop("TODO: Is this reachable?")

  i <- counter()

  block <- expr({
    !!user_call(expr)
    goto(!!(i + 1L))
  })
  new_state(block, NULL, tag = i)
}

yield_state <- function(expr, counter, continue, last) {
  if (last) {
    return(continue(expr, counter, last = TRUE))
  }

  i <- counter()

  block <- expr({
    !!user_call(expr)
    suspend_to(!!(i + 1L))
    return(last_value())
  })
  new_state(block, NULL, tag = i)
}
strip_yield <- function(expr) {
  node_cadr(expr)
}

loop_states <- function(preamble, condition, body, counter, continue, last) {
  i <- counter()
  next_i <- counter(inc = 1L)

  preamble_block <- expr({
    !!user_call(preamble)
    push_machine(loop = TRUE)
    goto(!!next_i)
  })
  states <- new_state(preamble_block, NULL, i)

  i <- next_i
  tail <- states

  if (!is_null(condition)) {
    condition_state <- new_state(block(condition), NULL, i)
    node_poke_cdr(tail, condition_state)

    i <- counter(inc = 1L)
    tail <- condition_state
  }

  nested_machine_block <- block(walk_nested_states(body, counter))
  nested_machine_state <- new_state(nested_machine_block, NULL, i)
  node_poke_cdr(tail, nested_machine_state)

  states
}

next_state <- function(expr, counter) {
  block <- expr({
    !!user_call(expr)
    goto(1)
  })
  new_state(block, NULL, tag = counter())
}

states <- function(...) {
  pairlist2(...)
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
