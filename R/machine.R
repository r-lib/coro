
generator_body <- function(fn) {
  walk_states(body(fn))
}

new_counter <- function() {
  i <- 1L
  function(inc = 0L) {
    i <<- i + inc
    i
  }
}

walk_states <- function(expr) {
  states <- expr_states(expr, new_counter(), return = TRUE)
  expr(repeat switch(state[[1]], !!!states, final = { return(invisible(NULL)) }))
}

expr_states <- function(expr, counter, return = FALSE) {
  switch(expr_type(expr),
    `{` = block_states(expr, counter, return = return),
    `expr` = expr_state(expr, counter, return = return),
    `yield` = yield_state(strip_yield(expr), counter, return = return),
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

block_states <- function(block, counter, return = FALSE) {
  # Only the last expression of `block` is returnable
  saved_return <- return
  return <- FALSE

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

  push_state <- function(state) {
    states <<- node_list_poke_cdr(states, state)
    counter(inc = 1L)
  }

  # Collect as many user expressions as possible
  while (!is_null(node)) {
    # Set last expression of `block` as returnable
    if (is_null(node_cdr(node))) {
      return <- saved_return
    }

    expr <- node_car(node)
    type <- expr_type(expr)

    switch(type,
      `expr` = {
        accum(next)
      },
      `yield` = {
        node_poke_car(node, strip_yield(expr))
        push_state(yield_state(collect(), counter, return = return))
        next
      },
      `return` = {
        node_poke_car(node, strip_explicit_return(expr))
        push_state(return_state(collect(), counter))
        next
      }
    )

    stop("TODO")
  }

  if (!is_null(curr_node)) {
    block <- new_refd_block(curr_node, curr_refs)
    push_state(finishing_state(block, counter, return = return))
  }

  states
}

expr_state <- function(expr, counter, return = FALSE) {
  if (return) {
    return_state(expr, counter)
  } else {
    stop("TODO")
  }
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

yield_state <- function(expr, counter, return = FALSE) {
  i <- counter()

  if (return) {
    suspend_call <- expr(kill())
  } else {
    suspend_call <- expr(suspend_to(!!(i + 1L)))
  }

  block <- expr({
    !!user_call(expr)
    !!suspend_call
    return(last_value())
  })
  new_state(block, NULL, tag = i)
}
strip_yield <- function(expr) {
  node_cadr(expr)
}

finishing_state <- function(expr, counter, return = FALSE) {
  if (return) {
    return_state(expr, counter)
  } else {
    stop("TODO")
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
