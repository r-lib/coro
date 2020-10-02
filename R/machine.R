
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

  states <- node_reverse(states)
  expr(repeat switch(state[[1]], !!!states, final = { return(invisible(NULL)) }))
}

expr_states <- function(expr, counter, return = FALSE) {
  switch(expr_type(expr),
    `expr` = expr_state(expr, counter, return = return),
    `return` = return_state(expr, counter),
    `yield` = yield_state(expr, counter, return = return),
    `{` = ,
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
    `return` = ,
    `yield` = ,
    `{` = ,
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
    !!user_block(expr)
    kill()
    return(last_value())
  })
  new_state(block, NULL, tag = counter())
}

strip_explicit_return <- function(expr) {
  if (is_call(expr, "return")) {
    return(node_cadr(expr))
  }

  if (is_call(expr, "{") && is_call(node_list_tail(expr, "return"))) {
    expr <- duplicate(expr, shallow = TRUE)
    tail <- node_list_tail(expr)
    node_poke_car(tail, strip_explicit_return(node_car(tail)))
  }

  expr
}

yield_state <- function(expr, counter, return = FALSE) {
  i <- counter()
  expr <- node_cadr(expr)

  if (return) {
    suspend_call <- expr(kill())
  } else {
    suspend_call <- expr(suspend_to(!!(i + 1L)))
  }

  block <- expr({
    !!user_block(expr)
    !!suspend_call
    return(last_value())
  })
  new_state(block, NULL, tag = i)
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
