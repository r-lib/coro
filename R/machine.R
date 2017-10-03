
state <- new_environment(list(idx = 1L))

current_state <- function() {
  state$idx
}
next_state <- function() {
  state$idx <- state$idx + 1L
  state$idx
}
reset_state <- function() {
  state$idx <- 1L
}

machine_parts <- function(fn) {
  reset_state()

  node <- set_returns(fn)
  node_list_parts(node)
}

node_list_parts <- function(node) {
  rest <- node
  parts <- NULL
  parent <- NULL

  while (!is_null(rest)) {
    expr <- node_car(rest)

    # Pausing state
    if (is_pause(expr)) {
      pause <- pause_lang(next_state())

      if (is_null(parent)) {
        pause_block <- new_block(node_list(pause))
      } else {
        node_poke_cdr(parent, node_list(pause))
        pause_block <- new_block(node)
      }
      parts <- node_list_poke_cdr(parts, node_list(pause_block))

      rest <- node <- node_cdr(rest)
      parent <- NULL
      next
    }

    # Nested states
    expr_parts <- expr_parts(expr)
    if (!is_null(expr_parts)) {
      # If any past expressions add them to pausing block
      if (!is_null(parent)) {
        pausing_block <- node_car(expr_parts)
        pausing_exprs <- node_list(pausing_block, goto_lang(current_state()))
        node_poke_cdr(parent, pausing_exprs)
        node_poke_car(expr_parts, new_block(node))
      }

      # Merge nested states
      parts <- node_list_poke_cdr(parts, expr_parts)

      rest <- node <- node_cdr(rest)
      parent <- NULL
      next
    }

    parent <- rest
    rest <- node_cdr(rest)
  }

  if (is_null(parts)) {
    return(NULL)
  }

  # `node` maybe be NULL in case of empty continuation
  if (!is_pairlist(node)) {
    node <- null_node()
  }
  remaining <- new_block(node)
  poke_attr(remaining, "tail", TRUE)

  node_list_poke_cdr(parts, node_list(remaining))
}

pause_lang <- function(idx, ...) {
  lang("_pause", as.character(idx), ...)
}
goto_lang <- function(idx) {
  lang("_goto", as.character(idx))
}
return_lang <- function(...) {
  lang("return", ...)
}


expr_parts <- function(expr) {
  if (!is_language(expr)) {
    return(NULL)
  }

  head <- node_car(expr)
  if (!is_symbol(head)) {
    return(NULL)
  }

  head <- as_string(head)
  parts <- switch(head,
    `{` = node_list_parts(node_cdr(expr)),
    `if` = stop("todo if"),
    `while` = stop("todo while"),
    NULL
  )

  # Add missing goto
  goto_node <- node_list(goto_lang(next_state()))
  last_block <- node_car(node_list_tail(parts))
  node_list_poke_cdr(last_block, goto_node)

  parts
}


poke_returns <- function(parts) {
  last_part <- node_car(node_list_tail(parts))
  if (!is_tail(last_part)) {
    return(NULL)
  }

  last_node <- node_list_tail(last_part)
  if (is_null_node(last_node)) {
    last_expr <- return_lang(NULL)
  } else {
    last_expr <- return_lang(node_car(last_node))
  }
  node_poke_car(last_node, last_expr)
}



is_pause <- function(x) {
  is_language(x, quote(yield))
}
is_tail <- function(x) {
  is_true(attr(x, "tail"))
}
