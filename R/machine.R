
state_idx <- 1L

current_state <- function() {
  state_idx
}
next_state <- function() {
  state_idx <<- state_idx + 1L
  state_idx
}
reset_state <- function() {
  state_idx <<- 1L
}

machine_parts <- function(fn) {
  reset_state()

  body <- duplicate(body(fn))
  node <- as_exprs_node(body)

  parts <- node_list_parts(node)
  poke_returns(parts)

  parts
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
        parts <- node_list_poke_cdr(parts, node_list(node_list(pause)))
      } else {
        node_poke_cdr(parent, node_list(pause))
        parts <- node_list_poke_cdr(parts, node_list(node))
      }

      rest <- node <- node_cdr(rest)
      parent <- NULL
      next
    }

    # Nested states
    expr_parts <- expr_parts(expr)
    if (!is_null(expr_parts)) {
      # Add missing goto
      goto <- goto_lang(next_state())
      last <- node_car(node_list_tail(expr_parts))
      node_list_poke_cdr(last, node_list(goto))

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
  poke_attr(node, "tail", TRUE)
  remaining <- node_list(node)

  node_list_poke_cdr(parts, remaining)
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
  switch(head,
    `{` = node_list_parts(node_cdr(expr)),
    `if` = stop("todo if"),
    `while` = stop("todo while"),
    NULL
  )
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
