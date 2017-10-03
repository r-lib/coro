
state <- new_environment(list(idx = 1L))

peek_state <- function() {
  state$idx
}
poke_state <- function() {
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
      pause <- pause_lang(poke_state())

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
      first <- node_car(expr_parts)

      # If any past expressions add them to pausing block
      if (!is_null(parent)) {
        pausing_block <- first

        # Only add a goto if there might be code reaching the
        # continuation from a non-yielding branch
        if (is_language(first, if_sym)) {
          pausing_exprs <- node_list(goto_lang(peek_state()))
        } else {
          pausing_exprs <- NULL
        }
        pausing_exprs <- node(pausing_block, pausing_exprs)

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

  # `node` may be NULL if there is no expression after a pause
  if (!is_null(node)) {
    remaining <- new_block(node)
    node_list_poke_cdr(parts, node_list(remaining))
  }

  parts
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

  if (is_null(parts)) {
    return(parts)
  }

  # Add missing goto
  last_block <- node_car(node_list_tail(parts))
  if (!is_exiting_block(last_block)) {
    goto_node <- node_list(goto_lang(poke_state()))
    node_list_poke_cdr(last_block, goto_node)
  }

  parts
}

is_pause <- function(x) {
  is_language(x, quote(yield))
}

is_exiting_block <- function(x) {
  if (is_null(x)) {
    return(FALSE)
  }
  last <- node_car(node_list_tail(x))
  is_language(last, exiting_syms)
}
exiting_syms <- list(return_sym, pause_sym, goto_sym)
