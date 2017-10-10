
block_parts <- function(expr) {
  parts <- node_list_parts(node_cdr(expr))

  if (is_null(parts)) {
    NULL
  } else {
    push_goto(node_list_tail_car(parts))
    parts
  }
}

node_list_parts <- function(node) {
  rest <- node
  parts <- NULL
  parent <- NULL

  has_future <- function() {
    !is_null(node_cdr(rest))
  }
  has_past <- function() {
    !is_null(parent)
  }

  while (!is_null(rest)) {
    expr <- node_car(rest)

    if (is_pause(expr)) {
      if (has_future()) {
        pause_node <- node_list(pause_lang(poke_state()))
      } else {
        pause_node <- peek_pause_node()
      }

      if (has_past()) {
        node_poke_cdr(parent, pause_node)
        pause_block <- new_block(node)
      } else {
        pause_block <- new_block(pause_node)
      }
      parts <- node_list_poke_cdr(parts, node_list(pause_block))

      rest <- node <- node_cdr(rest)
      parent <- NULL
      next
    }

    # Extract nested states. If there is a continuation, pass on the
    # relevant goto and pause nodes. Fill those nodes only when we
    # extracted the parts so they get the right state index.
    if (has_future()) {
      next_goto <- node(goto_lang(-1L), NULL)
      next_pause <- node(pause_lang(-1L), NULL)

      with_jump_nodes(next_goto, next_pause, {
        nested_parts <- expr_parts(expr)
      })
      if (!is_null(nested_parts)) {
        poke_state()
        node_poke_car(next_goto, goto_lang(peek_state()))
        node_poke_car(next_pause, pause_lang(peek_state()))
      }
    } else {
      nested_parts <- expr_parts(expr)
    }

    if (is_null(nested_parts)) {
      parent <- rest
      rest <- node_cdr(rest)
      next
    }

    # If we found nested states, check if there are any past
    # expressions to add them before the pausing block.
    pausing_part <- node_car(nested_parts)

    if (has_past()) {
      if (is_spliceable(pausing_part)) {
        pausing_part <- node_cdr(pausing_part)
      } else {
        pausing_part <- node_list(pausing_part)
      }

      node_poke_cdr(parent, pausing_part)
      node_poke_car(nested_parts, new_block(node))
    } else {
      poke_attr(pausing_part, "spliceable", NULL)
    }

    # Merge nested states
    parts <- node_list_poke_cdr(parts, nested_parts)

    rest <- node <- node_cdr(rest)
    parent <- NULL
    next
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

is_exiting_block <- function(x) {
  if (!is_named_language(x)) {
    return(FALSE)
  }

  head <- as_string(node_car(x))

  switch(head,
    `if` = {
      if (!is_exiting_block(if_branch_true(x))) {
        return(FALSE)
      }
      is_exiting_block(if_branch_else(x))
    },

    `{`  = {
      last <- node_car(node_list_tail(x))
      is_exiting_block(last)
    },

    is_language(x, exiting_syms)
  )
}
exiting_syms <- list(return_sym, pause_sym, goto_sym)
