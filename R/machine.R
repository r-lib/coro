
machine_parts <- function(fn) {
  reset_state()

  node <- set_returns(fn)
  node_list_parts(node)
}

node_list_parts <- function(node) {
  rest <- node
  parts <- NULL
  parent <- NULL

  is_trailing <- function() {
    is_null(node_cdr(rest))
  }

  while (!is_null(rest)) {
    expr <- node_car(rest)

    if (is_pause(expr)) {
      if (is_trailing()) {
        pause_node <- peek_pause_node()
      } else {
        pause_node <- node_list(pause_lang(poke_state()))
      }

      if (is_null(parent)) {
        pause_block <- new_block(pause_node)
      } else {
        node_poke_cdr(parent, pause_node)
        pause_block <- new_block(node)
      }
      parts <- node_list_poke_cdr(parts, node_list(pause_block))

      rest <- node <- node_cdr(rest)
      parent <- NULL
      next
    }

    # Extract nested states. If there is a continuation, pass on the
    # relevant goto and pause nodes. Fill those nodes only when we
    # extracted the parts so they get the right state index.
    if (is_null(node_cdr(rest))) {
      expr_parts <- expr_parts(expr)
    } else {
      next_goto <- node(goto_lang(-1L), NULL)
      next_pause <- node(pause_lang(-1L), NULL)

      with_jump_nodes(next_goto, next_pause, {
        expr_parts <- expr_parts(expr)
      })
      if (!is_null(expr_parts)) {
        poke_state()
        node_poke_car(next_goto, goto_lang(peek_state()))
        node_poke_car(next_pause, pause_lang(peek_state()))
      }
    }

    if (is_null(expr_parts)) {
      parent <- rest
      rest <- node_cdr(rest)
    } else {
      # If we found nested states, check if there are any past
      # expressions to add them before the pausing block.
      pausing_part <- node_car(expr_parts)
      if (is_null(parent)) {
        poke_attr(pausing_part, "spliceable", NULL)
      } else {
        if (is_spliceable(pausing_part)) {
          pausing_part <- node_cdr(pausing_part)
        } else {
          pausing_part <- node_list(pausing_part)
        }

        node_poke_cdr(parent, pausing_part)
        node_poke_car(expr_parts, new_block(node))
      }

      # Merge nested states
      parts <- node_list_poke_cdr(parts, expr_parts)

      rest <- node <- node_cdr(rest)
      parent <- NULL
    }

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
    `{` = block_parts(expr),
    `if` = if_parts(expr),
    `repeat` = stop("todo loops"),
    NULL
  )
}

block_parts <- function(expr) {
  parts <- node_list_parts(node_cdr(expr))

  if (is_null(parts)) {
    return(NULL)
  }

  push_goto(node_list_tail_car(parts))
  parts
}

if_parts <- function(expr) {
  branches <- node_cddr(expr)

  if_branch <- node_car(branches)
  if_node <- as_exprs_node(if_branch)
  if_parts <- node_list_parts(if_node)

  else_branch <- node_cadr(branches)
  if (is_null(else_branch)) {
    else_parts <- NULL
  } else {
    else_node <- as_exprs_node(else_branch)
    else_parts <- node_list_parts(else_node)
  }

  if (is_null(if_parts) && is_null(else_parts)) {
    return(NULL)
  }

  parts <- NULL

  # Extract first state and merge it in the if-else expression
  if (!is_null(if_parts)) {
    node_poke_car(branches, as_block(node_car(if_parts)))
    if_parts <- node_cdr(if_parts)
  }
  if (!is_null(else_parts)) {
    node_poke_cadr(branches, as_block(node_car(else_parts)))
    else_parts <- node_cdr(else_parts)
  }

  # Add gotos to continuation states
  if (!is_null(else_parts)) {
    else_parts <- branch_continuation(else_parts, branches)
    parts <- node_list_poke_cdr(else_parts, parts)
  }
  if (!is_null(if_parts)) {
    if_parts <- branch_continuation(if_parts, branches)
    parts <- node_list_poke_cdr(if_parts, parts)
  }

  # Add a goto to the continuation after the if-else block if there
  # are non-exiting branches
  if (!is_exiting_block(expr)) {
    expr <- spliceable_block(expr)
    push_goto(expr)
  }

  node(expr, parts)
}
branch_continuation <- function(parts, branches) {
  tail <- node_list_tail(parts)
  push_goto(node_car(tail))
  parts
}

push_goto <- function(block, goto_node = NULL) {
  if (is_exiting_block(block)) {
    block
  } else {
    node_list_poke_cdr(block, goto_node %||% peek_goto_node())
  }
}

is_pause <- function(x) {
  is_language(x, quote(yield))
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
