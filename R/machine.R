
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

      # If any past expressions add them to pausing block
      if (!is_null(parent)) {
        pausing_part <- node_car(expr_parts)

        # Only add a goto if there might be code reaching the
        # continuation from a non-yielding branch
        if (is_language(pausing_part, if_sym)) {
          pausing_part <- node_list(pausing_part)
          is_exiting_block(pausing_part)
          push_goto(pausing_part, peek_state())
        } else {
          pausing_part <- node_list(pausing_part)
        }

        # Merge past expressions in pausing block
        node_poke_cdr(parent, pausing_part)
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

  push_goto(node_list_tail_car(parts), poke_state())
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
  state <- poke_state()

  if (!is_null(else_parts)) {
    else_parts <- if_branch_parts(else_parts, branches, state)
    parts <- node_list_poke_cdr(else_parts, parts)
  }
  if (!is_null(if_parts)) {
    if_parts <- if_branch_parts(if_parts, branches, state)
    parts <- node_list_poke_cdr(if_parts, parts)
  }

  node(expr, parts)
}
if_branch_parts <- function(parts, branches, state) {
  # The first state is merged in an actual if-else expression
  branch <- node_car(parts)
  node_poke_car(branches, branch)

  # Discard that first state since it'll be handled elsewhere
  parts <- node_cdr(parts)

  tail <- node_list_tail(parts)
  push_goto(node_car(tail), state)

  parts
}

push_goto <- function(block, state) {
  if (!is_exiting_block(block)) {
    goto_node <- node_list(goto_lang(state))
    node_list_poke_cdr(block, goto_node)
  }

  block
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
