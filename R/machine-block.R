
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

    if (is_call(expr, "<-") && is_pause(rhs <- call_rhs(expr))) {
      # Signal constructor that this generator is a coroutine that
      # takes a `_next` argument
      poke_state_elt("coroutine", TRUE)

      # Splice the yield() assignment in the continuation
      assign_call <- call("<-", call_lhs(expr), next_value_sym)
      node_poke_cdr(rest, new_node(assign_call, node_cdr(rest)))

      # Continue with a normal pause
      expr <- rhs
    }

    if (is_pause(expr)) {
      # If pause has no future we don't know which state it should
      # resume to. We register it so the state can be adjusted later.
      if (has_future()) {
        pause_call <- new_pause(poke_state(), node_cdr(expr))
        pause_node <- pairlist(pause_call)
      } else {
        pause_call <- new_pause(peek_state(), node_cdr(expr))
        pause_node <- pairlist(pause_call)
        push_pause_node(pause_node)
      }

      if (has_past()) {
        node_poke_cdr(parent, pause_node)
        pause_block <- new_block(node)
      } else {
        pause_block <- new_block(pause_node)
      }
      parts <- node_list_poke_cdr(parts, pairlist(pause_block))

      rest <- node <- node_cdr(rest)
      parent <- NULL
      next
    }

    # Extract nested states. If there is a continuation, pass on the
    # relevant goto and pause nodes. Fill those nodes only when we
    # extracted the parts so they get the right state index.
    if (has_future()) {
      next_goto <- new_node(goto_call(-1L), NULL)
      pauses <- null_node()

      with_jump_nodes(next_goto, pauses, has_past(), {
        nested_parts <- expr_parts(expr)
      })

      if (!is_null(nested_parts)) {
        # Empty blocks occur when a translator returns a separate
        # state that shouldn't be appended to the current past.
        # In this case, poke state one more time.
        state <- poke_state()
        if (is_separate_state(nested_parts)) {
          state <- poke_state()
        }
        node_poke_car(next_goto, goto_call(state))
        pauses_push_state(pauses, state)
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
        pausing_part <- pairlist(pausing_part)
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
    node_list_poke_cdr(parts, pairlist(remaining))
  }

  parts
}

is_exiting_block <- function(x) {
  if (!is_call(x)) {
    return(FALSE)
  }

  if (is_call(x, exiting_syms)) {
    return(TRUE)
  }

  car <- node_car(x)
  if (!is_symbol(car)) {
    return(FALSE)
  }

  switch(as_string(car),
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

    FALSE
  )
}
exiting_syms <- list(
  return_sym,
  quote(coro_return),
  quote(coro_yield),
  quote(coro_goto)
)
