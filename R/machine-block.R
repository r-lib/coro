
block_refs <- function(block) {
  node_cdr(as.pairlist(attr(block, "srcref")))
}

block_parts <- function(expr) {
  refs <- block_refs(expr)
  parts <- node_list_parts(node_cdr(expr), refs = refs)

  if (is_null(parts)) {
    NULL
  } else {
    block_push_goto(node_list_tail_car(parts))
    parts
  }
}

node_list_parts <- function(node, refs = NULL) {
  rest <- node
  rest_refs <- refs
  parts <- NULL
  parent <- NULL

  has_future <- function() {
    !is_null(node_cdr(rest))
  }
  has_past <- function() {
    !is_null(parent)
  }

  arg_sym <- peek_state_elt("arg_sym")

  # First assign initial argument in the machine state environment
  if (!is_null(arg_sym)) {
    state <- poke_state()
    block <- block(
      expr(!!arg_sym <- `_next_arg`),
      goto_call(state)
    )
    parts <- new_node(block)
  }

  while (!is_null(rest)) {
    expr <- node_car(rest)

    if (is_call(expr, "<-") && is_pause(rhs <- call_rhs(expr))) {
      # Splice the yield() assignment in the continuation
      assign_call <- call("<-", call_lhs(expr), quote(`_next_arg`))
      node_poke_cdr(rest, new_node(assign_call, node_cdr(rest)))

      # Continue with a normal pause
      expr <- rhs
    }

    if (is_pause(expr)) {
      # If pause has no future we don't know which state it should
      # resume to. This is not always `peek_state() + 1` because there
      # might be intervening states, e.g. with `if (foo) yield(1) else
      # yield(2)`. To work around this we register the pause node so
      # the state can be adjusted later.
      if (has_future()) {
        pause_call <- new_pause(poke_state(), node_cdr(expr))
        pause_node <- pairlist(pause_call)
      } else {
        pause_call <- new_pause(-1, node_cdr(expr))
        pause_node <- pairlist(pause_call)
        push_pause_node(pause_node)
      }

      if (has_past()) {
        node_poke_cdr(parent, pause_node)
      } else {
        node <- pause_node
      }

      pause_block <- new_user_block(node, refs)
      parts <- node_list_poke_cdr(parts, pairlist(pause_block))

      rest <- node <- node_cdr(rest)
      rest_refs <- refs <- node_cdr(rest_refs)
      parent <- NULL
      next
    }

    with_jump_nodes(has_past(), has_future(), {
      nested_parts <- expr_parts(expr, rest_refs)
    })

    if (is_null(nested_parts)) {
      parent <- rest
      rest <- node_cdr(rest)
      rest_refs <- node_cdr(rest_refs)
      next
    }

    # If we found nested states, check if there are any past
    # expressions to add them before the pausing block. `nested_parts`
    # always starts with a pausing expression.
    pausing_part <- node_car(nested_parts)

    if (has_past()) {
      if (is_spliceable(pausing_part)) {
        pausing_part <- node_cdr(pausing_part)
        node_poke_cdr(parent, pausing_part)
        block <- new_user_block(node, refs)
      } else {
        node_poke_cdr(parent, NULL)
        block <- new_user_block(node, refs)
        node_list_poke_cdr(block, node_cdr(pausing_part))
      }

      node_poke_car(nested_parts, block)
    } else {
      poke_attr(pausing_part, "spliceable", NULL)
    }

    # Merge nested states
    parts <- node_list_poke_cdr(parts, nested_parts)

    rest <- node <- node_cdr(rest)
    rest_refs <- refs <- node_cdr(rest_refs)
    parent <- NULL
    next
  }

  if (is_null(parts)) {
    return(NULL)
  }

  # `node` may be NULL if there is no expression after a pause
  if (!is_null(node)) {
    remaining <- new_user_block(node, refs)
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

    `_block` = ,
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
  quote(coro_goto),
  quote(`next`),
  quote(`break`),
  quote(yield)
)

utils::globalVariables("_next_arg")
