
loop_parts <- function(expr, loop_state = peek_state()) {
  # These pausing nodes are used only when there is no continuation after
  # the pause. It ensures we restart at the start of the loop.
  pauses <- null_node()
  next_node <- new_node(goto_call(loop_state), NULL)

  # We don't know the finishing state until we've obtained all states
  # nested within the loop
  break_node <- new_node(goto_call(-1L), NULL)

  body <- as_exprs_node(expr)
  with_loop_nodes(pauses, next_node, break_node, {
    parts <- node_list_parts(body)
  })

  # Update the `break` gotos and `pause nodes` to point to the next state
  node_poke_car(break_node, goto_call(peek_state() + 1L))
  pauses_push_state(pauses, loop_state)

  # Add a looping goto at the end
  goto_node <- pairlist(goto_call(loop_state))
  tail <- node_list_tail_car(parts)
  push_goto(tail, goto_node)

  parts
}

next_parts <- function(expr) {
  next_block <- spliceable(new_block(peek_loop_next_node()))
  parts <- pairlist(next_block)
  poke_attr(parts, "loop_control", TRUE)
}
break_parts <- function(expr) {
  break_block <- spliceable(new_block(peek_loop_break_node()))
  parts <- pairlist(break_block)
  poke_attr(parts, "loop_control", TRUE)
}

is_loop_control <- function(x) {
  is_true(attr(x, "loop_control"))
}
all_loop_control <- function(node) {
  if (is_null(node)) {
    return(FALSE)
  }

  while (!is_null(node)) {
    if (!is_loop_control(node)) {
      return(FALSE)
    }
    node <- node_cdr(node)
  }

  TRUE
}

repeat_parts <- function(expr) {
  loop_state <- poke_state()

  body <- node_cadr(expr)
  parts <- loop_parts(body)

  if (is_null(parts)) {
    poke_state(loop_state - 1L)
    return(NULL)
  }

  loop_state <- spliceable(block(goto_call(loop_state)))
  new_node(loop_state, parts)
}

while_parts <- function(expr) {
  old_state <- peek_state()

  if (peek_has_past()) {
    poke_state()
  }
  loop_state <- peek_state()
  poke_state()

  body <- node_car(node_cddr(expr))
  parts <- loop_parts(body, loop_state)

  if (is_null(parts)) {
    poke_state(old_state)
    return(NULL)
  }

  goto_loop_end <- block(goto_call(peek_state() + 1L))
  goto_loop_start <- block(goto_call(loop_state + 1L))

  cond <- node_cadr(expr)
  cond_state <- block(if_call(cond, goto_loop_start, goto_loop_end))
  loop_parts <- new_node(cond_state, parts)

  # Merge into the current state if there is a past
  if (peek_has_past()) {
    goto_block <- spliceable(block(goto_call(loop_state)))
    loop_parts <- new_node(goto_block, loop_parts)
  }

  loop_parts
}

for_parts <- function(expr) {
  poke_state()
  loop_state <- peek_state()
  poke_state()

  body <- node_cadr(node_cddr(expr))
  parts <- loop_parts(body, loop_state)

  # We always fully translate a `for` loop in order to support flowery
  # iterators for convenience
  if (is_null(parts)) {
    body <- duplicate(body, shallow = TRUE)
    body <- as_block(body)
    node_list_poke_cdr(body, pairlist(goto_call(loop_state)))
    parts <- pairlist(body)
  }

  init_part <- for_init_part(loop_state, expr)
  next_part <- for_next_part(loop_state, expr)

  init_parts <- structure(init_part, spliceable = TRUE) # FIXME
  new_node(init_parts, new_node(next_part, parts))
}

for_init_part <- function(loop_state, expr) {
  iter_sym <- for_iter_sym(loop_state)
  coll_expr <- node_cadr(node_cdr(expr))

  expr({
    !!iter_sym <- !!coll_expr

    # `base::for()` internally converts factors to character vectors
    if (base::is.factor(!!iter_sym)) {
      !!iter_sym <- base::as.character(!!iter_sym)
    }
    !!iter_sym <- flowery::as_iterator(!!iter_sym)

    # Trigger an iteration error if iterator was already done
    if (flowery::is_done(!! iter_sym)) {
      UQ(iter_sym)()
    }

    !!goto_call(loop_state)
  })
}
for_next_part <- function(loop_state, expr) {
  elt_sym <- node_cadr(expr)
  iter_sym <- for_iter_sym(loop_state)

  expr({
    if (flowery::advance(!!iter_sym)) {
      !!elt_sym <- flowery::deref(!!iter_sym)
      !!goto_call(loop_state + 1L)
    } else {
      !!goto_call(peek_state() + 1L)
    }
  })
}

utils::globalVariables("!<-")
