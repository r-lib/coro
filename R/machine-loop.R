
loop_parts <- function(expr, loop_state = peek_state()) {
  # These pausing nodes are used only when there is no continuation after
  # the pause. It ensures we restart at the start of the loop.
  pauses <- null_node()
  next_node <- node(goto_lang(loop_state), NULL)

  # We don't know the finishing state until we've obtained all states
  # nested within the loop
  break_node <- node(goto_lang(-1L), NULL)

  body <- as_exprs_node(expr)
  with_loop_nodes(pauses, next_node, break_node, {
    parts <- node_list_parts(body)
  })

  if (is_null(parts)) {
    return(NULL)
  }

  # Update the `break` gotos and `pause nodes` to point to the next state
  node_poke_car(break_node, goto_lang(peek_state() + 1L))
  pauses_push_state(pauses, loop_state)

  # Add a looping goto at the end
  goto_node <- node_list(goto_lang(loop_state))
  tail <- node_list_tail_car(parts)
  push_goto(tail, goto_node)

  parts
}

next_parts <- function(expr) {
  next_block <- spliceable(new_block(peek_loop_next_node()))
  node_list(next_block)
}
break_parts <- function(expr) {
  break_block <- spliceable(new_block(peek_loop_break_node()))
  node_list(break_block)
}

repeat_parts <- function(expr) {
  loop_state <- poke_state()

  body <- node_cadr(expr)
  parts <- loop_parts(body)

  if (is_null(parts)) {
    poke_state(loop_state - 1L)
    return(NULL)
  }

  loop_state <- spliceable(block(goto_lang(loop_state)))
  node(loop_state, parts)
}

while_parts <- function(expr) {
  if (peek_has_past()) {
    poke_state()
    n_poke <- 2L
  } else {
    n_poke <- 1L
  }
  loop_state <- peek_state()
  poke_state()

  body <- node_car(node_cddr(expr))
  parts <- loop_parts(body, loop_state)

  if (is_null(parts)) {
    poke_state(loop_state - n_poke)
    return(NULL)
  }

  goto_loop_end <- block(goto_lang(peek_state() + 1L))
  goto_loop_start <- block(goto_lang(loop_state + 1L))

  cond <- node_cadr(expr)
  cond_state <- block(if_lang(cond, goto_loop_start, goto_loop_end))
  loop_parts <- node(cond_state, parts)

  # Merge into the current state if there is a past
  if (peek_has_past()) {
    goto_block <- spliceable(block(goto_lang(loop_state)))
    loop_parts <- node(goto_block, loop_parts)
  }

  loop_parts
}

for_parts <- function(expr) {
  poke_state()
  loop_state <- peek_state()
  poke_state()

  body <- node_cadr(node_cddr(expr))
  parts <- loop_parts(body, loop_state)

  if (is_null(parts)) {
    poke_state(loop_state - 1L)
    return(NULL)
  }

  init_part <- for_init_part(loop_state, expr)
  next_part <- for_next_part(loop_state, expr)

  init_parts <- set_attrs(init_part, spliceable = TRUE) # FIXME
  node(init_parts, node(next_part, parts))
}

for_init_part <- function(loop_state, expr) {
  idx_loop_sym <- for_idx_sym(loop_state)
  vec_loop_sym <- for_vec_sym(loop_state)
  vec_user_expr <- node_cadr(node_cdr(expr))

  expr({
    !! idx_loop_sym <- 0L
    !! vec_loop_sym <- !! vec_user_expr

    # `for` internally converts factors to character vectors
    if (base::is.factor(!! vec_loop_sym)) {
      !! vec_loop_sym <- base::as.character(!! vec_loop_sym)
    }

    !! goto_lang(loop_state)
  })
}
for_next_part <- function(loop_state, expr) {
  idx_loop_sym <- for_idx_sym(loop_state)
  idx_user_sym <- node_cadr(expr)
  vec_loop_sym <- for_vec_sym(loop_state)

  expr({
    !! idx_loop_sym <- UQ(idx_loop_sym) + 1L
    !! idx_user_sym <- UQ(vec_loop_sym)[[!! idx_loop_sym]]
    !! goto_lang(loop_state + 1L)
  })
}

utils::globalVariables("!<-")
