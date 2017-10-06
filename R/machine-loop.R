
loop_parts <- function(expr) {
  loop_state <- peek_state()

  # This pausing node is used only when there is no continuation after
  # the pause. It ensures we restart at the start of the loop.
  pause_node <- node(pause_lang(loop_state), NULL)
  next_node <- node(goto_lang(loop_state), NULL)

  # We don't know the finishing state until we've obtained all states
  # nested within the loop
  break_node <- node(goto_lang(-1L), NULL)

  body <- as_exprs_node(expr)
  with_loop_nodes(pause_node, next_node, break_node, {
    parts <- node_list_parts(body)
  })

  if (is_null(parts)) {
    return(NULL)
  }

  # Update the `break` gotos to point to the next state
  node_poke_car(break_node, goto_lang(peek_state() + 1L))

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
  parts <- loop_parts(node_cadr(expr))

  if (is_null(parts)) {
    poke_state(loop_state - 1L)
    return(NULL)
  }

  loop_block <- spliceable(block(goto_lang(loop_state)))
  node(loop_block, parts)
}

while_parts <- function(expr) {
  loop_state <- poke_state()

  cond <- node_cadr(expr)
  loop <- node_car(node_cddr(expr))

  loop_parts <- loop_parts(loop)

  if (is_null(loop_parts)) {
    poke_state(loop_state - 1L)
    return(NULL)
  }

  goto_loop_end <- goto_lang(poke_state())
  cond_lang <- if_lang(cond, block(goto_lang(loop_state)), block(goto_loop_end))
  cond_state <- spliceable(block(cond_lang))

  node(cond_state, loop_parts)
}
