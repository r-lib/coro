
loop_parts <- function(expr) {
  loop_state <- poke_state()
  pause_node <- node(pause_lang(loop_state), NULL)

  body <- as_exprs_node(node_cadr(expr))
  with_pause_node(pause_node, {
    parts <- node_list_parts(body)
  })

  if (is_null(parts)) {
    return(NULL)
  }

  goto_node <- node_list(goto_lang(loop_state))
  tail <- node_list_tail_car(parts)
  push_goto(tail, goto_node)

  loop_block <- goto_lang(loop_state)
  node(loop_block, parts)
}
