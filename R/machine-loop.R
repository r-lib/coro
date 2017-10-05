
loop_parts <- function(expr) {
  loop_state <- peek_state()

  body <- as_exprs_node(node_cadr(expr))
  parts <- node_list_parts(body)

  if (is_null(parts)) {
    return(NULL)
  }

  goto_node <- node_list(goto_lang(loop_state))
  tail <- node_list_tail_car(parts)
  push_goto(tail, goto_node)

  parts
}
