
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
    push_goto(node_list_tail_car(else_parts))
    parts <- node_list_poke_cdr(else_parts, parts)
  }
  if (!is_null(if_parts)) {
    push_goto(node_list_tail_car(if_parts))
    parts <- node_list_poke_cdr(if_parts, parts)
  }

  # Add a goto to the continuation after the if-else block if there
  # are non-exiting branches
  if (!is_exiting_block(expr)) {
    expr <- spliceable(block(expr))
    push_goto(expr)
  }

  new_node(expr, parts)
}
