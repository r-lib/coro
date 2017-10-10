
machine_parts <- function(fn) {
  reset_state()

  node <- set_returns(fn)
  node_list_parts(node)
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
    `repeat` = repeat_parts(expr),
    `while` = while_parts(expr),
    `for` = for_parts(expr),
    `break` = break_parts(expr),
    `next` = next_parts(expr),
    NULL
  )
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
