
walk_blocks <- function(expr, fn) {
  walk_blocks_switch(expr, cflow_type(expr), fn)
}

walk_blocks_switch <- function(expr, type, fn) {
  if (is_null(type)) {
    return(expr)
  }

  expr <- duplicate(expr, shallow = TRUE)

  if (is_string(type, "{")) {
    node <- node_cdr(expr)

    while (!is_null(node)) {
      car <- node_car(node)
      type <- cflow_type(car)

      if (is_null(type)) {
        # Apply function and end recursion
        fn(node)
      } else {
        node_poke_car(node, walk_blocks_switch(car, type, fn))
      }

      node <- node_cdr(node)
    }

    return(expr)
  }

  if (is_string(type, "if")) {
    cddr <- node_cddr(expr)

    node_poke_car(cddr, walk_blocks(node_car(cddr), fn))

    else_expr <- node_cadr(cddr)
    if (!is_null(else_expr)) {
      node_poke_cadr(cddr, walk_blocks(else_expr, fn))
    }

    return(expr)
  }

  switch(type,
    `repeat` = node_poke_cadr(expr, walk_blocks(node_cadr(expr), fn)),
    `while` = node_poke_car(node_cddr(expr), walk_blocks(node_car(node_cddr(expr)), fn)),
    `for` = node_poke_cadr(node_cddr(expr), walk_blocks(node_cadr(node_cddr(expr)), fn)),
    abort("Unexpected state in `walk_blocks_switch()`.")
  )

  expr
}

cflow_type <- function(expr) {
  if (!is_call(expr)) {
    return(NULL)
  }

  car <- node_car(expr)
  if (!is_symbol(car)) {
    return(NULL)
  }

  fn <- as_string(car)
  if (fn %in% c("{", "if", "repeat", "while", "for")) {
    fn
  } else {
    NULL
  }
}
