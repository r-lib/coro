
walk_blocks <- function(node, fn, which = "expr") {
  stopifnot(is_pairlist(node))

  while (!is_null(node)) {
    walk_blocks_expr(node, fn, which = which)
    node <- node_cdr(node)
  }

  NULL
}

walk_blocks_expr <- function(node, fn, which) {
  expr <- node_car(node)
  type <- cflow_type(expr)

  if (is_null(type)) {
    if ("expr" %in%  which) {
      fn(node)
    }
    return()
  }

  # Only shallow-duplicate when we descend one level in the tree,
  # i.e. when we take the CAR
  expr <- duplicate(expr, shallow = TRUE)
  node_poke_car(node, expr)

  if (type %in% which) {
    fn(node, type = type)
    return()
  }

  recurse <- function(node) walk_blocks(node, fn = fn, which = which)

  switch(type,
    `{` = recurse(node_cdr(expr)),
    `if` = recurse(node_cdr(expr)),
    `repeat` = recurse(node_cdr(expr)),
    `while` = recurse(node_cddr(expr)),
    `for` = recurse(node_cdr(node_cddr(expr))),
    abort("Unexpected state in `walk_blocks_switch()`.")
  )

  NULL
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
