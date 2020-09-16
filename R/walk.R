
walk_blocks <- function(expr, fn, which = "arg") {
  walk_blocks_switch(expr, cflow_type(expr), fn, which = which)
}

walk_blocks_switch <- function(expr, type, fn, which) {
  recurse_switch <- function(expr, type) walk_blocks_switch(expr, type, fn = fn, which = which)
  recurse <- function(expr) walk_blocks_switch(expr, cflow_type(expr), fn = fn, which = which)

  if (is_null(type)) {
    return(expr)
  }

  if (type %in% which) {
    return(fn(expr, type = type))
  }

  expr <- duplicate(expr, shallow = TRUE)

  if (is_string(type, "{")) {
    node <- node_cdr(expr)

    while (!is_null(node)) {
      car <- node_car(node)
      type <- cflow_type(car)

      if (!is_null(type)) {
        node_poke_car(node, recurse_switch(car, type))
      } else if ("arg" %in%  which) {
        fn(node)
      }

      node <- node_cdr(node)
    }

    return(expr)
  }

  if (is_string(type, "if")) {
    cddr <- node_cddr(expr)

    node_poke_car(cddr, recurse(as_block(node_car(cddr))))

    else_expr <- node_cadr(cddr)
    if (!is_null(else_expr)) {
      node_poke_cadr(cddr, recurse(as_block(else_expr)))
    }

    return(expr)
  }

  switch(type,
    `repeat` = node_poke_cadr(expr, recurse(as_block(node_cadr(expr)))),
    `while` = node_poke_car(node_cddr(expr), recurse(as_block(node_car(node_cddr(expr))))),
    `for` = node_poke_cadr(node_cddr(expr), recurse(as_block(node_cadr(node_cddr(expr))))),
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
