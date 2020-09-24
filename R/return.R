
set_returns <- function(expr) {
  if (is_closure(expr)) {
    expr <- body(expr)
  }

  expr <- as_block(expr)

  # FIXME: Excessive cloning
  expr <- node_clone(expr)

  handle <- pairlist(expr)
  poke_returns(handle)

  node_car(handle)
}

poke_returns <- function(node) {
  if (is_null(node)) {
    return(NULL)
  }

  # First change all explicit `return()` calls in the whole AST
  swap_returns(node)

  # Add an explicit return call in the last expression if there isn't one already
  tail <- node_list_tail(node)
  last <- node_car(tail)

  if (is_coro_return_call(last)) {
    return()
  }

  if (!is_call(last) || !is_symbol(node_car(last))) {
    fn <- ""
  } else {
    fn <- as_string(node_car(last))
  }

  switch(fn,
    `{` = {
      if (is_null(node_cdr(last))) {
        node_poke_cdr(last, pairlist(invisible_return_call()))
      } else {
        poke_returns(node_cdr(last))
      }
    },
    `if` = if_poke_returns(tail),
    `yield` = ,
    `repeat` = ,
    `while` = ,
    `for` = {
      node_poke_cdr(tail, pairlist(invisible_return_call()))
    },
    node_poke_car(tail, return_state_call(last))
  )

  NULL
}

invisible_return_call <- function() {
  return_state_call(quote(invisible(NULL)))
}

swap_returns <- function(node) {
  while (!is_null(node)) {
    car <- node_car(node)

    if (!is_call(car)) {
      node <- node_cdr(node)
      next
    }

    if (identical(node_car(car), return_sym)) {
      car <- return_state_call(node_cadr(car))
      node_poke_car(node, car)
    } else {
      swap_returns(car)
    }

    node <- node_cdr(node)
  }

  NULL
}

if_poke_returns <- function(node) {
  expr <- node_car(node)
  then_node <- node_cddr(expr)
  else_node <- node_cdr(then_node)

  then_expr <- as_block(node_car(then_node))
  node_poke_car(then_node, then_expr)
  poke_returns(node_cdr(then_expr))

  if (is_null(else_node)) {
    explicit_else <- pairlist(block(return_state_call(call2("invisible", NULL))))
    node_poke_cdr(then_node, explicit_else)
  } else {
    else_expr <- as_block(node_car(else_node))
    node_poke_car(else_node, else_expr)
    poke_returns(node_cdr(else_expr))
  }

  NULL
}
