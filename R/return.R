
set_returns <- function(expr) {
  if (is_closure(expr)) {
    expr <- body(expr)
  }
  node <- as_exprs_node(expr)
  node <- node_clone(node)
  poke_returns(node)
}

poke_returns <- function(expr) {
  node <- as_exprs_node(expr)
  node <- swap_returns(node)
  walk_poke_next(node)

  tail <- node_list_tail(node)
  last <- node_car(tail)

  if (is_coro_return_call(last)) {
    last <- last
  } else if (!is_call(last) || !is_symbol(node_car(last))) {
    last <- return_state_call(last)
  } else {
    head <- as_string(node_car(last))
    last <- switch(head,
      `return` = return_state_call(node_cadr(last)),
      `{` = new_block(poke_returns(last)),
      `if` = if_poke_returns(last),
      `yield` = ,
      `repeat` = ,
      `while` = ,
      `for` = {
        return_call <- return_state_call(quote(invisible(NULL)))
        node_poke_cdr(tail, pairlist(return_call))
        last
      },
      return_state_call(last)
    )
  }

  node_poke_car(tail, last)
  node
}

swap_returns <- function(node) {
  out <- node

  while (!is_null(node)) {
    car <- node_car(node)

    if (!is_call(car)) {
      node <- node_cdr(node)
      next
    }

    if (identical(node_car(car), return_sym)) {
      car <- return_state_call(node_cadr(car))
    } else {
      car <- swap_returns(car)
    }
    node_poke_car(node, car)

    node <- node_cdr(node)
  }

  out
}


if_poke_returns <- function(expr) {
  branches <- node_cddr(expr)

  if_branch <- new_block(poke_returns(node_car(branches)))
  node_poke_car(branches, if_branch)

  if (is_null(node_cadr(branches))) {
    explicit_else <- pairlist(block(return_state_call(call2("invisible", NULL))))
    node_poke_cdr(branches, explicit_else)
  } else {
    else_branch <- new_block(poke_returns(node_cadr(branches)))
    node_poke_cadr(branches, else_branch)
  }

  expr
}

walk_poke_next <- function(node) {
  walk_blocks(node, poke_next, which = c("repeat", "while", "for"))
}

poke_next <- function(node, type) {
  expr <- node_car(node)

  body <- switch(type,
    `repeat` = call_repeat_body(expr),
    `while` = call_while_body(expr),
    `for` = call_for_body(expr),
    stop_internal("poke_next")
  )

  body <- as_block(body)
  tail <- node_list_tail(body)
  if (!identical(node_car(tail), quote(next))) {
    node_poke_cdr(tail, pairlist(quote(next)))
  }

  switch(type,
    `repeat` = call_repeat_poke_body(expr, body),
    `while` = call_while_poke_body(expr, body),
    `for` = call_for_poke_body(expr, body),
    stop_internal("poke_next")
  )

  NULL
}
