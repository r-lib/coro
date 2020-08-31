
set_returns <- function(expr) {
  if (is_closure(expr)) {
    expr <- body(expr)
  }
  expr <- duplicate(expr, shallow = TRUE)
  poke_returns(expr)
}

poke_returns <- function(expr) {
  expr <- as_exprs_node(expr)
  tail <- node_list_tail(expr)
  last <- node_car(tail)

  if (!is_call(last) || !is_symbol(node_car(last))) {
    last <- return_state_lang(last)
  } else {
    head <- as_string(node_car(last))
    last <- switch(head,
      `return` = last,
      `{` = new_block(poke_returns(last)),
      `if` = if_poke_returns(last),
      `yield` = ,
      `repeat` = ,
      `while` = ,
      `for` = {
        return_lang <- return_state_lang(call2("invisible", NULL))
        node_poke_cdr(tail, pairlist(return_lang))
        last
      },
      return_lang(last)
    )
  }

  node_poke_car(tail, last)
  expr
}

if_poke_returns <- function(expr) {
  branches <- node_cddr(expr)

  if_branch <- new_block(poke_returns(node_car(branches)))
  node_poke_car(branches, if_branch)

  if (is_null(node_cadr(branches))) {
    explicit_else <- pairlist(block(return_state_lang(call2("invisible", NULL))))
    node_poke_cdr(branches, explicit_else)
  } else {
    else_branch <- new_block(poke_returns(node_cadr(branches)))
    node_poke_cadr(branches, else_branch)
  }

  expr
}
