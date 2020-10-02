
lang_has <- function(lang, is_element) {
  if (!is_call(lang)) {
    return(FALSE)
  }
  if (is_element(lang)) {
    return(TRUE)
  }

  rest <- lang
  while (!is_null(rest)) {
    if (lang_has(node_car(rest), is_element)) {
      return(TRUE)
    }
    rest <- node_cdr(rest)
  }

  FALSE
}

as_exprs_node <- function(expr) {
  if (is_pairlist(expr)) {
    expr
  } else if (is_call(expr, quote(`{`))) {
    node_cdr(expr) %||% pairlist(NULL)
  } else {
    new_node(expr, NULL)
  }
}

pause_call <- function(idx, ...) {
  call2(quote(flowery::coro_yield), as.character(idx), ...)
}
goto_call <- function(idx) {
  call2(quote(flowery::coro_goto), as.character(idx))
}
return_call <- function(...) {
  call2(return_sym, ...)
}
return_state_call <- function(...) {
  call2(quote(flowery::coro_return), ...)
}

new_pause <- function(state, cdr = NULL) {
  args <- new_node(as.character(state), cdr)
  new_call(quote(flowery::coro_yield), args)
}

block <- function(...) {
  call2(block_sym, ...)
}
new_block <- function(x) {
  new_call(block_sym, x)
}
is_block <- function(x) {
  is_call(x, block_sym)
}
as_block <- function(x) {
  if (is_block(x)) {
    x
  } else {
    block(x)
  }
}

user_block <- function(expr) {
  call("user", expr)
}

new_user_block <- function(x, refs = NULL) {
  block <- new_call(block_sym, x)

  if (!is_null(refs)) {
    # Implicit coercion from pairlist to list
    refs <- refs[seq_along(x)]

    # Add empty reference for `{`
    refs <- c(list(NULL), refs)

    attr(block, "srcref") <- refs
  }

  call("{", call("_block", block))
}

spliceable <- function(x) {
  poke_attr(x, "spliceable", TRUE)
  x
}
is_spliceable <- function(x) {
  is_true(attr(x, "spliceable"))
}

yield_call <- function(...) {
  call2(yield_sym, ...)
}
if_call <- function(...) {
  call2(if_sym, ...)
}
repeat_call <- function(...) {
  call2(repeat_sym, ...)
}
while_call <- function(...) {
  call2(while_sym, ...)
}
for_call <- function(...) {
  call2(for_sym, ...)
}
break_call <- function() {
  call2(break_sym)
}
next_call <- function() {
  call2(next_sym)
}

if_branch_true <- function(expr) {
  node_car(node_cddr(expr))
}
if_branch_else <- function(expr) {
  node_cadr(node_cddr(expr))
}

for_iter_sym <- function(i) {
  sym(paste0("_for_iter_", i))
}

call_lhs <- function(call) {
  node_cadr(call)
}
call_rhs <- function(call) {
  node_cadr(node_cdr(call))
}

call_repeat_body <- function(x) {
  node_cadr(x)
}
call_repeat_poke_body <- function(x, body) {
  node_poke_cadr(x, body)
}

call_while_body <- function(x) {
  node_cadr(node_cdr(x))
}
call_while_poke_body <- function(x, body) {
  node_poke_cadr(node_cdr(x), body)
}

call_for_body <- function(x) {
  node_cadr(node_cddr(x))
}
call_for_poke_body <- function(x, body) {
  node_poke_cadr(node_cddr(x), body)
}

is_yield_call <- function(x) {
  is_call(x, "yield", ns = c("", "flowery"))
}

return_sym <- quote(`return`)
yield_sym <- quote(`yield`)

return_state_sym <- quote(`return`)
goto_sym <- quote(`_goto`)

if_sym <- quote(`if`)
repeat_sym <- quote(`repeat`)
while_sym <- quote(`while`)
for_sym <- quote(`for`)
ctrl_syms <- list(if_sym, repeat_sym, while_sym, for_sym)
assignment_sym <- quote(`<-`)
block_sym <- quote(`{`)
switch_sym <- quote(`switch`)

break_sym <- quote(`break`)
next_sym <- quote(`next`)
loop_ctrl_syms <- list(break_sym, next_sym)
