
lang_has <- function(lang, is_element) {
  if (!is_language(lang)) {
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

is_named_language <- function(x) {
  is_language(x) && is_symbol(node_car(x))
}

as_exprs_node <- function(expr) {
  if (is_pairlist(expr)) {
    expr
  } else if (is_language(expr, quote(`{`))) {
    node_cdr(expr) %||% node_list(NULL)
  } else {
    node(expr, NULL)
  }
}

pause_lang <- function(idx, ...) {
  lang("_pause", as.character(idx), ...)
}
goto_lang <- function(idx) {
  lang("_goto", as.character(idx))
}
return_lang <- function(...) {
  lang("return", ...)
}

block <- function(...) {
  lang(block_sym, ...)
}
new_block <- function(x) {
  new_language(block_sym, x)
}
is_block <- function(x) {
  is_language(x, block_sym)
}
as_block <- function(x) {
  if (is_block(x)) {
    x
  } else {
    block(x)
  }
}

spliceable <- function(x) {
  poke_attr(x, "spliceable", TRUE)
  x
}
is_spliceable <- function(x) {
  is_true(attr(x, "spliceable"))
}

yield_lang <- function(...) {
  lang(yield_sym, ...)
}
if_lang <- function(...) {
  lang(if_sym, ...)
}
repeat_lang <- function(...) {
  lang(repeat_sym, ...)
}
while_lang <- function(...) {
  lang(while_sym, ...)
}
for_lang <- function(...) {
  lang(for_sym, ...)
}

if_branch_true <- function(expr) {
  node_car(node_cddr(expr))
}
if_branch_else <- function(expr) {
  node_cadr(node_cddr(expr))
}

return_sym <- quote(`return`)
pause_sym <- quote(`_pause`)
goto_sym <- quote(`_goto`)
yield_sym <- quote(`yield`)

if_sym <- quote(`if`)
repeat_sym <- quote(`repeat`)
while_sym <- quote(`while`)
for_sym <- quote(`for`)
ctrl_syms <- list(if_sym, repeat_sym, while_sym, for_sym)
assignment_sym <- quote(`<-`)
block_sym <- quote(`{`)
