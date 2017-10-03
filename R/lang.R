
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

as_exprs_node <- function(expr) {
  if (is_pairlist(expr)) {
    expr
  } else if (is_language(expr, quote(`{`))) {
    node_cdr(expr)
  } else {
    node(expr, NULL)
  }
}

block <- function(...) {
  lang(block_sym, ...)
}
new_block <- function(x) {
  new_language(block_sym, x)
}

ctrl_syms <- list(
  quote(`if`),
  quote(`for`),
  quote(`while`),
  quote(`repeat`)
)
assignment_sym <- quote(`<-`)
block_sym <- quote(`{`)
