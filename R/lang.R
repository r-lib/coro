
is_jump <- function(x) {
  is_lang(x, jump_syms)
}
jump_syms <- list(quote(`break`), quote(`next`))

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

has_shift <- function(expr) {
  lang_has(expr, function(x) is_shift(x) || is_assigned_shift(x))
}
is_shift <- function(expr) {
  is_language(expr, shift_sym)
}
is_assigned_shift <- function(expr) {
  is_language(expr, quote(`<-`)) && is_shift(node_cadr(node_cdr(expr)))
}

lang_check_shift <- function(lang) {
  if (is_assigned_shift(lang)) {
    return(NULL)
  }

  check_shift(node_car(lang))

  args <- node_cdr(lang)
  while (!is_null(args)) {
    car <- node_car(args)
    args <- node_cdr(args)

    check_shift(car)
  }
}
check_shift <- function(expr) {
  if (!is_language(expr)) {
    return(NULL)
  }

  if (is_shift(expr)) {
    abort("Can't shift within a function call")
  }
  lang_check_shift(expr)
}

as_block_lang <- function(x) {
  if (!is_language(x, block_sym)) {
    x <- lang(block_sym, x)
  }
  x
}


bangbang <- function(expr) {
  lang(quote(UQ), expr)
}

ctrl_syms <- list(
  quote(`if`),
  quote(`for`),
  quote(`while`),
  quote(`repeat`)
)
next_sym <- quote(`_next`)
shift_sym <- quote(SHIFT)
assignment_sym <- quote(`<-`)
block_sym <- quote(`{`)
