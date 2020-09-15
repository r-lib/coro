
vec_last <- function(x) {
  x[[length(x)]]
}
`vec_last<-` <- function(x, value) {
  x[[length(x)]] <- value
  x
}

map_last <- function(.x, .f, ...) {
  vec_last(.x) <- .f(vec_last(.x), ...)
  .x
}

compose <- function(...) {
  fs <- lapply(dots_splice(...), match.fun)
  n <- length(fs)

  last <- fs[[n]]
  rest <- fs[-n]

  function(...) {
    out <- last(...)
    for (fn in rev(rest)) {
      out <- fn(out)
    }
    out
  }
}
negate <- function(.p) {
  function(...) !.p(...)
}

unstructure <- function(x) {
  attributes(x) <- NULL
  x
}

assert_lambda <- function(call) {
  if (!is_call(call, "function")) {
    abort("`fn` must be an anonymous function.")
  }
}
