
unstructure <- function(x) {
  attributes(x) <- NULL
  x
}

blast <- function(expr, env = caller_env()) {
  eval_bare(enexpr(expr), env)
}

`%&&%` <- function(x, y) {
  if (is_null(x)) {
    x
  } else {
    y
  }
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

assert_lambda <- function(call) {
  if (!is_call(call, "function")) {
    abort("`fn` must be an anonymous function.")
  }
}

stop_internal <- function(fn, msg) {
  abort(sprintf("Internal error in `%s()`: %s.", fn, msg))
}
