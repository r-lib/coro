
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

negate <- function(.p) {
  function(...) !.p(...)
}

assert_lambda <- function(call) {
  if (!is_call(call, "function")) {
    abort("`fn` must be an anonymous function.")
  }
}

stop_internal <- function(fn, msg) {
  abort(sprintf("Internal error in `%s()`: %s.", fn, msg))
}

stop_unimplemented <- function(what) {
  abort(c(
    sprintf("%s is not implemented yet.", what),
    i = "Please send a feature request if you are interested."
  ))
}

try_catch_arg <- function(call) {
  i <- match("expr", names(call))
  if (!is_na(i)) {
    return(i)
  }

  i <- which(!have_name(call[-1]))
  if (length(i)) {
    return(i[[1]] + 1)
  }

  abort("Can't supply empty `tryCatch()`.")
}

without_call_errors <- function(expr, env = caller_env()) {
  withCallingHandlers(expr, simpleError = function(cnd) {
    cnd$call <- NULL
    stop(cnd)
  })
}

env_exits <- function(env) {
  exit <- eval_bare(call2(sys.on.exit), env)

  if (is_null(exit)) {
    return(NULL)
  }
  if (is_call(exit, "{")) {
    return(node_cdr(exit))
  }
  new_node(exit)
}
env_poke_exits <- function(env, exprs) {
  old <- env_exits(env)

  if (length(exprs)) {
    arg <- block(!!!exprs)
  } else {
    arg <- NULL
  }
  eval_bare(call2(on.exit, arg), env)

  invisible(old)
}

zap_env <- function(x) {
  environment(x) <- global_env()
  x
}

# Safe version of `<<-` that never assigns in the global
# environment. It fails with an error if `lhs` does not exist.
`<<-` <- function(lhs, value) {
  env <- caller_env()
  lhs <- as_string(substitute(lhs))
  env_poke(env, lhs, value, inherit = TRUE, create = FALSE)
  invisible(value)
}
