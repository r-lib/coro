
`%<~%` <- function(lhs, rhs, env = caller_env()) {
  env_bind_lazy(env, !!substitute(lhs) := !!substitute(rhs), .eval_env = env)
}

on_load <- function(expr, env = topenv(caller_env())) {
  callback <- function() eval_bare(expr, env)

  hook <- env$.__rlang_hook__.
  env$.__rlang_hook__. <- list2(!!!hook, callback)
}

run_on_load <- function(env = topenv(caller_env())) {
  hook <- env$.__rlang_hook__.
  env_unbind(env, ".__rlang_hook__.")

  for (callback in hook) {
    callback()
  }

  NULL
}

.onLoad <- function(...) {
  run_on_load()
}
