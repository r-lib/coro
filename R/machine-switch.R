
machine_switch_call <- function(parts) {
  parts <- node_list_enumerate_tags(parts)

  last <- node_list_tail(parts)
  catch_all_state <- pairlist(block(quote(rlang::abort(
    base::sprintf("Internal error: Unexpected state `%s`.", `_state`)
  ))))
  node_poke_cdr(last, catch_all_state)

  switch_args <- new_node(block(quote(base::as.character(`_state`))), parts)

  new_call(switch_sym, switch_args)
}
node_list_enumerate_tags <- function(node) {
  i <- 0L
  rest <- node

  while (!is_null(rest)) {
    i <- i + 1L
    node_poke_tag(rest, sym(as.character(i)))
    rest <- node_cdr(rest)
  }

  invisible(node)
}

#' Control flow for coroutine state machines
#'
#' @keywords internal
#' @export
coro_goto <- function(state, frame = caller_env()) {
  env <- env_get(frame, "_machine_state_env", default = frame)
  env$`_state` <- state
  eval_bare(call2(next_sym), env)
}
#' @rdname coro_goto
#' @export
coro_yield <- function(state, value = NULL, frame = caller_env()) {
  if (is_null(value)) {
    abort("Can't yield `NULL`.")
  }
  frame$`_state` <- state
  eval_bare(call2(base::return, value), frame)
}
#' @rdname coro_goto
#' @export
coro_return <- function(value, frame = caller_env()) {
  env <- env_get(frame, "_machine_state_env", default = frame)
  # Goto NULL-return state to terminate iterator
  env$`_state` <- env_get(env, "_return_state")
  eval_bare(call2(base::return, value), env)
}

is_coro_return_call <- function(x) {
  is_call(x, "coro_return", ns = "flowery")
}
is_coro_yield_call <- function(x) {
  is_call(x, "coro_yield", ns = "flowery")
}


state_machine_variables <- c("_state", "_self", "_as_promise", "_then")

flowery_declare_globals <- function(env = caller_env()) {
  evalq(utils::globalVariables(state_machine_variables), env)
}
flowery_declare_globals()
