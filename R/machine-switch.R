
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
  frame$`_state` <- state
  eval_bare(call2(next_sym), frame)
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
  # Goto NULL-return state to terminate iterator
  frame$`_state` <- env_get(frame, "_return_state")
  eval_bare(call2(base::return, value), frame)
}
#' @export
coro_await <- function(x, callback) {
  promises::then(as_promise(x), onFulfilled = callback)
}

is_coro_return_call <- function(x) {
  is_call(x, "coro_return", ns = "flowery")
}
is_coro_yield_call <- function(x) {
  is_call(x, "coro_yield", ns = "flowery")
}
