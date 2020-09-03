
machine_switch_call <- function(parts) {
  parts <- node_list_enumerate_tags(parts)
  switch_args <- new_node(quote(`_state`), parts)
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
  env_poke(frame, "_state", state)
  eval_bare(call2(base::return, value), frame)
}
#' @rdname coro_goto
#' @export
coro_return <- function(value, frame = caller_env()) {
  # Goto NULL-return state to terminate iterator
  return_state <- env_get(frame, "_return_state")
  env_poke(frame, "_state", return_state)
  eval_bare(call2(base::return, value), frame)
}
