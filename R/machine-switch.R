
machine_switch_lang <- function(parts) {
  parts <- node_list_enumerate_tags(parts)
  switch_args <- node(quote(`_state`), parts)
  new_language(switch_sym, switch_args)
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

`_goto` <- function(state, frame = caller_env()) {
  env_poke(frame, "_state", state)
  eval_bare(next_lang(), frame)
}
`_pause` <- function(state, value = NULL, frame = caller_env()) {
  env_poke(frame, "_state", state)
  if (is_null(value)) {
    value <- null_box()
  }
  eval_bare(lang(base::return, value), frame)
}
`_return` <- function(value, frame = caller_env()) {
  if (!is_null(value)) {
    value <- done_box(value)
  }
  eval_bare(lang(base::return, value), frame)
}
control_flow_ops <- list(
  `_goto` = `_goto`,
  `_pause` = `_pause`,
  `return` = `_return`
)
