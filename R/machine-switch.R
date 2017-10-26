
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

gen_env <- function(env, args) {
  env <- env_bury(env, !!! control_flow_ops)
  env <- env_bury(env, `_state` = "1", !!! args)
  env
}

`_goto` <- function(state, frame = caller_env()) {
  env_poke(frame, "_state", state)
  eval_bare(next_lang(), frame)
}
`_pause` <- function(state, value = NULL, frame = caller_env()) {
  env_poke(frame, "_state", state)
  eval_bare(return_lang(value), frame)
}
control_flow_ops <- list(`_goto` = `_goto`, `_pause` = `_pause`)
