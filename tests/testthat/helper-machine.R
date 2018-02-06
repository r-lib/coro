
return_invisible_lang <- return_lang(rlang::call2("invisible", NULL))

new_for_parts <- function(state, i, x,
                          next_state = NULL,
                          body = quote(yield())) {
  for_lang <- for_lang(i, x, body)
  parts <- with_state(state, for_parts(for_lang))

  if (!is_null(next_state)) {
    cond_branches <- node_cddr(node_cadr(node_cadr(parts)))
    goto_block <- node_cadr(cond_branches)
    node_poke_cadr(goto_block, goto_lang(next_state))
  }

  node_poke_cddr(parts, NULL)
  parts
}
