
return_invisible_lang <- return_lang(rlang::lang("invisible", NULL))

new_for_parts <- function(state, i, x, body = quote(yield())) {
  for_lang <- for_lang(i, x, body)
  parts <- with_state(state, for_parts(for_lang))

  node_poke_cddr(parts, NULL)
  parts
}
