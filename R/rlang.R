
dots_node <- function(...) {
  node_cdr(substitute(lang(...)))
}

is_language <- is_lang
get_environment <- get_env

node_poke_car <- mut_node_car
node_poke_cdr <- mut_node_cdr
node_poke_cadr <- mut_node_cadr
node_poke_cddr <- mut_node_cddr

poke_attr <- function(x, name, value) {
  invisible(.Call(rlang_poke_attr, x, sym(name), value))
}
