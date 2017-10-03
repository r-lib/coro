
walk_node_list_tail <- function(.x, .f, ...) {
  tail <- node_list_tail(.x)
  .f(tail, ...)
  .x
}

null_node_sym <- quote(`__rlang_null_node`)
null_node <- function() {
  node(null_node_sym, NULL)
}
is_null_node <- function(x) {
  is_pairlist(x) &&
    identical(node_car(x), null_node_sym) &&
    identical(node_cdr(x), NULL)
}
