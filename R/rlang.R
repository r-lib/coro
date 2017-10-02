
dots_node <- function(...) {
  node_cdr(substitute(lang(...)))
}

node_list_tail <- function(node) {
  while (!is_null(node_cdr(node))) {
    node <- node_cdr(node)
  }
  node
}

node_list_poke_car <- function(node, new_car) {
  node_poke_car(node_list_tail(node), new_car)
}
node_list_poke_cdr <- function(node, new_cdr) {
  node_poke_cdr(node_list_tail(node), new_cdr)
}

is_reference <- rlang:::is_reference

node_poke_car <- mut_node_car
node_poke_cdr <- mut_node_cdr
