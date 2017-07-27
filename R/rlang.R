
node_tail <- function(node) {
  while(!is_null(node_cdr(node))) {
    node <- node_cdr(node)
  }
  node
}

mut_node_tail_car <- function(node, new_car) {
  mut_node_car(node_tail(node), new_car)
}
mut_node_tail_cdr <- function(node, new_cdr) {
  mut_node_cdr(node_tail(node), new_cdr)
}

is_reference <- rlang:::is_reference
