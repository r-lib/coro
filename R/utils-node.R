node_list_tail <- function(node) {
  while (!is_null(node_cdr(node))) {
    node <- node_cdr(node)
  }
  node
}

node_list_poke_car <- function(node, new_car) {
  if (is_null(node)) {
    new_node(new_car, NULL)
  } else {
    node_poke_car(node_list_tail(node), new_car)
    node
  }
}
node_list_poke_cdr <- function(node, new_cdr) {
  if (is_null(node)) {
    new_cdr
  } else {
    node_poke_cdr(node_list_tail(node), new_cdr)
    node
  }
}

# Unlike `[[` this returns `NULL` when OOB
node_get <- function(node, i) {
  if (node < 1L) {
    abort("`i` must be an integer greater than 0.")
  }
  while (i > 1L) {
    node <- node_cdr(node)
    i <- i - 1L
  }
  node_car(node)
}
