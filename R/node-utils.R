
node_walk_car <- function(.x, .f, ...) {
  node_poke_car(.x, .f(node_car(.x), ...))
}
node_walk_cdr <- function(.x, .f, ...) {
  node_poke_cdr(.x, .f(node_cdr(.x), ...))
}
node_walk_cadr <- function(.x, .f, ...) {
  node_poke_cadr(.x, .f(node_cadr(.x), ...))
}
node_walk_cddr <- function(.x, .f, ...) {
  node_poke_cddr(.x, .f(node_cddr(.x), ...))
}

node_list_walk <- function(.x, .f, ...) {
  rest <- .x
  while (!is_null(rest)) {
    .f(rest, ...)
    rest <- node_cdr(rest)
  }
  .x
}

node_list_walk_car <- function(.x, .f, ...) {
  f <- function(node) .f(node_car(node), ...)
  node_list_walk(.x, f)
}

node_list_tail <- function(node) {
  while (!is_null(node_cdr(node))) {
    node <- node_cdr(node)
  }
  node
}
node_list_tail_car <- function(node) {
  node_car(node_list_tail(node))
}
node_list_tail_cdr <- function(node) {
  node_car(node_list_tail(node))
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

node_list_detect <- function(.x, .p, ...) {
  rest <- .x

  while (!is_null(rest) && !.p(rest, ...)) {
    rest <- node_cdr(rest)
  }

  rest
}
node_list_detect_parent <- function(.x, .p, ..., .missing = null_node()) {
  rest <- .x
  parent <- .missing

  while (!is_null(rest) && !.p(rest, ...)) {
    parent <- rest
    rest <- node_cdr(rest)
  }

  if (is_null(rest)) {
    NULL
  } else {
    parent
  }
}

walk_node_list_tail <- function(.x, .f, ...) {
  tail <- node_list_tail(.x)
  .f(tail, ...)
  .x
}

node_list_walk_at <- function(.x, .at, .f, ...) {
  node <- node_list_detect(.x, is_reference, .at)

  if (is_null(node)) {
    abort("Can't find `.at`")
  } else {
    .f(node, ...)
    .x
  }
}
node_list_walk_at_parent <- function(.x, .at, .f, ...) {
  node <- node_list_detect_parent(.x, is_reference, .at)

  if (is_null(node)) {
    abort("Can't find `.at`")
  } else if (is_null_node(node)) {
    abort("`.at` has no parent")
  } else {
    .f(node, ...)
    .x
  }
}

node_list_poke_cdr_at <- function(x, at, newcdr) {
  node <- node_list_detect_parent(x, is_reference, at)

  if (is_null(node)) {
    abort("Can't find `.at`")
  } else {
    node_poke_cdr(node, newcdr)
    x
  }
}

null_node_sym <- quote(`__rlang_null_node`)
null_node <- function() {
  new_node(null_node_sym, NULL)
}
is_null_node <- function(x) {
  is_pairlist(x) &&
    identical(node_car(x), null_node_sym) &&
    identical(node_cdr(x), NULL)
}

node_clone <- function(node) {
  out <- duplicate(node, shallow = TRUE)

  node <- out
  while (!is_null(node)) {
    car <- node_car(node)

    if (typeof(car) %in% c("language", "pairlist")) {
      node_poke_car(node, node_clone(car))
    }

    node <- node_cdr(node)
  }

  out
}

call_tree_walk <- function(x, fn) {
  to_visit <- pairlist(x)

  while (!is_null(to_visit)) {
    x <- node_car(to_visit)
    to_visit <- node_cdr(to_visit)

    # Visit the node
    fn(x)

    if (!is_call(x)) {
      next
    }

    # Only add CAR to nodes to visit if it is a call
    if (is_call(node_car(x))) {
      x <- duplicate(x, shallow = TRUE)
    } else {
      x <- duplicate(node_cdr(x), shallow = TRUE)
    }

    if (!is_null(x)) {
      node_poke_cdr(node_list_tail(x), to_visit)
      to_visit <- x
    }
  }

  invisible(NULL)
}
