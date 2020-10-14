
block <- function(...) {
  call2(quote(`{`), ...)
}

block_refs <- function(block) {
  node_cdr(as.pairlist(attr(block, "srcref")))
}

user_call <- function(expr) {
  if (is_call(expr, "user")) {
    expr
  } else {
    call("user", expr)
  }
}

new_refd_block <- function(node, refs = NULL) {
  block <- new_call(quote(`{`), node)

  if (!is_null(refs)) {
    # Implicit coercion from pairlist to list
    refs <- refs[seq_along(node)]

    # Add empty reference for `{`
    refs <- c(list(NULL), refs)

    attr(block, "srcref") <- refs
  }

  user_call(block)
}
refd_block <- function(expr, ref = NULL) {
  new_refd_block(new_node(expr), new_node(ref))
}

is_yield_call <- function(x) {
  is_call(x, "yield", ns = c("", "flowery"))
}
is_await_call <- function(x) {
  is_call(x, "await", ns = c("", "flowery"))
}