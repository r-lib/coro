
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
  is_call(x, "yield", ns = c("", "coro"))
}
is_await_call <- function(x) {
  is_call(x, "await", ns = c("", "coro"))
}

# Useful to circumvent notes emitted by the bytecode compiler
next_call <- function() call("next")
break_call <- function() call("break")

call_lhs <- function(call) {
  node_cadr(call)
}
call_rhs <- function(call) {
  node_cadr(node_cdr(call))
}

yield_expr <- function(call, assign) {
  if (assign) {
    node_get(node_get(call, 3), 2)
  } else {
    node_get(call, 2)
  }
}
yield_lhs <- function(call, assign) {
  if (assign) {
    as_string(node_get(call, 2))
  } else {
    NULL
  }
}

stop_non_async_generator <- function(keyword) {
  abort(c(
    sprintf("Can't use `%s()` in a non-async generator.", keyword),
    i = "Do you need `async_generator()`?"
  ))
}
