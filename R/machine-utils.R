
parts_srcrefs <- function(node) {
  refs <- NULL

  call_tree_walk(node, function(x) {
    x_refs <- block_srcref(x)
    if (!is_null(x_refs)) {
      refs <<- new_node(x_refs, refs)
    }
  })

  refs
}

block_srcref <- function(x) {
  if (is_call(x, "_block")) {
    block <- node_cadr(x)
    args <- node_cdr(block)
    refs <- attr(block, "srcref")[-1] %||% list(NULL)
    map2(args, refs, new_flowery_ref)
  }
}

# Pretty printing for snapshot tests
new_flowery_ref <- function(expr, ref) {
  structure(list(expr = expr, ref = ref), class = "flowery:::ref")
}
#' @export
`print.flowery:::ref` <- function(x, ...) {
  cat("expr: ")
  expr_print(x$expr)

  cat("ref: ")
  print(x$ref)

  invisible(x)
}

spliceable_part <- function(x) {
  cdr <- node_cdr(x)
  cadr <- node_car(cdr)

  if (is_call(cadr, "_block")) {
    node_cdr(node_cadr(cadr))
  } else {
    cdr
  }
}
