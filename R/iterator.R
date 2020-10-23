#' Iterator protocol
#'
#' @description
#' ```{r, child = "man/md/iterator.Rmd"}
#' ```
#' @export
exhausted <- function() {
  NULL
}
#' @export
is_exhausted <- function(x) {
  is_null(x)
}

#' Iterate over an iterator
#'
#' `iterate()` instruments `for` loops to support iteration with
#' flowery iterators and generators.
#'
#' @param loop A `for` loop expression.
#'
#' @seealso [collect()] is a functional that iterates over an
#'   iterator and returns the values.
#' @export
#' @examples
#' iter <- gen(for (x in 1:10) yield(x^2))
#' iterate(for (x in iter) {
#'   print(x * 100)
#' })
iterate <- function(loop) {
  loop <- substitute(loop)
  if (!is_call(loop, "for")) {
    abort("`loop` must be a `for` loop")
  }

  env <- caller_env()

  args <- node_cdr(loop)
  var <- as_string(node_car(args))
  iterator <- as_iterator(eval_bare(node_cadr(args), env))
  body <- node_cadr(node_cdr(args))

  loop_env <- current_env()

  elt <- NULL
  advance <- function() !is_exhausted(elt <<- iterator())
  update <- function() env[[var]] <- elt

  loop <- expr(
    while (!!call2(advance)) {
      !!call2(update)
      !!body
    }
  )
  eval_bare(loop, env)

  invisible(exhausted())
}

#' Iterable functions
#'
#' @description
#'
#' A flowery iterator is a function that implements the iteration
#' protocol:
#'
#' - The iterator is advanced by invoking it without argument. This
#'   returns the next value.
#'
#' - An iterator signals exhaustion by returning `NULL`.
#'
#'  The `NULL` sentinel synergises well with the R control flow
#' constructs like `while ()` as they return `NULL` when they are
#' done.
#'
#' @name iterator
NULL

#' @rdname iterator
#' @param x A vector to be transformed to an iterable function.
#'   Functions are returned as is.
#' @export
as_iterator <- function(x) {
  if (is_closure(x)) {
    return(x)
  }

  n <- length(x)
  i <- 0L

  function() {
    if (i == n) {
      return(exhausted())
    }

    i <<- i + 1L
    x[[i]]
  }
}
