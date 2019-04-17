#' Box a NULL value
#'
#' This returns a boxed `NULL` of class `null_box` that can be
#' returned from an iterator in order to return a literal `NULL`
#' without marking the iterator as done. If you use the `generator()`
#' syntax you can simply use
#'
#' @export
#' @examples
#' # Let's create an iterator that extracts each element of a
#' # vector. We'll want to support lists and lists might contain
#' # `NULL`. If that is the case we need
#' new_vector_iterator <- function(x) {
#'   n <- length(x)
#'   i <- 0
#'
#'   new_iterator(function() {
#'     while (i < n) {
#'       i <<- i + 1
#'       elt <- rlang::as_box_if(x[[i]], is.null, "null_box")
#'       return(elt)
#'     }
#'   })
#' }
#'
#'
#' # This iterator factory is equivalent to as_iterator():
#' iter <- new_vector_iterator(1:10)
#' iter()
#'
#' # Our iterator now supports `NULL` value:
#' iter <- new_vector_iterator(list(1, NULL, 3))
#' iter()
#' iter()
#' iter()
#'
#'
#' # Note that in the case of generators NULL values are automatically
#' # boxed by yield():
#' new_vector_iterator <- function(x) generator({
#'   # Here we can use `for` instead of `while` since the state is saved
#'   for (elt in x) yield(elt)
#' })
null_box <- function() {
  new_box(NULL, "null_box")
}
