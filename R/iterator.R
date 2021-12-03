#' Iterator protocol
#'
#' @description
#'
#' An __iterator__ is a function that implements the following
#' protocol:
#'
#' - Calling the function advances the iterator. The new value is
#'   returned.
#'
#' - When the iterator is exhausted and there are no more elements to
#'   return, the symbol `quote(exhausted)` is returned. This signals
#'   exhaustion to the caller.
#'
#' - Once an iterator has signalled exhaustion, all subsequent
#'   invokations must consistently return `coro::exhausted()` or
#'   `as.symbol(".__exhausted__.")`.
#'
#' ```{r}
#' iterator <- as_iterator(1:3)
#'
#' # Calling the iterator advances it
#' iterator()
#' iterator()
#'
#' # This is the last value
#' iterator()
#'
#' # Subsequent invokations return the exhaustion sentinel
#' iterator()
#' ```
#'
#' Because iteration is defined by a protocol, creating iterators is
#' free of dependency. However, it is often simpler to create
#' iterators with [generators][generator], see
#' `vignette("generator")`. To loop over an iterator, it is simpler
#' to use the [loop()] and [collect()] helpers provided in this
#' package.
#'
#'
#' @section Properties:
#'
#' Iterators are __stateful__. Advancing the iterator creates a
#' persistent effect in the R session. Also iterators are
#' __one-way__. Once you have advanced an iterator, there is no going
#' back and once it is exhausted, it stays exhausted.
#'
#' Iterators are not necessarily finite. They can also represent
#' infinite sequences, in which case trying to exhaust them is a
#' programming error that causes an infinite loop.
#'
#'
#' @section The exhausted sentinel:
#'
#' Termination of iteration is signalled via a sentinel value,
#' `as.symbol(".__exhausted__.")`. Alternative designs include:
#'
#' - A condition as in python.
#' - A rich value containing a termination flag as in Javascript.
#'
#' The sentinel design is a simple and efficient solution but it has a
#' downside. If you are iterating over a collection of elements that
#' inadvertently contains the sentinel value, the iteration will be
#' terminated early. To avoid such mix-ups, the sentinel should only
#' be used as a temporary value. It should be created from scratch by
#' a function like `coro::exhausted()` and never stored in a container
#' or namespace.
#'
#' @name iterator
NULL

#' @rdname iterator
#' @export
exhausted <- function() {
  as.symbol(".__exhausted__.")
}
#' @rdname iterator
#' @param x An object.
#' @export
is_exhausted <- function(x) {
  identical(x, exhausted())
}

#' Transform an object to an iterator
#'
#' @description
#'
#' `as_iterator()` is a generic function that transforms its input to
#' an [iterator function][iterator]. The default implementation
#' is as follows:
#'
#' - Functions are returned as is.
#'
#' - Other objects are assumed to be vectors with `length()` and `[[`
#'   methods.
#'
#' Methods must return functions that implement coro's [iterator
#' protocol][iterator].
#'
#' `as_iterator()` is called by coro on the RHS of `in` in `for`
#' loops. This applies within [generators][generator], [async
#' functions][async], and [loop()].
#'
#' @param x An object.
#' @return An iterable function.
#'
#' @export
#' @examples
#' as_iterator(1:3)
#'
#' i <- as_iterator(1:3)
#' loop(for (x in i) print(x))
as_iterator <- function(x) {
  UseMethod("as_iterator")
}
#' @rdname as_iterator
#' @export
as_iterator.default <- function(x) {
  if (is_closure(x)) {
    return(x)
  }

  # This uses `length()` and `{{` rather than `vec_size()` and
  # `vec_slice2()` for compatibility with base R because
  # `as_iterator()` is used in generators to instrument for loops
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

on_load(on_package_load("reticulate", {
  if (!s3_has_method("coro::as_iterator", "python.builtin.iterator")) {
    s3_register("coro::as_iterator", "python.builtin.iterator", function(x) {
      function() {
        reticulate::iter_next(x, completed = exhausted())
      }
    })
  }
}))
