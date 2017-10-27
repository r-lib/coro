
iter <- function(body, env = caller_env()) {
  body <- enexpr(body)
  fn <- new_function(body, env = env)
  new_iterator(fn)
}

#' Create a new iterator
#'
#' @description
#'
#' This wraps `fn` in an iterator function that supports:
#'
#' * [deref()] to dereference the current value of the iterator.
#' * [advance()] to advance to the next value.
#' * [is_done()] to check if the iterator is terminated
#'
#' See [iter()] for more information on iterators.
#'
#'
#' @section Iterable functions:
#'
#' In order to be iterable, `fn()` must meet these specifications:
#'
#' * It should be callable without arguments. This is how the iterator
#'   obtains the next value.
#'
#' * It should return `NULL` when the iterator has exhausted all
#'   elements. If the next element is a literal `NULL`, return a
#'   [boxed NULL][null_box] instead. It will be automatically unboxed.
#'
#' @param fn A iterable function.
#'
#' @seealso [as_iterator()]
#' @export
#' @examples
#' # An iterator is a stateful function since it must return different
#' # results each time it is called. A convenient way of setting up
#' # the state is with a factory function, i.e. a function that
#' # returns another function:
#' new_counter <- function(n) {
#'   force(n)
#'   fn <- function() {
#'     if (n == 0) {
#'       # Return NULL to finish iteration
#'       return(NULL)
#'     }
#'
#'     # Update the counter each time:
#'     n <<- n - 1
#'
#'     n
#'   }
#'
#'   # Wrapping `fn` in an iterator enables many features
#'   new_iterator(fn)
#' }
#'
#' # We can instantiate a new iterator by calling the factory:
#' it <- new_counter(3)
#' it()
#'
#' # This function supports all iterator features. It can be
#' # dereferenced, advanced, and tested for termination:
#' advance(it)
#' deref(it)
#' is_done(it)
#'
#' # You can also use `iterate()`
#' it <- new_counter(3)
#' iterate(for(n in it) cat(n, "to go!\n"))
new_iterator <- function(fn) {
  stopifnot(is_closure(fn))

  # Flag so methods can check that they have an iterator
  `_flowery_iterator` <- TRUE

  done <- FALSE
  last <- NULL

  iter <- function() {
    if (done) {
      abort("Iterator is done")
    }

    out <- withVisible(fn())
    last <<- out$value

    if (is_null(last)) {
      done <<- TRUE
    } else if (is_box(last, "null_box")) {
      last <<- NULL
    }

    if (out$visible) {
      last
    } else {
      invisible(last)
    }
  }

  set_attrs(iter, class = "iterator")
}

is_iterator <- function(x) {
  inherits(x, "iterator")
}

deref <- function(x) {
  stopifnot(is_iterator(x))
  env_get(iter_env(x), "last")
}
advance <- function(x) {
  stopifnot(is_iterator(x))
  !(is_null(x()) && is_done(x))
}
is_done <- function(x) {
  stopifnot(is_iterator(x))
  env_get(iter_env(x), "done")
}

iter_env <- function(iter) {
  env <- get_env(iter)
  if (!env_has(env, "_flowery_iterator")) {
    abort("Expected an iterator")
  }
  env
}

print.iterator <- function(x, ...) {
  cat("<iterator>\n")
  fn <- env_get(iter_env(x), "fn")
  print(fn)

  invisible(x)
}

null_box <- function() {
  box(NULL, "null_box")
}

# Requires length() and `[[` methods
as_iterator <- function(x) {
  if (is_iterator(x)) {
    return(x)
  }
  if (is_closure(x)) {
    return(new_iterator(x))
  }

  n <- length(x)
  i <- 0L

  iter <- function() {
    if (i == n) {
      return(NULL)
    }

    i <<- i + 1L
    as_box_if(x[[i]], is_null, "null_box")
  }

  new_iterator(iter)
}

#' Box a NULL value
#'
#' This returns a boxed `NULL` of class `null_box` that can be
#' returned from an iterator in order to return a literal `NULL`
#' without marking the iterator as done.
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
#' new_vector_iterator <- function(x) gen({
#'   # Here we can use `for` instead of `while` since the state is saved
#'   for (elt in x) yield(elt)
#' })
null_box <- function() {
  box(NULL, "null_box")
}
