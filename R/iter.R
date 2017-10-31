#' Iterator functions
#'
#' @description
#'
#' R programming is usually about full lists and vectors. Vectorised
#' operations such as `+` or function mappers like `lapply()` operate
#' on whole collections of elements. However these idioms do not work
#' so well when the data does not fit in memory. In this case vector
#' idioms must be replaced with chunk ones. Iterators are a convenient
#' way of structuring the generation of chunks of data. Iterators can
#' easily be [transformed][iter_adapt], [mapped over][steps], or
#' [drained][drain()] to a final output vector.
#'
#' Formally, an iterator is a function that returns a new value each
#' time it is called and that supports:
#'
#' * `deref()` to dereference the current value of the iterator.
#'
#' * `is_done()` to check if the iterator has exhausted its
#'   elements. Calling an exhausted iterator causes an error of class
#'   `exhausted_iter`.
#'
#' * `advance()` to advance to the next value. This is similar to
#'   calling the iterator for a new element but returns `TRUE` if the
#'   iterator indeed had another element or `FALSE` if the iterator
#'   was exhausted. The element can then be retrieved with `deref()`.
#'
#' The recommended way of creating an iterator is with [generator()].
#'
#'
#' @section Creating iterators:
#'
#' There are three ways of creating an iterator:
#'
#' * [generator()] is the recommended way of creating an iterator. It
#'   creates a generator function that can pause itself and yield a
#'   value. When it is called again it resumes from where it left
#'   off. Generators are a convenient way of creating iterators
#'   because they keep their state.
#'
#' * `new_iterator()` takes an iterable function (see section below)
#'   and wraps it in a proper iterator that can be dereferenced and
#'   advanced.
#'
#' * `as_iterator()` also supports vectors. It creates an iterator
#'   that iterates over the elements. If passed a regular function, it
#'   is equivalent to `new_iterator()`. If passed an iterator, it is a
#'   no-op. `as_iterator()` is convenient for creating functions that
#'   support both iterators and vectors.
#'
#'   Vector-like objects are also supported if the class implements
#'   `length()` and `[[` methods. The extraction method must support
#'   positions.
#'
#'
#' @section Termination of an iterator:
#'
#' There is no general way to know in advance whether an iterator has
#' remaining elements. E.g. when iterating over a stream of data the
#' stream might close abruptly. The main purpose of `advance()` and
#' `deref()` is to make it easy to work with iterators.
#'
#' * You first try to `advance()` your iterator. If advance was
#'   succesful, the iterator now sits at the next value. Otherwise the
#'   iterator is done and you can finish the current operation.
#'
#' * If there was indeed a next value, you can obtain it by
#'   dereferencing the iterator with `deref()`.
#'
#' The idiom of advancing and dereferencing is useful for looping
#' manually over an iterator:
#'
#' ```
#' while (advance(iter)) do(deref(iter))
#' ```
#'
#' Note that functions like [iterate()] or [drain()] deal with the
#' looping automatically so that you don't have to worry about
#' termination and derefencing.
#'
#'
#' @section Iterable functions:
#'
#' Technically, iterators are thin wrappers around iterable functions
#' which do the actual work of generating data. While in most cases
#' [generator()] is sufficient to create iterators, it is also
#' possible to create iterators with normal (but iterable) functions
#' that you pass to `new_iterator()`. In order to be iterable, a
#' function must meet these requirements:
#'
#' * It should be callable without arguments. This is how the iterator
#'   obtains the next value.
#'
#' * It should signal termination by returning `NULL`. If the next
#'   element is a literal `NULL`, you can return a [boxed
#'   NULL][null_box]. It will be automatically unboxed and won't cause
#'   the iterator to terminate.
#'
#' @seealso [generator()] is the recommended way of creating iterators.
#' @name iterator
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
#' iter <- new_counter(3)
#' iter()
#'
#' # This function supports all iterator features. It can be
#' # dereferenced, advanced, and tested for termination:
#' advance(iter)
#' deref(iter)
#' is_done(iter)
#'
#' # A tricky aspect of iterators is that they don't know whether
#' # there is a next element. Always use `advance()` when looping over
#' # an iterator. It tries to advance to the next element and returns
#' # TRUE if it was succeful, FALSE otherwise. You then use `deref()`
#' # to get the value of this new element:
#' iter <- new_counter(3)
#' while (advance(iter)) {
#'   cat(deref(iter), "to go!\n")
#' }
#'
#' # It is often easier to loop over an iterator with `iterate()`. It
#' # instruments `for` and makes it handle iterators:
#' iter <- new_counter(3)
#' iterate(for(n in iter) cat(n, "to go!\n"))
#'
#'
#' # It is much easier to create iterators with generators:
#' new_counter <- function(n) {
#'   force(n)
#'   generator({
#'     while (n > 0) {
#'       n <- n - 1
#'       yield(n)
#'     }
#'   })
#' }
#'
#' # This generator behaves exactly as our other iterator function:
#' iter <- new_counter(3)
#' iterate(for(n in iter) cat(n, "to go!\n"))
NULL

#' @rdname iterator
#' @param fn An iterable function. It should be callable without
#'   arguments and return `NULL` when the iterator is exhausted.
#' @export
new_iterator <- function(fn) {
  stopifnot(is_closure(fn))

  # Flag so methods can check that they have an iterator
  `_flowery_iterator` <- TRUE

  done <- FALSE
  last <- NULL

  iter <- function() {
    if (done) {
      abort("Iterator is done", "exhausted_iter")
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

  set_class(iter, "iterator")
}
#' @rdname iterator
#' @param x For `is_iterator()`, an object to test. For
#'   `as_iterator()`, a vector, iterator, or iterable function. S3
#'   objects with `length()` and position-based `[[` methods are also
#'   supported.
#' @export
is_iterator <- function(x) {
  inherits(x, "iterator")
}

#' @rdname iterator
#' @param iter An iterator function.
#' @export
deref <- function(iter) {
  stopifnot(is_iterator(iter))
  env_get(get_iter_env(iter), "last")
}
#' @rdname iterator
#' @export
advance <- function(iter) {
  stopifnot(is_iterator(iter))
  !(is_null(iter()) && is_done(iter))
}
#' @rdname iterator
#' @export
is_done <- function(iter) {
  stopifnot(is_iterator(iter))
  env_get(get_iter_env(iter), "done")
}

get_iter_env <- function(iter) {
  env <- get_env(iter)
  if (!env_has(env, "_flowery_iterator")) {
    abort("`iter` must be an iterator")
  }
  env
}

#' @rdname iterator
#' @export
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

#' @export
print.iterator <- function(x, ...) {
  cat("<iterator>\n")
  fn <- env_get(get_iter_env(x), "fn")
  print(fn)

  invisible(x)
}
