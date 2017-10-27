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
#' easily be [transformed][iter_adapt], mapped over and drained to a
#' final output vector.
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
#'   This is useful for looping over an iterator since you cannot know
#'   in advance whether an iterator is exhausted. You have to request
#'   the next element. `advance()` does the job of calling the
#'   iterator and checking if it is done.
#'
#'
#' @section Creating iterators:
#'
#' There are three ways of creating an iterator:
#'
#' * [gen()] is the recommended way of creating an iterator. It
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
#' @section Iterable functions:
#'
#' Iterators are thin wrappers around iterable functions which do the
#' actual work of generating data. In order to be iterable, a function
#' must meet these requirements:
#'
#' * It should be callable without arguments. This is how the iterator
#'   obtains the next value.
#'
#' * It should return `NULL` when the iterator has exhausted all
#'   elements. If the next element should be a literal `NULL`, return
#'   a [boxed NULL][null_box] instead. It will be automatically
#'   unboxed by the iterator wrapper.
#'
#' @seealso [gen()] is the recommended way of creating iterators.
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
#' # You loop over an iterator with `iterate()`. It instruments `for`
#' # to make it handle iterators:
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
  iter()
  !is_done(iter)
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
