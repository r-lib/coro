#' Make an async function
#'
#' `async()` functions are the building blocks of cooperative
#' concurrency. They suspend themselves with `await()` when they
#' expect an operation to take a long time to complete. While they
#' wait for the result, other async functions can be resumed once they
#' are ready to make progress.
#'
#' @param fn An anonymous function within which `await()` calls are
#'   allowed.
#' @return A function that returns a [promises::promise()].
#'
#' @section Concurrency framework:
#'
#' The default scheduler used by `async()` functions is provided by
#' the _later_ package. It currently only supports timers but these
#' are sufficient to implement basic cooperative concurrency. Since
#' this scheduler normally runs at top level, the async functions are
#' called back once all current computations have finished running.
#'
#' `async()` functions can be chained to promises from the _promises_
#' package.
#'
#' @seealso [async_generator()] and [await_each()].
#' @export
async <- function(fn) {
  assert_lambda(substitute(fn))
  generator0(fn, type = "async")
}
#' @rdname async
#' @param x An awaitable value, i.e. a [promise][promises::promise].
#' @export
await <- function(x) {
  abort("`await()` can't be called directly or within function arguments.")
}

#' Construct an async generator
#'
#' An async generator constructs iterable functions that are also
#' awaitables. They support both the `yield()` and `await()` syntax.
#' An async iterator can be looped within async functions and
#' iterators using `await_each()` on the input of a `for` loop.
#'
#' @param fn An anonymous function describing an async generator
#'   within which `await()` calls are allowed.
#' @return A generator factory. Generators constructed with this
#'   factory always return [promises::promise()].
#'
#' @seealso [async()] for creating awaitable functions and
#'   [async_collect()] for collecting the values of an async iterator.
#' @examples
#' # Creates awaitable functions that transform their inputs into a stream
#' new_stream <- async_generator(function(x) for (elt in x) yield(elt))
#'
#' # Maps a function to a stream
#' async_map <- async_generator(function(.i, .fn, ...) {
#'   for (elt in await_each(.i)) {
#'     yield(.fn(elt, ...))
#'   }
#' })
#'
#' # new_stream(1:3) %>% async_map(`*`, 2) %>% async_collect()
#' @export
async_generator <- function(fn) {
  assert_lambda(substitute(fn))
  generator0(fn, type = "async_generator")
}
#' @rdname async_generator
#' @inheritParams await
#' @export
await_each <- function(x) {
  abort("`await_each()` must be called within a `for` loop of an async function.")
}

#' @export
print.flowery_async <- function(x, ...) {
  # TODO: Print user-friendly async function by default. Make it
  # configurable for easier development.

  writeLines("<async>")
  print(unclass(x), ...)

  print_state_machine(x, ...)

  invisible(x)
}
#' @export
print.flowery_async_generator <- function(x, ...) {
  writeLines("<async/generator>")
  print(unclass(x), ...)

  print_state_machine(x, ...)

  invisible(x)
}

# Customisation point for the {async} package or any concurrency
# framework that defines a "then" operation
flowery_ops <- function(env) {
  ops <- env_get(env, ".__flowery_async_ops__.", inherit = TRUE, default = NULL)

  ops %||% list(
    package = "promises",
    `_then` = function(x, callback) promises::then(x, onFulfilled = callback),
    `_as_promise` = function(x) as_promise(x)
  )
}

as_promise <- function(x) {
  if (promises::is.promise(x)) {
    x
  } else {
    promises::promise_resolve(x)
  }
}
