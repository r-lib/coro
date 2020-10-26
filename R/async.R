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
#' @seealso [async_generator()] and [await_each()];
#'   [flowery_debug()] for step-debugging.
#'
#' @examples
#' # This async function counts down from `n`, sleeping for 2 seconds
#' # at each iteration:
#' async_count_down <- async(function(n) {
#'   while (n > 0) {
#'     cat("Down", n, "\n")
#'     await(async_sleep(2))
#'     n <- n - 1
#'   }
#' })
#'
#' # This async function counts up until `stop`, sleeping for 0.5
#' # seconds at each iteration:
#' async_count_up <- async(function(stop) {
#'   n <- 1
#'   while (n <= stop) {
#'     cat("Up", n, "\n")
#'     await(async_sleep(0.5))
#'     n <- n + 1
#'   }
#' })
#'
#' # You can run these functions concurrently using `promise_all()`
#' if (FALSE) {
#'   promises::promise_all(async_count_down(5), async_count_up(5))
#' }
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
#' @description
#'
#' An async generator constructs iterable functions that are also
#' awaitables. They support both the `yield()` and `await()` syntax.
#' An async iterator can be looped within async functions and
#' iterators using `await_each()` on the input of a `for` loop.
#'
#' The iteration protocol is derived from the one described in
#' [`iterator`][iterator]. An async iterator always returns a
#' promise. When the iterator is exhausted, it returns a resolved
#' promise to the exhaustion sentinel.
#'
#' @param fn An anonymous function describing an async generator
#'   within which `await()` calls are allowed.
#' @return A generator factory. Generators constructed with this
#'   factory always return [promises::promise()].
#'
#' @seealso [async()] for creating awaitable functions;
#'   [async_collect()] for collecting the values of an async iterator;
#'   [flowery_debug()] for step-debugging.
#' @examples
#' # Creates awaitable functions that transform their inputs into a stream
#' generate_stream <- async_generator(function(x) for (elt in x) yield(elt))
#'
#' # Maps a function to a stream
#' async_map <- async_generator(function(.i, .fn, ...) {
#'   for (elt in await_each(.i)) {
#'     yield(.fn(elt, ...))
#'   }
#' })
#'
#' # Example usage:
#' if (FALSE) {
#'   library(magrittr)
#'   generate_stream(1:3) %>% async_map(`*`, 2) %>% async_collect()
#' }
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
print.flowery_async <- function(x, ..., internals = FALSE) {
  writeLines("<async>")
  print_generator(x, ..., internals = internals)
}
#' @export
print.flowery_async_generator <- function(x, ..., internals = FALSE) {
  writeLines("<async/generator>")
  print_generator(x, ..., internals = internals)
}


#' Async operations
#'
#' @description
#'
#' Customisation point for the _async_ package or any concurrency
#' framework that defines a "then" operation. Assign the result of
#' `async_ops()` to the `.__flowery_async_ops__.` symbol in your
#' namespace.
#'
#' @param package The package name of the framework as a
#'   string. `async()` and `async_generator()` check that the package
#'   is installed at runtime.
#' @param then A function of two arguments. The first argument is a
#'   promise object (as created by `as_promise`). The second argument
#'   is a callback function that must be called once the promise
#'   object is resolved.
#' @param as_promise A function of one argument. It should be a no-op
#'   when passed a promise object, and otherwise wrap the value in a
#'   resolved promise.
#'
#' @keywords internal
#' @export
async_ops <- function(package, then, as_promise) {
  stopifnot(
    is_string(package),
    is_function(then),
    is_function(as_promise)
  )
  structure(
    list(
      package = package,
      then = then,
      as_promise = as_promise
    ),
    class = "flowery_async_ops"
  )
}

get_async_ops <- function(env) {
  ops <- env_get(env, ".__flowery_async_ops__.", inherit = TRUE, default = NULL)

  if (!is_null(ops)) {
    return(ops)
  }

  async_ops(
    package = "promises",
    then = op_then,
    as_promise = op_as_promise
  )
}
op_then <- function(x, callback) {
  promises::then(
    x,
    onFulfilled = callback,
    onRejected = function(cnd) callback(stop(cnd))
  )
}
op_as_promise <- function(x) {
  if (promises::is.promise(x)) {
    x
  } else {
    promises::promise_resolve(x)
  }
}

#' Sleep asynchronously
#' @param seconds The number of second to sleep.
#' @return A chainable promise.
#' @export
async_sleep <- function(seconds) {
  promises::promise(function(resolve, reject) {
    later::later(~ resolve(NULL) , delay = seconds)
  })
}
