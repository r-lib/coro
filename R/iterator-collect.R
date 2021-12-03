#' Iterate over iterator functions
#'
#' @description
#'
#' `loop()` and `collect()` are helpers for iterating over
#' [iterator functions][iterator] such as [generators][generator].
#'
#' - `loop()` takes a `for` loop expression in which the collection
#'   can be an iterator function.
#'
#' - `collect()` loops over the iterator and collects the values in a
#'   list.
#'
#' @param x An iterator function.
#' @param n The number of elements to collect. If `x` is an infinite
#'   sequence, `n` must be supplied to prevent an infinite loop.
#' @return `collect()` returns a list of values; `loop()` returns
#'   the [exhausted()] sentinel, invisibly.
#'
#' @seealso [async_collect()] for async generators.
#'
#' @examples
#' generate_abc <- generator(function() for (x in letters[1:3]) yield(x))
#' abc <- generate_abc()
#'
#' # Collect 1 element:
#' collect(abc, n = 1)
#'
#' # Collect all remaining elements:
#' collect(abc)
#'
#' # With exhausted iterators collect() returns an empty list:
#' collect(abc)
#'
#'
#' # With loop() you can use `for` loops with iterators:
#' abc <- generate_abc()
#' loop(for (x in abc) print(x))
#' @export
collect <- function(x, n = NULL) {
  steps <- n %&&% iter_take(n)
  reduce_steps(x, steps, along_builder(list()))
}
#' @rdname collect
#' @param loop A `for` loop expression.
#' @export
loop <- function(loop) {
  loop <- substitute(loop)
  if (!is_call(loop, "for")) {
    abort("`loop` must be a `for` expression.")
  }

  env <- caller_env()

  if (is_true(env$.__generator_instance__.)) {
    abort(c(
      "Can't use `loop()` within a generator.",
      i = "`for` loops already support iterators in generator functions."
    ))
  }

  args <- node_cdr(loop)
  var <- as_string(node_car(args))
  iterator <- as_iterator(eval_bare(node_cadr(args), env))
  body <- node_cadr(node_cdr(args))

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


#' Collect elements of an asynchronous iterator
#'
#' `async_collect()` takes an asynchronous iterator, i.e. an iterable
#' function that is also awaitable. `async_collect()` returns an
#' awaitable that eventually resolves to a list containing the values
#' returned by the iterator. The values are collected until exhaustion
#' unless `n` is supplied. The collection is grown geometrically for
#' performance.
#'
#' @inheritParams collect
#' @examples
#'
#' # Emulate an async stream by yielding promises that resolve to the
#' # elements of the input vector
#' generate_stream <- async_generator(function(x) for (elt in x) yield(elt))
#'
#' # You can await `async_collect()` in an async function. Once the
#' # list of values is resolved, the async function resumes.
#' async(function() {
#'   stream <- generate_stream(1:3)
#'   values <- await(async_collect(stream))
#'   values
#' })
#' @export
async_collect <- function(x, n = NULL) {
  steps <- n %&&% iter_take(n)
  async_reduce_steps(x, steps, along_builder(list()))
}
