#' Create a generator function
#'
#' @description
#'
#' A generator is an [iterator function][iterator] that can pause its
#' execution with [yield()] and resume from where it left off. Because
#' they manage state for you, generators are the easiest way to create
#' iterators. The main difference between a regular function and a
#' generator is thus that you can [yield()] values. The following
#' rules apply:
#'
#' * Yielded values do not terminate the generator. If you call the
#'   generator again, the execution resumes right after the yielding
#'   point. All local variables are preserved.
#'
#' * Returned values terminate the generator. If called again after a
#' `return()`, the generator keeps returning `NULL`.
#'
#' Generators are compatible with all iterator features such as
#' [iterate()], [iter_adapt()], or [drain()].
#'
#' @param body The function body for the generator. It can [yield()]
#'   and `return()` values. Within a generator, `for` loops have
#'   [iterator] support.
#'
#' @export
#' @examples
#' # A generator statement creates an iterator function:
#' iter <- generator({
#'   yield("foo")
#'   yield("bar")
#'   "baz"
#' })
#' iter()
#' iter()
#'
#' # Once a generator has returned it keeps returning `NULL`. This
#' # signals to its caller that new values can no longer be
#' # produced. The generator is exhausted:
#' iter()
#' iter()
#'
#'
#' # You can only exhaust a generator once. Let's create a generator
#' # factory to make it easy to get fresh generators:
#' new_yielder <- function(...) {
#'   generator({
#'     values <- list(...)
#'     n <- length(values)
#'     last <- values[[n]]
#'     for (x in values[-n]) yield(x)
#'     last
#'   })
#' }
#' iter <- new_yielder("foo", "bar", "barbaz")
#'
#' # As generators are regular iterators, you can use all iterator
#' # tools such as iterate() which allows you to loop over all values
#' # with a `for` loop:
#' iterate(for (x in iter) cat(x, "\n"))
#'
#' # You can also check for `NULL` values that signal exhaustion to
#' # loop manually:
#' iter <- new_yielder("foo", "bar", "barbaz")
#' while (!is.null(new <- iter())) cat(new, "\n")
#'
#'
#' # The generator also has a short syntax `gen()`. It is completely
#' # identical to the long version but is handy when chaining
#' # generator expressions like in Python. Note that within a
#' # generator you can supply iterators to `for` loops just like in
#' # `iterate()`. This feature and the short alias make it handy to
#' # chain iterators:
#' numbers <- 1:10
#' odds <- gen(for (x in numbers) if (x %% 2 != 0) yield(x))
#' squares <- gen(for (x in odds) yield(x^2))
#' greetings <- gen(for (x in squares) yield(paste("Hey", x)))
#'
#'
#' # As all iterators, you can take() elements from a generator:
#' take(greetings, 2)
#'
#' # Or drain the remaining elements:
#' drain(greetings)
generator <- function(body) {
  body <- substitute(body)

  node <- set_returns(body)
  parts <- generator_parts(node)

  # Add a late return point
  return_call <- call2(quote(base::return), quote(invisible(NULL)))
  parts <- node_list_poke_cdr(parts, pairlist(block(return_call)))

  env <- env(caller_env(),
    `_state` = "1",
    `_return_state` = length(parts)
  )

  iter <- blast(function() {
    evalq(env, expr = {
      while (TRUE) {
        !!machine_switch_call(parts)
      }
    })
  })

  # Zap source references so you can see the state machine
  unstructure(iter)
}
generator_parts <- function(node) {
  reset_state()
  parts <- node_list_parts(node)

  if (is_null(parts)) {
    pairlist(new_call(block_sym, node))
  } else {
    parts
  }
}

#' @rdname generator
#' @export
gen <- generator

#' Yield a value from a generator
#'
#' @description
#'
#' `yield()` is like `return()` except that the function continues
#' execution at the yielding point when it is called again. `yield()`
#' can be called within loops and if-else branches but for technical
#' reasons it has a few limitations:
#'
#' * `yield()` cannot be called as part of a function argument. Code
#'   such as `list(yield())` is illegal.
#'
#' * `yield()` does not cross function boundaries. You can't use it a
#'   lambda function passed to `lapply()` for instance.
#'
#' @param x A value to yield.
#'
#' @seealso [generator()] for examples.
#' @export
yield <- function(x) {
  abort("`yield()` can't be called directly or within function arguments")
}
