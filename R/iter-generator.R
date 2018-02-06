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
#' * Yielded values never terminate the iterator, even a yielded
#'   `NULL`.  If you call the generator or [advance()] it, the
#'   execution resumes right after the yielding point. All local
#'   variables are preserved.
#'
#' * Returned values always terminate the iterator. Reentering the
#'   generator after a return is an error.
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
#' # As long as the generator yields, the iterator is not done:
#' is_done(iter)
#'
#' # Once a generator has returned the iterator is done:
#' iter()
#' is_done(iter)
#'
#'
#' # Like any iterator, once a generator has exhausted its elements it
#' # fails with an error if you reenter it:
#' # iter()  # Would be an error if run
#'
#' # Let's regenerate our generator. If you're going to use the same
#' # kind of iterators repeatedly, it often makes sense to create a
#' # generator factory for that purpose:
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
#' # You can also use advance() and deref() to loop manually:
#' iter <- new_yielder("foo", "bar", "barbaz")
#' while (advance(iter)) cat(deref(iter), "\n")
#'
#'
#' # The termination condition of a generator works the way you would
#' # expect. You can *yield* `NULL` from a generator without
#' # terminating the iteration:
#' iter <- generator(while (TRUE) yield(NULL))
#' iter()
#' iter()
#' is_done(iter)
#'
#' # On the other hand *returning* NULL terminates the iterator:
#' iter <- generator({ while (TRUE) return(NULL) })
#' advance(iter)
#'
#' # This is particularly handy in loops because they return `NULL`
#' # when the looping is over. In the following loop, the last yielded
#' # value is 3L. The generator is then reentered a last time, at
#' # which point the loop completes and the generator returns NULL.
#' # This signals that the iterator has completed:
#' iter <- generator(for (x in 1:3) yield(x))
#' iterate(for (x in iter) cat("iteration", x, "\n"))
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
#' # As all iterators, you can take() elements from a generator:
#' take(greetings, 2)
#'
#' # Or drain the remaining elements:
#' drain(greetings)
generator <- function(body) {
  node <- set_returns(enexpr(body))
  parts <- generator_parts(node)

  # Add a late return point
  return_lang <- call2(base::return, quote(invisible(NULL)))
  parts <- node_list_poke_cdr(parts, node_list(block(return_lang)))

  env <- env_bury(caller_env(),
    `_state` = "1",
    `_return_state` = length(parts),
    !!! control_flow_ops
  )

  iter <- expand(function() {
    evalq(env, expr = {
      while (TRUE) {
        !! machine_switch_lang(parts)
      }
    })
  })

  new_iterator(iter)
}
generator_parts <- function(node) {
  reset_state()
  parts <- node_list_parts(node)

  if (is_null(parts)) {
    node_list(new_language(block_sym, node))
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
