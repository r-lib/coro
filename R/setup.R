#' Set up per-step state in a coroutine
#'
#' @description
#' `setup()` registers an expression that runs at the start of **every** step of
#' a [generator()] or [async()] function: the initial entry and every resumption
#' after a [yield()] or [await()]. Any `withr::defer()`, `withr::local_*()`, or
#' [on.exit()] registered while `expr` runs is torn down at the **end of that
#' step** (the next `yield`/`await`, a `return`, normal completion, an error, or
#' early close).
#'
#' This gives setup/teardown parity for each step of a coroutine, unlike a
#' top-level `on.exit()` which only fires once when the whole function exits.
#'
#' @details
#' - `expr` runs in an isolated environment: it can read the function's
#'   arguments, locals, and lexical scope, and mutate external state
#'   (`the$x <- 1`, `<<-`), but **plain assignments stay local to `expr`** and are
#'   not visible to the function body.
#' - When execution reaches a `setup()` call it is registered and runs for the
#'   current step; re-encountering the *same* `setup()` call (e.g. on a later loop
#'   iteration) is a no-op. A `setup()` inside an `if`/loop registers only if and
#'   when reached.
#' - Multiple `setup()` calls stack and run in registration order each step; their
#'   teardowns fire in reverse order.
#' - `setup()` cannot contain `yield()`/`await()` and its result cannot be
#'   assigned.
#'
#' Like [yield()] and [await()], `setup()` is a syntactic construct recognised by
#' the coroutine compiler. Calling it directly (outside a coroutine body) is an
#' error.
#'
#' @param expr An expression to run at the start of each step.
#'
#' @seealso [generator()], [async()], [yield()], [await()].
#' @export
setup <- function(expr) {
  abort("`setup()` can't be called directly or within function arguments.")
}
