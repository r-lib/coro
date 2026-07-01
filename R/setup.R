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
#'   current step. A `setup()` inside an `if` branch registers only if and when
#'   reached.
#' - Multiple `setup()` calls stack and run in registration order each step; their
#'   teardowns fire in reverse order.
#' - `setup()` cannot be used inside a loop (`for`/`while`/`repeat`), cannot
#'   contain `yield()`/`await()`, and its result cannot be assigned.
#'
#' For per-iteration setup and teardown, factor the loop body into its own
#' generator and delegate to it with a `for` loop (see examples). Each
#' sub-generator instance has its own `setup()` lifecycle, scoped to that
#' iteration.
#'
#' Like [yield()] and [await()], `setup()` is a syntactic construct recognised by
#' the coroutine compiler. Calling it directly (outside a coroutine body) is an
#' error.
#'
#' @param expr An expression to run at the start of each step.
#'
#' @seealso [generator()], [async()], [yield()], [await()].
#' @examples
#' the <- new.env()
#' the$x <- 0
#'
#' gen <- generator(function() {
#'   setup({
#'     old_x <- the$x
#'     the$x <- 1
#'     on.exit(the$x <- old_x, add = TRUE)
#'   })
#'   yield(the$x)   # 1 while the step runs
#'   yield(the$x)   # 1 again: setup re-ran for this step
#' })
#'
#' g <- gen()
#' g()        # 1
#' the$x      # 0 — restored at the end of the step
#' g()        # 1
#' the$x      # 0
#'
#' # `setup()` can't be used inside a loop. For per-iteration setup and
#' # teardown, move the loop body into a sub-generator and delegate to it with a
#' # `for` loop; the sub-generator's `setup()` runs afresh for each iteration:
#' the$x <- 0
#' step <- generator(function(i) {
#'   setup({
#'     the$x <- i                       # set up for this iteration...
#'     on.exit(the$x <- 0, add = TRUE)  # ...and torn down at the end of it
#'   })
#'   yield(the$x)
#' })
#'
#' gen <- generator(function() {
#'   for (i in 1:3) {
#'     for (x in step(i)) yield(x)
#'   }
#' })
#'
#' collect(gen())   # 1, 2, 3 — each produced with the$x set for that iteration
#' the$x            # 0 — restored after every iteration
#' @export
setup <- function(expr) {
  abort("`setup()` can't be called directly or within function arguments.")
}
