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
#' - `setup()` cannot contain `yield()`/`await()` and its result cannot be
#'   assigned.
#'
#' Like [yield()] and [await()], `setup()` is a syntactic construct recognised by
#' the coroutine compiler. Calling it directly (outside a coroutine body) is an
#' error.
#'
#' @section Using setup() in a loop:
#'
#' `setup()` cannot be used inside a loop (`for`, `while`, or `repeat`); doing so
#' is an error. Per-step registration interacts poorly with iteration: a loop
#' body may branch and yield, so it is ambiguous when the setup should be
#' registered, re-run, and torn down relative to each iteration.
#'
#' When you need per-iteration setup and teardown, factor the loop body into its
#' own generator and delegate to it with a `for` loop. Each sub-generator
#' instance has its own `setup()` lifecycle, scoped to its steps. The second
#' `yield()` below shows the effect: after the first yield the step ends and
#' `the$x` is reset to `0`, then `setup()` re-runs before the next step, so the
#' second yield again sees `i`:
#'
#' ```r
#' the <- new.env()
#' the$x <- 0
#'
#' step <- generator(function(i) {
#'   setup({
#'     the$x <- i                 # set up before every step...
#'     withr::defer(the$x <- 0)   # ...and torn down at the end of each
#'   })
#'   yield(the$x)   # the$x is i here
#'   yield(the$x)   # still i: reset to 0 at the last step end, then setup() re-ran
#' })
#'
#' gen <- generator(function() {
#'   for (i in 1:3) {
#'     for (x in step(i)) yield(x)
#'   }
#' })
#'
#' collect(gen())   # 1, 1, 2, 2, 3, 3
#' the$x            # 0
#' ```
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
#'     withr::defer(the$x <- old_x)
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
#' @export
setup <- function(expr) {
  abort("`setup()` can't be called directly or within function arguments.")
}
