#' Set up per-step state in a coroutine
#'
#' @description
#' `setup()` registers an expression that runs at the start of **every** step of
#' a [generator()]/[async()] function — each time it is **entered or resumed** —
#' and tears down anything it registers (`withr::defer()`, `withr::local_*()`, or
#' [on.exit()]) at the **end of that step**, when the coroutine **suspends**
#' (`yield()`/`await()`) or **finishes** (`return`, completion, error, or close).
#'
#' This gives setup/teardown parity for each step of a coroutine, unlike a
#' top-level `on.exit()` which only fires once when the whole function exits.
#'
#' @details
#' - `expr` runs in a child of the coroutine's environment: it can read the
#'   function's arguments, locals, and lexical scope, and mutate external state
#'   (`the$x <- 1`, `<<-`), but **plain assignments stay local to `expr`** and are
#'   not visible to the function body (see the "Assignments and scope" section).
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
#' @section When setup and teardown run:
#'
#' It helps to compare `setup()` with a regular `withr::defer()`/[on.exit()] over
#' a multi-step run:
#'
#' * A regular `withr::defer()`/[on.exit()] in the function body registers
#'   **once** and runs **once**, when the whole coroutine finishes.
#' * A `setup()` body — including any assignments in it — runs at the **start of
#'   every step**.
#' * A `withr::defer()`/[on.exit()] registered inside `setup()` runs at the **end
#'   of every step**.
#'
#' The log below traces a generator with two `yield()`s (so three steps):
#'
#' ```r
#' log <- character()
#'
#' gen <- generator(function() {
#'   log <<- c(log, "beginning")
#'   withr::defer(log <<- c(log, "end  (once, at function exit)"))
#'   setup({
#'     x <- "local"   # an assignment here runs at each step start (local to setup)
#'     log <<- c(log, "  setup body     (start of every step)")
#'     withr::defer(log <<- c(log, "  setup defer    (end of every step)"))
#'   })
#'   log <<- c(log, "    body 1")
#'   yield("a")
#'   log <<- c(log, "    body 2")
#'   yield("b")
#'   log <<- c(log, "    body 3")
#' })
#'
#' invisible(collect(gen()))
#' writeLines(log)
#' #> beginning
#' #>   setup body     (start of every step)
#' #>     body 1
#' #>   setup defer    (end of every step)
#' #>   setup body     (start of every step)
#' #>     body 2
#' #>   setup defer    (end of every step)
#' #>   setup body     (start of every step)
#' #>     body 3
#' #>   setup defer    (end of every step)
#' #> end  (once, at function exit)
#' ```
#'
#' @section Assignments and scope:
#'
#' `setup()` runs its body in a child of the coroutine's environment. It can read
#' the function's variables and lexical scope, but a plain assignment (`x <- 1`)
#' creates a *local* binding that the function body does not see.
#'
#' This is deliberate. Unlike [on.exit()] and `withr::defer()`, which register
#' once and run at exit, `setup()` re-runs its body before *every* step. If plain
#' assignments were visible to the body, each step would silently overwrite
#' whatever the body had accumulated. In the generator below, `setup()`'s local
#' `n` does not touch the body's `n`, so it yields the expected `5` then `6`; were
#' the assignment visible, the second step would reset `n` to `100` and yield
#' `101`:
#'
#' ```r
#' generator(function() {
#'   n <- 5
#'   setup({
#'     # local to setup; does NOT set the generator's `n`
#'     n <- 100
#'   })
#'   yield(n)    # yields `5`
#'   n <- n + 1
#'   yield(n)    # yields `6`
#' })
#' ```
#'
#' To change state that the body (or the outside world) can see, mutate an
#' existing object instead of creating a local binding: assign into an
#' environment (`the$x <- 1`) or use `<<-` to update an enclosing binding, and
#' pair the change with `withr::defer()`/[on.exit()] to restore it at the end of
#' each step. (See "Examples" section)
#'
#' If you need a value computed per step *and* visible to the body, use the
#' sub-generator pattern shown in "Using `setup()` in a loop".
#'
#' @section Using `setup()` in a loop:
#'
#' `setup()` cannot be used inside a loop (`for`, `while`, or `repeat`); doing so
#' is an error. Per-step registration interacts poorly with iteration: a loop
#' body may itself branch and yield, so it is ambiguous when the setup should be
#' registered, re-run, and torn down relative to each iteration.
#'
#' ```r
#' generator(function() {
#'   for (i in 1:3) {
#'     # Error! Can't use `setup()` within a loop
#'     setup({
#'       the$x <- i
#'       withr::defer(the$x <- 0)
#'     })
#'     yield(the$x)
#'   }
#' })
#' ```
#'
#' When you need setup and teardown *per iteration*, move the loop body into its
#' own generator and drive it from the outer generator with `for (x in generate_x(i))`.
#' The key idea is that a generator's `setup()` is scoped to *that generator's
#' own steps*: it re-runs at the start of each step and its teardown fires at the
#' end of each step, independently of whatever is consuming it.
#'
#' Follow `the$x` through the example below. It is a shared variable, yet the
#' mutation inside `generate_x(i)` is never observable from the outer generator:
#'
#' * **Inside `generate_x(i)`.** Each time `generate_x(i)` resumes it re-runs its setup
#'   (`the$x <- i`); each time it suspends at a `yield()` it runs its teardown
#'   (`the$x <- 0`). So at every `yield()` *inside* `generate_x(i)`, `the$x` equals `i`.
#'
#' * **Outside `generate_x(i)`.** `for (x in generate_x(i))` pulls one value per iteration.
#'   Receiving a value means `generate_x(i)` has just suspended, so its teardown has
#'   *already run*. By the time the outer loop body executes, `the$x` is back to
#'   `0` — which is what `stopifnot(the$x == 0)` asserts. Nothing `generate_x(i)` did to
#'   `the$x` leaks out to the outer generator or its caller.
#'
#' ```r
#' the <- new.env()
#' the$x <- 0
#'
#' # Per-iteration setup/teardown lives in its own generator:
#' generate_x <- generator(function(i) {
#'   setup({
#'     the$x <- i                 # runs at the start of each of generate_x(i)'s steps
#'     withr::defer(the$x <- 0)   # runs at the end of each of generate_x(i)'s steps
#'   })
#'   yield(the$x)   # the$x == i
#'   yield(the$x)   # the$x == i again (setup re-ran for this step)
#' })
#'
#' # The outer generator delegates to generate_x(i) and re-yields its values:
#' gen <- generator(function() {
#'   for (i in 1:3) {
#'     for (x in generate_x(i)) {
#'       stopifnot(the$x == 0)  # 0 out here: generate_x(i)'s teardown ran when it suspended
#'       yield(x)
#'     }
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
#'
#' # `setup()` is handy for per-step hygiene: global state changed with a withr
#' # helper is restored at the end of each step, so it never leaks across steps
#' # or back to the caller. Here an option is scoped to each step:
#' gen <- generator(function() {
#'   setup(withr::local_options(coro.example = "step"))
#'   yield(getOption("coro.example", "unset"))
#'   yield(getOption("coro.example", "unset"))
#' })
#'
#' g <- gen()
#' getOption("coro.example", "unset")   # "unset"
#' g()                                  # "step" — option set for this step
#' getOption("coro.example", "unset")   # "unset" — restored at the step end
#' g()                                  # "step"
#' getOption("coro.example", "unset")   # "unset"
#' @export
setup <- function(expr) {
  abort("`setup()` can't be called directly or within function arguments.")
}
