#' Create a generator function
#'
#' @description
#'
#' `generator()` creates a factory function for generators. A
#' generator is an [iterator function][iterator] that can pause its
#' execution with [yield()] and resume from where it left off. Because
#' they manage state for you, generators are the easiest way to create
#' iterators. The following rules apply:
#'
#' * Yielded values do not terminate the generator. If you call the
#'   generator again, the execution resumes right after the yielding
#'   point. All local variables are preserved.
#'
#' * Returned values terminate the generator. If called again after a
#'   `return()`, the generator keeps returning `NULL`.
#'
#' Generators are compatible with all iterator features such as
#' [iterate()], [iter_adapt()], or [drain()].
#'
#' @param fn A function of zero or one argument to be transformed into
#'   a generator function that can [yield()] and `return()` values.
#'   Within a generator, `for` loops have [iterator] support.
#'
#' @section Passing arguments to a generator:
#'
#' You can create generator functions that take one argument. The
#' first time the generator is called, the argument is defined in the
#' suspendable function. On subsequent invokations, the argument is
#' returned from `yield()`.
#'
#' @export
#' @examples
#' # A generator statement creates a generator constructor:
#' new_gen <- generator(function() {
#'   yield("foo")
#'   yield("bar")
#'   "baz"
#' })
#'
#' # The constructor creates generator functions. They are essentially
#' # iterators that you can call successively to obtain values from:
#' iter <- new_gen()
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
#' # You can only exhaust a generator once but you can always create
#' # new ones from a factory:
#' iter <- new_gen()
#' iter()
#'
#' # As generators are regular iterators, you can use all iterator
#' # tools such as iterate() which allows you to loop over all values
#' # with a `for` loop:
#' iterate(for (x in iter) cat(x, "\n"))
#'
#'
#' # flowery provides a short syntax `gen()` for creating one-off
#' # generator functions. It is handy to chain iterators:
#' numbers <- 1:10
#' odds <- gen(for (x in numbers) if (x %% 2 != 0) yield(x))
#' squares <- gen(for (x in odds) yield(x^2))
#' greetings <- gen(for (x in squares) yield(paste("Hey", x)))
#'
#' # As with all iterators, you can take() elements from a generator:
#' take(greetings, 2)
#'
#' # Or drain the remaining elements:
#' drain(greetings)
#'
#'
#' # You can supply arguments to generator functions. They are
#' # returned from `yield()`.
#' new_tally <- generator(function() {
#'   count <- 0
#'   while (TRUE) {
#'     i <- yield(count)
#'     count <- count + i
#'   }
#' })
#' tally <- new_tally()
#' tally(1)
#' tally(2)
#' tally(10)
generator <- function(fn) {
  assert_lambda(substitute(fn))
  generator0(fn)
}
#' @rdname generator
#' @param expr A yielding expression.
#' @export
gen <- function(expr) {
  fn <- new_function(NULL, substitute(expr), caller_env())
  generator0(fn)()
}
generator0 <- function(fn) {
  state_machine <- NULL
  fmls <- formals(fn)
  env <- environment(fn)

  out <- new_function(fmls, quote({
    # Generate the state machine lazily at runtime
    if (is_null(state_machine)) {
      state_machine <<- walk_states(body(fn))
    }

    env <- new_generator_env(env)
    user_env <- env$user_env

    # Forward arguments inside the user space of the state machine
    frame <- environment()
    lapply(names(fmls), function(arg) env_bind_arg(user_env, arg, frame = frame))

    # Create function around the state machine
    gen <- blast(function(arg = NULL) {
      # Forward generator argument inside the state machine environment
      delayedAssign("arg", arg, assign.env = env)

      # Resume state machine
      evalq(envir = env, !!state_machine)
    })

    # Zap source references so we can see the state machine
    unstructure(gen)
  }))

  structure(out, class = c("flowery_generator", "function"))
}

#' @export
print.flowery_generator <- function(x, ...) {
  writeLines("<generator>")
  print(unstructure(x))

  machine <- with(
    fn_env(x),
    state_machine %||% walk_states(body(fn))
  )

  writeLines("State machine:")
  print(machine)

  invisible(x)
}

new_generator_env <- function(parent) {
  env <- env(ns_env("flowery"))
  user_env <- env(parent)

  env$user_env <- user_env
  env$exhausted <- FALSE
  env$state <- 1L
  env$iterators <- list()

  with(env, {
    .last_value <- NULL

    user <- function(expr) {
      .last_value <<- eval_bare(substitute(expr), user_env)
    }
    last_value <- function() {
      .last_value
    }
  })

  env
}

env_bind_arg <- function(env, arg, frame = caller_env()) {
  if (identical(arg, "...")) {
    env$... <- env_get(frame, "...", inherit = TRUE, default = missing_arg())
  } else {
    env_bind_lazy(env, !!arg := !!sym(arg), .eval_env = frame)
  }
}

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

# Currently a no-op but will disable exit expressions in the future
suspend <- function() NULL
