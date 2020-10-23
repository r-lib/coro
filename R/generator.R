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
#'   `return()`, the generator keeps returning the [exhausted()]
#'   sentinel.
#'
#' Generators are compatible with all iterator features such as
#' [iterate()], or [drain()].
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
#' @seealso [flowery_debug()] for step-debugging.
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
#' # Once a generator has returned it keeps returning `exhausted()`.
#' # This signals to its caller that new values can no longer be
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
generator0 <- function(fn, type = "generator") {
  state_machine <- NULL
  fmls <- formals(fn)
  env <- environment(fn)

  # Flipped when `flowery_debug()` is applied on a generator factory
  debugged <- FALSE

  # Create the generator factory (returned by `generator()` and
  # entered by `async()`)
  out <- new_function(fmls, quote({
    info <- machine_info(type, env = caller_env())

    # Generate the state machine lazily at runtime
    if (is_null(state_machine)) {
      state_machine <<- walk_states(body(fn), info = info)
    }

    ops <- info$async_ops
    if (!is_null(ops) && !is_installed(ops$package)) {
      abort(sprintf("The %s package must be installed.", ops$package))
    }

    env <- new_generator_env(env, info)
    user_env <- env$user_env

    # Forward arguments inside the user space of the state machine
    frame <- environment()
    lapply(names(fmls), function(arg) env_bind_arg(user_env, arg, frame = frame))

    # Flipped when `f` is pressed in the browser
    undebugged <- FALSE

    # Create the generator. This is a function that resumes a state machine.
    gen <- blast(function(arg = NULL) {
      # Forward generator argument inside the state machine environment
      delayedAssign("arg", arg, assign.env = env)

      if (!undebugged && (debugged || is_true(peek_option("flowery_debug")))) {
        env_browse(user_env)

        on.exit({
          # `f` was pressed, disable debugging for this generator
          if (!env_is_browsed(user_env)) {
            undebugged <<- TRUE
          }
        }, add = TRUE)
      }

      # Disable generator on error, interrupt, debugger quit, etc.
      # There is no safe way of resuming a generator that didn't
      # suspend normally.
      if (is_true(env$jumped)) {
        abort("This function has been disabled because of an unexpected exit.")
      }

      if (is_true(env$exhausted)) {
        return(exhausted())
      }

      # Resume state machine. Set up an execution env in the user
      # environment first to serve as a target for on.exit()
      # expressions. Then evaluate state machine in its private
      # environment.
      env$jumped <- TRUE
      out <- evalq(envir = user_env,
        base::evalq(envir = !!env, {
          env_poke_exits(user_env, exits)
          !!state_machine
        })
      )
      env$jumped <- FALSE

      out
    })

    env$.self <- gen

    if (is_string(type, "async")) {
      # Step into the generator right away
      gen(NULL)
    } else {
      # Zap source references so we can see the state machine
      unstructure(gen)
    }
  }))

  structure(out, class = c(paste0("flowery_", type), "function"))
}

#' @export
print.flowery_generator <- function(x, ...) {
  writeLines("<generator>")
  print(unstructure(x), ...)

  print_state_machine(x, ...)

  invisible(x)
}

print_state_machine <- function(x, ...) {
  machine <- with(env(fn_env(x)), {
    info <- machine_info(type, env = global_env())
    state_machine %||% walk_states(body(fn), info = info)
  })

  writeLines("State machine:")
  print(machine, ...)
}

new_generator_env <- function(parent, info) {
  env <- env(ns_env("flowery"))
  user_env <- env(parent)

  env$user_env <- user_env
  env$exhausted <- FALSE
  env$state <- 1L
  env$iterators <- list()
  env$handlers <- list()
  env$exits <- NULL
  env$.last_value <- NULL

  with(env, {
    user <- function(expr) {
      .last_value <<- eval_bare(substitute(expr), user_env)
    }
    last_value <- function() {
      .last_value
    }

    suspend <- function() {
      exits <<- env_poke_exits(user_env, NULL)
    }
  })

  if (!is_null(info$async_ops)) {
    env$then <- info$async_ops$then
    env$as_promise <- info$async_ops$as_promise
  }

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

#' Debug a generator or async function
#'
#' @description
#'
#' * Call `flowery_debug()` on a [generator()], [async()], or
#'   [async_generator()] function to enable step-debugging.
#'
#' * Alternatively, set `options(flowery_debug = TRUE)` for
#' step-debugging through all functions created with flowery.
#'
#' @param fn A generator factory or an async function created by
#'   flowery.
#' @param value Whether to debug the function.
#'
#' @export
flowery_debug <- function(fn, value = TRUE) {
  if (!is_generator_factory(fn)) {
    abort("`fn` must be a `generator()`, `async()`, or `async_generator()` function.")
  }

  env_poke(fn_env(fn), "debugged", value, create = FALSE)
}

is_generator_factory <- function(x) {
  inherits_any(x, c(
    "flowery_generator",
    "flowery_async",
    "flowery_async_generator"
  ))
}

with_try_catch <- function(handlers, expr) {
  blast(tryCatch(expr, !!!handlers))
}

utils::globalVariables(c(
  "last_value",
  "state",
  "arg",
  ".self",
  "then",
  "as_promise",
  "user",
  "exits",
  "suspend"
))
