#' Create a generator function
#'
#' @description
#'
#' `generator()` creates an generator factory. A generator is an
#' [iterator function][iterator] that can pause its execution with
#' [yield()] and resume from where it left off. Because they manage
#' state for you, generators are the easiest way to create
#' iterators. See `vignette("generator")`.
#'
#' The following rules apply:
#'
#' * Yielded values do not terminate the generator. If you call the
#'   generator again, the execution resumes right after the yielding
#'   point. All local variables are preserved.
#'
#' * Returned values terminate the generator. If called again after a
#'   `return()`, the generator keeps returning the [exhausted()]
#'   sentinel.
#'
#' Generators are compatible with all features based on the iterator
#' protocol such as [loop()] and [collect()].
#'
#' @param fn A function template for generators. The function can
#'   [yield()] values. Within a generator, `for` loops have
#'   [iterator] support.
#'
#' @seealso [yield()], [coro_debug()] for step-debugging.
#' @export
#' @examples
#' # A generator statement creates a generator factory. The
#' # following generator yields two times and then returns `"c"`:
#' generate_abc <- generator(function() {
#'   yield("a")
#'   yield("b")
#'   "c"
#' })
#'
#' # Or equivalently:
#' generate_abc <- generator(function() {
#'   for (x in letters[1:3]) {
#'     yield(x)
#'   }
#' })
#'
#' # The factory creates generator instances. They are iterators
#' # that you can call successively to obtain new values:
#' abc <- generate_abc()
#' abc()
#' abc()
#'
#' # Once a generator has returned it keeps returning `exhausted()`.
#' # This signals to its caller that new values can no longer be
#' # produced. The generator is exhausted:
#' abc()
#' abc()
#'
#' # You can only exhaust a generator once but you can always create
#' # new ones from a factory:
#' abc <- generate_abc()
#' abc()
#'
#'
#' # As generators implement the coro iteration protocol, you can use
#' # coro tools like `loop()`. It makes it possible to loop over
#' # iterators with `for` expressions:
#' loop(for (x in abc) print(x))
#'
#' # To gather values of an iterator in a list, use `collect()`. Pass
#' # the `n` argument to collect that number of elements from a
#' # generator:
#' abc <- generate_abc()
#' collect(abc, 1)
#'
#' # Or drain all remaining elements:
#' collect(abc)
#'
#'
#' # coro provides a short syntax `gen()` for creating one-off
#' # generator _instances_. It is handy to adapt existing iterators:
#' numbers <- 1:10
#' odds <- gen(for (x in numbers) if (x %% 2 != 0) yield(x))
#' squares <- gen(for (x in odds) yield(x^2))
#' greetings <- gen(for (x in squares) yield(paste("Hey", x)))
#'
#' collect(greetings)
#'
#'
#' # Arguments passed to generator instances are returned from the
#' # `yield()` statement on reentry:
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

  # Flipped when `coro_debug()` is applied on a generator factory
  debugged <- FALSE

  `_parent` <- environment()

  # Create the generator factory (returned by `generator()` and
  # entered by `async()`)
  factory <- new_function(fmls, quote({
    # Evaluate here so the formals of the generator factory do not
    # mask our variables
    `_private` <- rlang::env(`_parent`)
    `_private`$generator_env <- base::environment()
    `_private`$caller_env <- base::parent.frame()

    base::local(envir = `_private`, {
      info <- machine_info(type, env = caller_env)

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

      # The compiler caches function bodies, so inline a weak
      # reference to avoid leaks (#36)
      weak_env <- new_weakref(env)

      # Forward arguments inside the user space of the state machine
      lapply(names(fmls), function(arg) env_bind_arg(user_env, arg, frame = generator_env))

      # Flipped when `f` is pressed in the browser
      undebugged <- FALSE

      # Create the generator instance. This is a function that resumes
      # a state machine.
      instance <- inject(function(arg) {
        # Forward generator argument inside the state machine environment
        delayedAssign("arg", arg, assign.env = env)

        if (!undebugged && (debugged || is_true(peek_option("coro_debug")))) {
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
          # In case a scheduler calls back the generator for error
          # handling or cleanup
          if (!missing(arg)) {
            force(arg)
          }
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
          base::evalq(envir = rlang::wref_key(!!weak_env), {
            env_poke_exits(user_env, exits)
            !!state_machine
          })
        )
        env$jumped <- FALSE

        out
      })

      env$.self <- instance

      if (is_string(type, "async")) {
        # Step into the generator right away
        instance(NULL)
      } else {
        structure(instance, class = "coro_generator_instance")
      }
    })
  }))

  structure(factory, class = c(paste0("coro_", type), "function"))
}

# Creates a child of the coro namespace that holds all the variables
# used by the generator runtime
new_generator_env <- function(parent, info) {
  env <- env(ns_env("coro"))
  user_env <- env(parent, .__generator_instance__. = TRUE)

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
    env[["..."]] <- env_get(frame, "...", inherit = TRUE, default = missing_arg())
  } else {
    env_bind_lazy(env, !!arg := !!sym(arg), .eval_env = frame)
  }
}

#' @export
print.coro_generator <- function(x, ..., internals = FALSE) {
  writeLines("<generator>")
  print_generator(x, ..., internals = internals)
}
#' @export
print.coro_generator_instance <- function(x, ..., internals = FALSE) {
  type <- env_get(fn_env(x), "type", inherit = TRUE)

  if (is_string(type, "async_generator")) {
    writeLines("<async/generator/instance>")
  } else {
    writeLines("<generator/instance>")
  }

  print_generator(x, ..., internals = internals)
}

print_generator <- function(x, ..., internals = FALSE, reproducible = FALSE) {
  fn <- env_get(fn_env(x), "fn", inherit = TRUE)

  if (reproducible) {
    fn <- zap_env(fn)
  }

  print(fn, ...)

  if (internals) {
    print_state_machine(x, ...)
  }

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


#' Yield a value from a generator
#'
#' @description
#'
#' The `yield()` statement suspends [generator()] functions. It works
#' like `return()` except that the function continues execution at the
#' yielding point when it is called again.
#'
#' `yield()` can be called within loops and if-else branches but for
#' technical reasons it can't be used anywhere in R code:
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
#' * Call `coro_debug()` on a [generator()], [async()], or
#'   [async_generator()] function to enable step-debugging.
#'
#' * Alternatively, set `options(coro_debug = TRUE)` for
#' step-debugging through all functions created with coro.
#'
#' @param fn A generator factory or an async function.
#' @param value Whether to debug the function.
#'
#' @export
coro_debug <- function(fn, value = TRUE) {
  if (!is_generator_factory(fn)) {
    abort("`fn` must be a `generator()`, `async()`, or `async_generator()` function.")
  }

  env_poke(fn_env(fn), "debugged", value, create = FALSE)
}

is_generator_factory <- function(x) {
  inherits_any(x, c(
    "coro_generator",
    "coro_async",
    "coro_async_generator"
  ))
}

with_try_catch <- function(handlers, expr) {
  inject(tryCatch(expr, !!!handlers))
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
  "suspend",
  "generator_env"
))
