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

  info <- gen0_list(body(fn), fn_env(fn))
  `_env` <- info$env

  fmls <- formals(fn)

  out <- new_function(fmls, quote({
    # Refresh the state machine environment
    `_env` <- env_clone(`_env`)

    # Forward arguments inside the state machine environment
    frame <- environment()
    lapply(names(fmls), function(arg) env_bind_arg(`_env`, arg, frame = frame))

    # Create function around the state machine
    gen <- blast(function(`_next_arg` = NULL) !!info$expr)

    # Zap source references so we can see the state machine
    unstructure(gen)
  }))

  structure(out, class = c("flowery_generator", "function"))
}

#' @export
print.flowery_generator <- function(x, ...) {
  writeLines("<generator>")
  print(unstructure(x))

  writeLines("State machine:")
  print(env_get(fn_env(x), "info")$expr)

  invisible(x)
}

env_bind_arg <- function(env, arg, frame = caller_env()) {
  if (identical(arg, "...")) {
    env$... <- env_get(frame, "...", inherit = TRUE, default = missing_arg())
  } else {
    env_bind_lazy(env, !!arg := !!sym(arg), .eval_env = frame)
  }
}

gen0 <- function(expr, env) {
  info <- gen0_list(expr, env)
  `_env` <- info$env

  out <- new_function(pairlist2(`_next_arg` = NULL), info$expr)

  # Zap source references so you can see the state machine
  unstructure(out)
}

gen0_list <- function(expr, env) {
  node <- set_returns(expr)
  parts <- generator_parts(node)

  # Add a late return point
  return_call <- call2(quote(base::return), quote(invisible(NULL)))
  parts <- node_list_poke_cdr(parts, pairlist(block(return_call)))

  # Create the persistent closure environment of the generator
  env <- env(env,
    `_state` = "1",
    `_return_state` = as.character(length(parts))
  )

  expr <- expr({
    # Define value sent into the generator inside the state machine.
    # TODO: This should happen lazily inside the relevant state. This
    # way, interrupts and errors can be sent into the generator for
    # clean up.
    `_env`$`_next_arg` <- `_next_arg`

    # Evaluate in the persistent environment
    evalq(`_env`, expr = {
      while (TRUE) {
        !!machine_switch_call(parts)
      }
    })
  })

  list(expr = expr, env = env)
}

generator_parts <- function(node, arg = NULL) {
  reset_state()
  if (!is_null(arg)) {
    poke_state_elt("arg_sym", sym(arg))
  }

  parts <- node_list_parts(node)

  if (is_null(parts)) {
    pairlist(new_call(block_sym, node))
  } else {
    parts
  }
}

#' @rdname generator
#' @param expr A yielding expression.
#' @export
gen <- function(expr) {
  gen0(substitute(expr), env = caller_env())
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
