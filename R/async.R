#' Make an async function
#'
#' `async()` functions are the building blocks of cooperative
#' concurrency. They suspend themselves with `await()` when they
#' expect an operation to take a long time to complete. While they
#' wait for the result, other async functions can be resumed once they
#' are ready to make progress.
#'
#' @param fn An anonymous function within which `await()` calls are
#'   allowed.
#' @return A function that returns a [promises::promise()].
#'
#' @section Concurrency framework:
#'
#' The default scheduler used by `async()` functions is provided by
#' the _later_ package. It currently only supports timers but these
#' are sufficient to implement basic cooperative concurrency. Since
#' this scheduler normally runs at top level, the async functions are
#' called back once all current computations have finished running.
#'
#' `async()` functions can be chained to promises from the _promises_
#' package.
#'
#' @export
async <- function(fn) {
  assert_lambda(substitute(fn))
  fn <- ensure_promises(fn)
  new_async_generator(fn, step = TRUE)
}
#' @rdname async
#' @param x An awaitable value, i.e. a [promise][promises::promise].
#' @export
await <- function(x) {
  abort("`await()` can't be called directly or within function arguments.")
}

#' Construct an async generator
#'
#' @param fn An anonymous function describing an async generator
#'   within which `await()` calls are allowed.
#' @return A generator factory. Generators constructed with this
#'   factory always return [promises::promise()].
#'
#' @export
async_generator <- function(fn) {
  assert_lambda(substitute(fn))
  fn <- ensure_promises(fn)
  new_async_generator(fn, step = FALSE)
}

ensure_promises <- function(fn) {
  body(fn) <- expr({
    if (!rlang::is_installed(c("promises", "later"))) {
      rlang::abort("The {later} and {promises} packages must be installed.")
    }

    !!!fn_body(fn)
  })

  fn
}

#' Low-level constructor for async functions
#'
#' Unlike [async()] which uses concurrency based on
#' [promises](https://rstudio.github.io/promises/),
#' `new_async_generator()` allows constructing async-await functions
#' and async generators for other concurrency frameworks.
#'
#' @param step If `TRUE`, the async generator is immediately stepped
#'   in after creation. This returns a promise. If `FALSE`, the async
#'   generator is returned instead. Set `step` to `TRUE` when you
#'   create an [async()] variant and to `FALSE` when you create an
#'   [async_generator()] variant.
#'
#' @keywords internal
#' @export
new_async_generator <- function(fn, step, ops = NULL) {
  body <- fn_block(fn)

  # We make three extra passes for convenience. This will be changed
  # to a single pass later on.
  body <- walk_blocks(body, poke_await)
  body <- new_call(quote(`{`), set_returns(body))
  body <- walk_blocks(body, poke_async_return)

  info <- gen0_list(body, fn_env(fn))
  `_env` <- info$env

  ops <- ops %||% list(
    `_then` = function(x, callback) promises::then(x, onFulfilled = callback),
    `_as_promise` = function(x) as_promise(x)
  )
  env_bind(`_env`, !!!ops)

  fmls <- formals(fn)

  out <- new_function(fmls, expr({
    # Refresh the state machine environment
    `_env` <- env_clone(`_env`)

    # Forward arguments inside the state machine environment
    !!!forward_args_calls(fmls)

    # Create function around the state machine
    generator <- function(`_next_arg` = NULL) !!info$expr

    # Bind generator to `_self`. This binding can be hooked as callback.
    env_bind(`_env`, `_self` = generator)

    if (step) {
      generator(NULL)
    } else {
      generator
    }
  }))

  if (step) {
    structure(out, class = c("flowery_async", "function"))
  } else {
    structure(out, class = c("flowery_generator", "function"))
  }
}

#' @export
print.flowery_async <- function(x, ...) {
  # TODO: Print user-friendly async function by default. Make it
  # configurable for easier development.

  writeLines("<async>")
  print(unclass(x), ...)

  writeLines("State machine:")
  print(async_internal_generator(x), ...)

  invisible(x)
}

async_internal_generator <- function(fn) {
  env_get(fn_env(fn), "info")$expr
}

poke_await <- function(node) {
  car <- node_car(node)

  if (is_await(car)) {
    node_poke_car(node, yield_await_call(await_arg(car)))
    return()
  }

  if (is_call(car, "<-")) {
    rhs_node <- node_cddr(car)
    rhs <- node_car(rhs_node)
    if (is_await(rhs)) {
      lhs <- node_cadr(car)
      await_arg <- node_cadr(rhs)
      new_rhs <- call("<-", lhs, yield_await_call(await_arg))
      node_poke_car(node, new_rhs)
    }
    return()
  }
}

poke_async_return <- function(node) {
  car <- node_car(node)

  if (is_coro_return_call(car)) {
    node_poke_car(node, async_return_call(node_cadr(car)))
  }

  if (is_coro_yield_call(car)) {
    stop("TODO")
  }
}

is_await <- function(expr) {
  is_call(expr, "await", ns = c("", "flowery"))
}
await_arg <- function(call) {
  node_cadr(call)
}

yield_await_call <- function(arg) {
  expr(yield(`_then`(`_as_promise`(!!arg), callback = `_self`)))
}
async_return_call <- function(arg) {
  expr(flowery::coro_return(`_as_promise`(!!arg)))
}

as_promise <- function(x) {
  if (promises::is.promise(x)) {
    x
  } else {
    promises::promise_resolve(x)
  }
}

utils::globalVariables(c("_self", "_as_promise", "_then"))
