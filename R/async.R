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
  new_async_generator(fn, step = TRUE)
}
#' @rdname async
#' @param x An awaitable value, i.e. a [promise][promises::promise].
#' @export
await <- function(x) {
  abort("`await()` can't be called directly or within function arguments.")
}
#' @rdname async
#' @export
await_each <- function(x) {
  abort("`await_each()` must be called within a `for` loop.")
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
  new_async_generator(fn, step = FALSE)
}

# Customisation point for the {async} package or any concurrency
# framework that defines a "then" operation
flowery_ops <- function(env) {
  ops <- env_get(env, ".__flowery_async_ops__.", inherit = TRUE, default = NULL)

  ops %||% list(
    package = "promises",
    `_then` = function(x, callback) promises::then(x, onFulfilled = callback),
    `_as_promise` = function(x) as_promise(x)
  )
}

ensure_promises <- function(fn, package) {
  stopifnot(is_string(package))

  body(fn) <- expr({

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
new_async_generator <- function(fn, step) {
  body <- duplicate(fn_block(fn), shallow = TRUE)

  # We make three extra passes for convenience. This will be changed
  # to a single pass later on.
  walk_poke_await(node_cdr(body), allow_yield = !step)
  body <- new_call(quote(`{`), set_returns(body))
  walk_blocks(node_cdr(body), poke_async_return)

  info <- gen0_list(body, fn_env(fn))
  `_env` <- info$env

  fmls <- formals(fn)

  out <- new_function(fmls, quote({
    # Refresh the state machine environment
    `_env` <- env_clone(`_env`)

    # Look up lexically defined async operations
    ops <- flowery_ops(caller_env())

    if (!is_installed(ops$package)) {
      abort(sprintf("The %s package must be installed.", ops$package))
    }

    # Define async operations in the state machine environment
    env_bind(`_env`, !!!ops)

    # Forward arguments inside the state machine environment
    frame <- environment()
    lapply(names(fmls), function(arg) env_bind_arg(`_env`, arg, frame = frame))

    # Create function around the state machine
    generator <- blast(function(`_next_arg` = NULL) !!info$expr)

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

walk_poke_await <- function(node, allow_yield) {
  poke <- function(node, type = NULL) poke_await(node, type = type, allow_yield = allow_yield)
  walk_blocks(node, poke, which = c("expr", "for"))
}

poke_await <- function(node, type, allow_yield) {
  if (identical(type, "for")) {
    return(poke_async_for(node, allow_yield))
  }

  car <- node_car(node)

  # With `yield()` the caller is responsible for calling the generator back
  if (is_yield_call(car)) {
    if (!allow_yield) {
      abort("Can't use `yield()` within an `async()` function.")
    }
    node_poke_car(node, async_yield_call(yield_arg(car)))
    return()
  }

  # With `await()` the async scheduler calls the generator back
  if (is_await_call(car)) {
    node_poke_car(node, async_yield_await_call(await_arg(car)))
    return()
  }

  if (is_call(car, "<-")) {
    rhs_node <- node_cddr(car)
    rhs <- node_car(rhs_node)
    if (is_await_call(rhs)) {
      lhs <- node_cadr(car)
      await_arg <- node_cadr(rhs)
      new_rhs <- call("<-", lhs, async_yield_await_call(await_arg))
      node_poke_car(node, new_rhs)
    }
    return()
  }
}

poke_async_for <- function(node, allow_yield) {
  expr <- node_car(node)
  var_node <- node_cdr(expr)
  iter_node <- node_cdr(var_node)
  body_node <- node_cdr(iter_node)

  walk_poke_await(body_node, allow_yield)

  iter <- node_car(iter_node)
  if (is_call(iter, "await_each", ns = c("", "flowery"))) {
    var <- node_car(var_node)
    await_loop <- new_await_loop_call(var, node_cadr(iter), node_car(body_node))
    node_poke_car(node, await_loop)
  }

  NULL
}

new_await_loop_call <- function(var, iterable, block) {
  if (!is_symbol(iterable)) {
    abort("Can't supply a complex expression to `await_each()`.")
  }

  expr(repeat {
    !!var <- !!async_yield_await_call(call2(iterable))
    if (base::is.null(!!var)) {
      break
    }
    !!!as_block(block)
  })
}

poke_async_return <- function(node) {
  car <- node_car(node)

  if (is_coro_return_call(car)) {
    node_poke_car(node, async_return_call(node_cadr(car)))
  }
}

is_await_call <- function(expr) {
  is_call(expr, "await", ns = c("", "flowery"))
}
await_arg <- function(call) {
  node_cadr(call)
}
yield_arg <- function(call) {
  node_cadr(call)
}

async_yield_call <- function(arg) {
  expr(yield(`_as_promise`(!!arg)))
}
async_yield_await_call <- function(arg) {
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
