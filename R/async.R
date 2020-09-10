
#' @export
async <- function(fn) {
  call <- substitute(fn)
  if (!is_call(call, "function")) {
    abort("`fn` must be an anonymous function.")
  }
  new_async(fn)
}

#' @export
new_async <- function(fn, ops = NULL) {
  body <- fn_block(fn)

  # We make three extra passes for convenience. This will be changed
  # to a single pass later on.
  body <- walk_blocks(body, poke_await)
  body <- new_call(quote(`{`), set_returns(body))
  body <- walk_blocks(body, poke_async_return)

  gen_fmls <- pairlist2(`_resolved` = )
  info <- gen0_list(body, fn_env(fn), gen_fmls)

  `_env` <- info$env

  ops <- ops %||% list(
    `_then` = function(x, callback) promises::then(x, onFulfilled = callback),
    `_as_promise` = function(x) as_promise(x)
  )
  env_bind(`_env`, !!!ops)

  fmls <- formals(fn)

  forward_args_calls <- lapply(names(fmls), function(nm) {
    if (identical(nm, "...")) {
      quote(delayedAssign("...", get("..."), assign.env = `_env`))
    } else {
      expr(delayedAssign(!!nm, !!sym(nm), assign.env = `_env`))
    }
  })

  out <- new_function(fmls, expr({
    # Refresh the state machine environment
    `_env` <- env_clone(`_env`)

    !!!forward_args_calls

    gen <- new_function(gen_fmls, info$expr)
    env_bind(`_env`, `_self` = gen)

    gen(NULL)
  }))

  structure(out, class = c("flowery_async", "function"))
}

#' @export
print.flowery_async <- function(x, ...) {
  # TODO: Print user-friendly async function by default. Make it
  # configurable for easier development.

  writeLines("<async>")
  print(unclass(x), ...)

  writeLines("State machine:")
  print(async_generator(x), ...)

  invisible(x)
}

async_generator <- function(fn) {
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
