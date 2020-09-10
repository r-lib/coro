
#' @export
async <- function(fn) {
  body(fn) <- walk_blocks(fn_block(fn), poke_await)

  new_function(formals(fn), expr({
    fmls <- pairlist2(`_resolved` = )
    info <- gen0_list(body(fn), environment(), fmls)
    gen <- new_function(fmls, info$expr)

    # Wrap generator so it always returns a promise
    `_async_generator` <- function(x) as_promise(gen(x))

    # environment() contains initial arguments
    `_resolved` <- NULL
    as_promise(eval(info$expr))
  }))
}

poke_await <- function(node) {
  car <- node_car(node)

  if (is_await(car)) {
    arg <- node_cadr(car)
    node_poke_car(node, yield_await_call(arg))
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

is_await <- function(expr) {
  is_call(expr, "await", ns = c("", "flowery"))
}
yield_await_call <- function(arg) {
  expr(yield(flowery::coro_await(!!arg, `_async_generator`)))
}

as_promise <- function(x) {
  if (promises::is.promise(x)) {
    x
  } else {
    promises::promise_resolve(x)
  }
}
