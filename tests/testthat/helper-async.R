
async_generator <- function(fn) {
  body(fn) <- walk_blocks(fn_block(fn), poke_await)

  node <- set_returns(body(fn))
  arg <- "_resolved"

  generator_parts(node, arg = arg)
}

expect_async_snapshot <- function(fn) {
  blast(expect_snapshot(print(async_generator(!!enexpr(fn))), cran = TRUE), caller_env())
}
