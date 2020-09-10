
async_generator <- function(fn) {
  block <- walk_blocks(fn_block(fn), poke_await)
  generator_parts(set_returns(block), arg = "_resolved")
}

expect_async_snapshot <- function(fn) {
  blast(expect_snapshot(print(async_generator(!!enexpr(fn))), cran = TRUE), caller_env())
}
