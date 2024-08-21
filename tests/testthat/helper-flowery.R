expect_snapshot0 <- function(expr, cran = TRUE) {
  skip_on_covr()
  inject(expect_snapshot(!!enquo0(expr), cran = cran))
}

expect_exhausted <- function(x) {
  expect_true(is_exhausted(x))
}

local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}
