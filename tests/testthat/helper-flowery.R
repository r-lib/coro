# Work around for `{{` blocks
# Branching is for compatibility with r-lib/testthat#1492
if ("enquo0" %in% all.names(body(expect_snapshot))) {
  expect_snapshot0 <- function(expr, cran = TRUE) {
    inject(
      expect_snapshot(!!enquo0(expr), cran = cran)
    )
  }
} else {
  expect_snapshot0 <- function(expr, cran = TRUE) {
    expect_snapshot(!!enquo0(expr), cran = cran)
  }
}

expect_exhausted <- function(x) {
  expect_true(is_exhausted(x))
}

local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}
