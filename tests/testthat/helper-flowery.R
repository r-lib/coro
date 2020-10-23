
expect_snapshot0 <- function(expr, cran = TRUE) {
  # Work around for `{{` blocks
  quo <- new_quosure(substitute(expr), caller_env())
  expect_snapshot(!!quo, cran = cran)
}

expect_exhausted <- function(x) {
  expect_true(is_exhausted(x))
}
