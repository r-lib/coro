
expect_snapshot0 <- function(expr, cran = TRUE) {
  # Work around for `{{` blocks
  quo <- new_quosure(substitute(expr), caller_env())
  expect_snapshot(!!quo, cran = cran)
}

# FIXME - spliceable attribute doesn't show up on CI
skip_spliceable_attribute <- function() {
  skip_on_ci()
  skip_on_cran()
}
