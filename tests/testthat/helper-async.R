expect_promise <- function(x, value, status = NULL) {
  expect_true(inherits(x, "promise"))

  if (!missing(value)) {
    expect_equal(prom_value(x), value)
  }
  if (!is_null(status)) {
    expect_equal(prom_status(x), status)
  }
}
