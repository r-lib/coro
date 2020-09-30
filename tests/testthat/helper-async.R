
async_state_machine <- function(fn) {
  gen <- blast(async_generator(async(!!enexpr(fn))))

  expr <- gen[[3]][[3]][[2]][[3]][[2]][-(1:2)]
  machine <- as.pairlist(as.list(expr))

  machine
}

expect_async_snapshot <- function(fn) {
  blast(expect_snapshot(async_state_machine(!!enexpr(fn)), cran = TRUE), caller_env())
}

expect_promise <- function(x, value, status = NULL) {
  expect_true(inherits(x, "promise"))

  if (!missing(value)) {
    expect_equal(prom_value(x), value)
  }
  if (!is_null(status)) {
    expect_equal(prom_status(x), status)
  }
}
