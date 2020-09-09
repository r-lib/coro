
test_that("async functions construct a generator", {
  expect_async_snapshot(function() "value")
  expect_async_snapshot(function() await("value"))
  expect_async_snapshot(function() if (1) await("value") else "else")
  expect_async_snapshot(function() while (1) if (2) await("value"))
  expect_async_snapshot(function() while (1) foo <- await("value"))
})

test_that("async functions are not sensitive to blocks", {
  fn1 <- function() await("value")
  fn2 <- function() { await("value") }
  expect_equal(async_generator(fn1), async_generator(fn2))

  fn1 <- function() while (1) if (2) await("value")
  fn2 <- function() while (1) { if (2) { await("value") } }
  expect_equal(async_generator(fn1), async_generator(fn2))
})
