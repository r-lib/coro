
test_that("async functions construct a generator", {
  expect_async_snapshot(function() "value")
  expect_async_snapshot(function() await("value"))
  expect_async_snapshot(function() if (1) await("value") else "else")
  expect_async_snapshot(function() while (1) if (2) await("value"))
  expect_async_snapshot(function() while (1) foo <- await("value"))
})

test_that("async functions are not sensitive to blocks", {
  fn1 <- async(function() await("value"))
  fn2 <- async(function() { await("value") })
  expect_equal(async_generator(fn1), async_generator(fn2))

  fn1 <- async(function() while (1) if (2) await("value"))
  fn2 <- async(function() while (1) { if (2) { await("value") } })
  expect_equal(async_generator(fn1), async_generator(fn2))
})

test_that("async() takes anonymous functions", {
  fn <- function() await("value")
  expect_error(async(fn), "anonymous")
})


# -------------------------------------------------------------------------
# The following tests rely on promises internals and so should be
# skipped on CRAN in case of breaking changes
skip_on_cran()

test_that("async functions inherit from environment", {
  fn <- local({
    foobar <- "value"
    async(function() foobar)
  })
  expect_equal(prom_value(fn()), "value")
})

test_that("async functions wrap return and yielded values", {
  later::with_temp_loop({
    out <- async(function() "value")()
    expect_promise(out, "value", "fulfilled")

    out <- async(function() await("value"))()
    expect_promise(out, status = "pending")
  })
})

test_that("await() yields", {
  out <- NULL
  fn <- async(function() {
    for (i in 1:3) {
      out <<- c(out, i)
      await(i)
    }
  })
  wait_for(fn())
  expect_equal(out, 1:3)
})

test_that("state of async() functions are independent", {
  fn <- async(function() "value")

  async_state <- function(fn) {
    machine_state_env <- env_get(fn_env(fn), "_env", inherit = TRUE)
    machine_state_env$`_state`
  }

  expect_equal(async_state(fn), "1")
  fn()
  expect_equal(async_state(fn), "1")
})
