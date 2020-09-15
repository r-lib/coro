
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
  expect_equal(async_internal_generator(fn1), async_internal_generator(fn2))

  fn1 <- async(function() while (1) if (2) await("value"))
  fn2 <- async(function() while (1) { if (2) { await("value") } })
  expect_equal(async_internal_generator(fn1), async_internal_generator(fn2))
})

test_that("async() takes anonymous functions", {
  fn <- function() await("value")
  expect_error(async(fn), "anonymous")
})

test_that("await() can't be called directly or out of place", {
  expect_error(await(1), "called directly")

  f <- async(function() list(await(1)))
  expect_error(f(), "within function arguments")
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

test_that("async_generator() creates streams", {
  produced <- NULL
  new_stream <- async_generator(function(x) {
    for (elt in x) {
      await(async_sleep(0))
      produced <<- c(produced, elt)
      yield(elt)
    }
  })

  s <- new_stream(1:3)
  out <- wait_for(s())
  expect_equal(out, 1L)

  consumed <- NULL
  async_obs <- async(function(i) {
    while (TRUE) {
      x <- await(i())
      consumed <<- c(consumed, x)

      if (is_null(x)) {
        return("done")
      }
    }
  })

  out <- wait_for(async_obs(s))
  expect_equal(out, "done")

  expect_equal(produced, 1:3)
  expect_equal(consumed, 2:3)
})

test_that("can adapt async streams", {
  new_stream <- async_generator(function(x) for (elt in x) yield(elt))

  consumed <- NULL
  async_obs <- async(function(i) {
    while (TRUE) {
      x <- await(i())
      consumed <<- c(consumed, x)

      if (is_null(x)) {
        return("done")
      }
    }
  })

  s1 <- new_stream(1:3)
  s2 <- async_adapt(s1, iter_map(`*`, 3L))
  wait_for(async_obs(s2))

  expect_identical(consumed, 1:3 * 3L)
})

test_that("async() functions can take dots", {
  fn <- async(function(...) list(...))
  expect_equal(wait_for(fn(x = 1)), list(x = 1))
})

