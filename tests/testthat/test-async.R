test_that("async functions construct a generator", {
  expect_snapshot0(async_body(function() "value"))
  expect_snapshot0(async_body(function() await("value")))
  expect_snapshot0(async_body(function() if (1) await("value") else "else"))
  expect_snapshot0(async_body(function() while (1) if (2) await("value")))
  expect_snapshot0(async_body(function() while (1) foo <- await("value")))
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

      if (is_exhausted(x)) {
        return("done")
      }

      consumed <<- c(consumed, x)
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

      if (is_exhausted(x)) {
        return("done")
      }

      consumed <<- c(consumed, x)
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

test_that("async_collect() collects", {
  new_stream <- async_generator(function(x) for (elt in x) yield(elt))

  s1 <- new_stream(1:5)
  s2 <- async_adapt(s1, iter_map(`*`, 3))

  out <- wait_for(async_collect(s2))
  expect_equal(out, as.list(1:5 * 3))


  s1 <- new_stream(1:5)
  s2 <- async_adapt(s1, iter_map(`*`, 3))

  out <- wait_for(async_collect(s2, n = 3))
  expect_equal(out, as.list(1:3 * 3))
})

test_that("for loops support await_each()", {
  new_stream <- async_generator(function(x) for (elt in x) yield(elt))

  f <- async_generator(function(s) for (x in await_each(s)) yield(x * 2))
  out <- wait_for(async_collect(f(new_stream(1:3))))
  expect_equal(out, list(2, 4, 6))

  values <- NULL
  f <- async_generator(function(s1, s2) {
    for (x in await_each(s1)) {
      values <<- c(values, x)
      for (y in await_each(s2)) {
        values <<- c(values, y)
      }
    }
  })
  wait_for(async_collect(f(new_stream(1:3), new_stream(11:12))))
  expect_equal(values, c(1, 11, 12, 2, 3))

  expect_snapshot0(async_body(function(s1, s2) {
    for (x in await_each(s1)) {
      values <<- c(values, x)
      for (y in await_each(s2)) {
        values <<- c(values, y)
      }
    }
  }))
})

test_that("await_each() can't be used in generators", {
  expect_error(generator(function() for (x in await_each(i)) NULL)()(), "non-async generator")
})

test_that("yield() can't be used in async() functions", {
  expect_error(async(function() yield(1))(), "Can't")
})

test_that("yield() inside `async_generator()` returns a promise", {
  new_g <- async_generator(function() yield(1))
  expect_true(inherits(new_g()(), "promise"))
})

test_that("async functions handle errors", {
  async_fail <- async(function() {
    await(async_sleep(0))
    stop("some error")
  })
  g <- async(function() {
    await(async_fail())
    "value"
  })
  expect_error(
    wait_for(g()),
    "some error"
  )

  g <- async(function() {
    tryCatch(await(async_fail()), error = function(...) "handled")
    "value"
  })
  expect_equal(
    wait_for(g()),
    "value"
  )
})

test_that("async functions and async generator factories print nicely", {
  fn <- async(function() await(NULL))
  expect_snapshot(print(fn, reproducible = TRUE))
  expect_snapshot(print(fn, internals = TRUE, reproducible = TRUE))

  factory <- async_generator(function() { await(NULL); yield(NULL) })
  expect_snapshot(print(factory, reproducible = TRUE))
  expect_snapshot(print(factory, internals = TRUE, reproducible = TRUE))

  instance <- factory()
  expect_snapshot(print(instance, reproducible = TRUE))
  expect_snapshot(print(instance, internals = TRUE, reproducible = TRUE))
})

test_that("trailing await() returns awaited value", {
  input <- function() async_sleep(0)$then(function(...) "output")

  fn <- async(function() await(input()))
  expect_equal(wait_for(fn()), "output")

  fn <- async(function() x <- await(input()))
  expect_equal(wait_for(fn()), "output")

  fn <- async(function() x <- tryCatch(await(input())))
  expect_equal(wait_for(fn()), "output")

  fn <- async(function() x <- tryCatch(if (TRUE) await(input())))
  expect_equal(wait_for(fn()), "output")

  fn <- async(function() {
    x <- tryCatch(await(input()))
    x
  })
  expect_equal(wait_for(fn()), "output")
})

test_that("can tryCatch() r-promises to async-promises", {
  async_fail <- async(function() {
    async_sleep(0)
    abort("foo")
  })
  async_catch <- async(function(expr) {
    tryCatch(await(expr), error = identity)
  })

  out <- wait_for(
    async_catch(async_fail())
  )
  expect_true(inherits(out, "error"))
  expect_equal(out$message, "foo")
})

test_that("async ops are picked up from caller env", {
  fn <- async(function() await(TRUE))

  local({
    .__coro_async_ops__. <- coro::async_ops(
      package = "coro",
      then = function(...) signal("then", "found"),
      as_promise = function(...) signal("as_promise", "found")
    )

    expect_condition(
      fn(),
      "then",
      class = "found"
    )
  })
})

test_that("can await-assign with `=` (#29)", {
  fn <- async(function() {
    x = tryCatch(await(1))
    x + 1
  })
  expect_equal(wait_for(fn()), 2)
})
