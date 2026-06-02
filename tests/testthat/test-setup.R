test_that("setup() can't be called directly or within function arguments", {
  expect_error(setup(1), "called directly")

  f <- generator(function() {
    list(setup(1))
    yield(2)
  })
  expect_error(f()(), "within function arguments")
})

test_that("setup() runs before every step; teardown fires at each step end", {
  the <- new.env()
  the$x <- 0
  log <- character()

  gen <- generator(function() {
    setup({
      old <- the$x
      the$x <- 9
      withr::defer(the$x <- old)
    })
    log <<- c(log, paste0("before1:", the$x))
    yield(1)
    log <<- c(log, paste0("before2:", the$x))
    yield(2)
  })
  g <- gen()

  expect_equal(g(), 1)               # step 1 ran setup, yielded 1
  expect_equal(the$x, 0)             # step 1 teardown restored x
  expect_equal(g(), 2)               # step 2 re-ran setup, yielded 2
  expect_equal(the$x, 0)             # step 2 teardown restored x
  expect_exhausted(g())              # step 3 (after yield 2) ran setup + teardown
  expect_equal(the$x, 0)
  expect_equal(log, c("before1:9", "before2:9"))
})

test_that("setup() teardown is restored around await() (issue #68 reprex)", {
  skip_on_cran()
  the <- new.env()
  the$x <- 0
  seen <- list()

  f <- async(function(x) {
    setup({
      old_x <- the$x
      the$x <- x
      withr::defer(the$x <- old_x)
    })
    seen$before <<- c(seen$before, the$x)
    await(async_sleep(0))
    seen$after <<- c(seen$after, the$x)
    the$x
  })

  out <- wait_for(f(1))
  expect_equal(out, 1)
  expect_equal(seen$before, 1)
  expect_equal(seen$after, 1)
  expect_equal(the$x, 0)
})

test_that("multiple setup() calls stack; teardowns fire in reverse order", {
  log <- character()

  gen <- generator(function() {
    setup(withr::defer(log <<- c(log, "teardown-A")))
    setup(withr::defer(log <<- c(log, "teardown-B")))
    log <<- c(log, "body")
    yield(1)
  })
  g <- gen()

  expect_equal(g(), 1)
  expect_equal(log, c("body", "teardown-B", "teardown-A"))
})
