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
