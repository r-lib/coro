test_that("can loop() with iterators", {
  iter <- as_iterator(1:3)
  out <- int()

  loop(for (x in iter) {
    out <- c(out, x)
  })
  expect_identical(out, 1:3)
})

test_that("can use break within loop()", {
  out <- int()
  bool <- FALSE

  loop(for (x in 1:3) {
    out <- c(out, x)
    if (bool) {
      break
    }
    bool <- TRUE
  })

  expect_identical(out, 1:2)
})

test_that("can use next within loop()", {
  out <- int()
  bool <- FALSE

  loop(for (x in 1:3) {
    if (bool) {
      next
    } else {
      out <- c(out, x)
    }
    bool <- TRUE
  })

  expect_identical(out, 1L)
})

test_that("can loop with a non-block expression", {
  iter <- as_iterator(1:3)
  out <- int()

  loop(for (x in iter) out <- c(out, x))
  expect_identical(out, 1:3)
})

test_that("iterating with a done iterator cause an error", {
  iter <- gen("foo")
  loop(for (x in iter) x)
  expect_exhausted(loop(for (x in iter) x))
})

test_that("iterating works when coro is not loaded", {
  new_env <- new.env(parent = baseenv())

  evalq(
    coro::loop(for (x in coro::as_iterator(1:3)) x),
    new_env
  )

  expect_equal(new_env[["x"]], 3)
})

test_that("loop returns invisibly", {
  out <- withVisible(loop(
    for (i in as_iterator(1:3)) {
      i
    }
  ))
  expect_false(out$visible)
})

test_that("can return from `loop()`", {
  fn <- function() {
    loop(for (x in 1) return("foo"))
  }
  expect_identical(fn(), "foo")
})

test_that("loop() fails informatively inside generators (#31)", {
  expect_snapshot_error(
    gen(loop(for (x in 1:10) yield(x)))(),
    cran = TRUE
  )
})

test_that("can use loop() in lambdas inside generators", {
  out <- NULL
  g <- gen(for (i in 1:3) yield(i))
  gen({
    f <- function() loop(for (x in g) out <<- c(out, x))
    f()
  })()
  expect_equal(out, 1:3)
})
