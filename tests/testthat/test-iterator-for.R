
test_that("can iterate() with iterators", {
  iter <- as_iterator(1:3)
  out <- int()

  iterate(for (x in iter) {
    out <- c(out, x)
  })
  expect_identical(out, 1:3)
})

test_that("can use break within iterate()", {
  out <- int()
  bool <- FALSE

  iterate(for (x in 1:3) {
    out <- c(out, x)
    if (bool) {
      break
    }
    bool <- TRUE
  })

  expect_identical(out, 1:2)
})

test_that("can use next within iterate()", {
  out <- int()
  bool <- FALSE

  iterate(for (x in 1:3) {
    if (bool) {
      next
    } else {
      out <- c(out, x)
    }
    bool <- TRUE
  })

  expect_identical(out, 1L)
})

test_that("can iterate with a non-block expression", {
  iter <- as_iterator(1:3)
  out <- int()

  iterate(for (x in iter) out <- c(out, x))
  expect_identical(out, 1:3)
})

test_that("iterating with a done iterator cause an error", {
  iter <- gen("foo")
  iterate(for (x in iter) x)
  expect_exhausted(iterate(for (x in iter) x))
})

test_that("iterating works when coro is not loaded", {
  new_env <- new.env(parent = baseenv())

  evalq(
    coro::iterate(for (x in coro::as_iterator(1:3)) x),
    new_env
  )

  expect_equal(new_env[["x"]], 3)
})

test_that("iterate returns invisibly", {
  out <- withVisible(iterate(
    for (i in as_iterator(1:3)) {
      i
    }
  ))
  expect_false(out$visible)
})

test_that("can return from `iterate()`", {
  fn <- function() {
    iterate(for (x in 1) return("foo"))
  }
  expect_identical(fn(), "foo")
})

test_that("iterate() fails informatively inside generators (#31)", {
  expect_snapshot_error(
    gen(iterate(for (x in 1:10) yield(x)))(),
    cran = TRUE
  )
})

test_that("can use iterate() in lambdas inside generators", {
  out <- NULL
  g <- gen(for (i in 1:3) yield(i))
  gen({
    f <- function() iterate(for (x in g) out <<- c(out, x))
    f()
  })()
  expect_equal(out, 1:3)
})
