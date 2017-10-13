context("iterator-iterate")

test_that("can iterate() with over batch iterators", {
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
