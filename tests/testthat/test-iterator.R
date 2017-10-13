context("iterator")

bare_iter <- new_iterator(function() "foo")

test_that("can restart on length() of iterator with unknown length", {
  expect_error(length(bare_iter), "unknown length")

  n <- with_handlers(length(bare_iter),
    iterator_unknown_length = restarting("iterator_length", 10L)
  )
  expect_identical(n, 10L)
})

test_that("remaining() is an alias to length()", {
  iter <- new_iterator(function() NULL, 10L)
  expect_identical(remaining(iter), 10L)
  expect_identical(length(iter), 10L)
})

test_that("can't dereference non-subclassed iterators", {
  expect_error(deref(bare_iter))
})
