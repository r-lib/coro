context("iterator")

test_that("can take the length of iterators", {
  expect_identical(length(stream_iter), na_int)
  expect_identical(length(batch_iter), 10L)
})

test_that("remaining() is an alias to length()", {
  expect_identical(remaining(stream_iter), na_int)
  expect_identical(remaining(batch_iter), 10L)
})

test_that("can't dereference non-subclassed iterators", {
  expect_error(deref(stream_iter), "bare iterators")
  expect_error(deref(batch_iter), "bare iterators")
})

test_that("print method distinguishes stream and batch iterators", {
  expect_output(print(stream_iter), "<stream-iterator>")
  expect_output(print(batch_iter), "<batch-iterator>")
})

test_that("new iterators are not done", {
  expect_false(is_done(stream_iter))
  expect_false(is_done(batch_iter))
})
