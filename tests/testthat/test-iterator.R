context("iterator")

test_that("can take the length of iterators", {
  expect_identical(length(stream_iter), na_int)
  expect_identical(length(batch_iter), 10L)
})

test_that("remaining() is an alias to length()", {
  expect_identical(remaining(stream_iter), na_int)
  expect_identical(remaining(batch_iter), 10L)
})

test_that("print method distinguishes stream and batch iterators", {
  expect_output(print(stream_iter), "<stream-iterator>")
  expect_output(print(batch_iter), "<batch-iterator>")
})

test_that("print method prints original function", {
  expect_output(print(stream_iter), "\"body\"")
  expect_output(print(batch_iter), "\"body\"")

  # Print methods are preserved
  fn <- set_attrs(function() NULL, class = "foo")
  print.foo <- function(x) cat("print foo\n")
  iter <- new_iterator(fn)

  with_bindings(.env = global_env(),
    print.foo = print.foo,
    expect_output(print(iter), "print foo")
  )
})

test_that("new iterators are not done", {
  expect_false(is_done(stream_iter))
  expect_false(is_done(batch_iter))
})

test_that("first value is NULL", {
  expect_null(deref(stream_iter))
  expect_null(deref(batch_iter))
})

test_that("last value is recorded", {
  iter <- new_iterator(new_integer_stream())

  expect_identical(iter(), 0L)
  expect_identical(deref(iter), 0L)

  expect_identical(iter(), 1L)
  expect_identical(deref(iter), 1L)
})

test_that("NULL terminates stream iterators", {
  done <- FALSE
  stream <- new_iterator(function() {
    if (done) NULL else "foo"
  })

  expect_false(is_done(stream))
  expect_identical(stream(), "foo")
  expect_false(is_done(stream))

  done <- TRUE
  expect_null(stream())
  expect_true(is_done(stream))
})

test_that("NULL does not terminate batch iterators", {
  iter <- new_iterator(function() NULL, 3)
  expect_null(iter()); iter()

  expect_false(is_done(iter))
  expect_null(iter())
  expect_true(is_done(iter))
})

test_that("Batch iterators terminate after `n` iterations", {
  iter <- new_iterator(new_integer_stream(), 3)
  iter(); iter()

  expect_false(is_done(iter))
  expect_identical(iter(), 2L)
  expect_true(is_done(iter))
})

test_that("can convert vectors to iterators", {
  iter <- as_iterator(1:3)

  expect_identical(iter(), 1L)
  expect_identical(iter(), 2L)
  expect_false(is_done(iter))

  expect_identical(iter(), 3L)
  expect_true(is_done(iter))
})

test_that("as_iterator() is a no-op with iterators", {
  iter <- as_iterator(1:3)
  iter()

  out <- as_iterator(iter)
  expect_identical(deref(out), 1L)
})
