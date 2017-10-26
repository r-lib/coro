context("iter")

test_that("print method prints original function", {
  expect_output(print(simple_iter), "\"body\"")

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
  expect_false(is_done(simple_iter))
})

test_that("first value is NULL", {
  expect_null(deref(simple_iter))
})

test_that("last value is recorded", {
  iter <- new_integer_iterator()

  expect_identical(iter(), 0L)
  expect_identical(deref(iter), 0L)

  expect_identical(iter(), 1L)
  expect_identical(deref(iter), 1L)
})

test_that("NULL terminates iterators", {
  done <- FALSE
  it <- new_iterator(function() {
    if (done) NULL else "foo"
  })

  expect_false(is_done(it))
  expect_identical(it(), "foo")
  expect_false(is_done(it))

  done <- TRUE
  expect_null(it())
  expect_true(is_done(it))
  expect_null(it())
})

test_that("boxed NULLs don't terminate iterators", {
  it <- new_iterator(function() {
    if (boxed) null_box() else NULL
  })

  boxed <- TRUE
  expect_null(it())
  expect_false(is_done(it))

  boxed <- FALSE
  expect_null(it())
  expect_true(is_done(it))
})

test_that("can use advance() to check for termination with side effect", {
  i <- 0L
  it <- new_iterator(function() {
    i <<- i + 1L
    if (i < 2L) null_box() else NULL
  })

  expect_true(advance(it))
  expect_identical(i, 1L)
  expect_null(deref(it))

  expect_false(advance(it))
  expect_identical(i, 2L)
  expect_null(deref(it))
})

test_that("can convert vectors to iterators", {
  iter <- as_iterator(1:3)

  expect_identical(iter(), 1L)
  expect_identical(iter(), 2L)
  expect_false(is_done(iter))

  expect_identical(iter(), 3L)
  expect_null(iter())
  expect_true(is_done(iter))
})

test_that("as_iterator() is a no-op with iterators", {
  iter <- as_iterator(1:3)
  iter()

  out <- as_iterator(iter)
  expect_identical(deref(out), 1L)
})

test_that("as_iterator() handles bare closures", {
  iter <- as_iterator(function() "foo")
  expect_true(is_iterator(iter))
})

test_that("iter() is a shortcut for creating iterators", {

  x <- 1:2
  n <- length(x)
  i <- 0L

  it <- iter({
    while (i < n) {
      i <<- i + 1L
      return(x[[i]] + 10L)
    }
  })

  expect_identical(it(), 11L)
  expect_identical(it(), 12L)
  expect_null(it())
})

test_that("iterator wrapper forwards visibility", {
  it <- new_iterator(function() {
    if (vis) "foo" else invisible("foo")
  })

  vis <- TRUE
  expect_true(withVisible(it())$visible)

  vis <- FALSE
  expect_false(withVisible(it())$visible)
})
