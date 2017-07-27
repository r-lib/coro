context("continuations")

test_that("detects assigned shift", {
  expr <- quote(foo <- SHIFT(NULL))
  expect_true(is_assigned_shift(expr))
})

test_that("detects deep shift", {
  expect_true(has_shift(quote(foo(SHIFT()))))
  expect_true(has_shift(quote(foo(bar, SHIFT()))))
  expect_true(has_shift(quote(SHIFT()())))
})

test_that("can't shift outside of a reset", {
  expect_error(SHIFT(), "Can't shift outside of a reset")
})

test_that("can't shift within a call", {
  expect_error(reset({ list(SHIFT()) }), "Can't shift within a function call")
  expect_error(reset({ list(letters, SHIFT()) }), "Can't shift within a function call")
  expect_error(reset({ SHIFT()() }), "Can't shift within a function call")
  expect_error(reset({ list(SHIFT())() }), "Can't shift within a function call")
  expect_error(reset({ a <- list(SHIFT()) }), "Can't shift within a function call")
})

test_that("can shift within an assignment", {
  cnt <- NULL

  out <- reset({
    a <- "foo"
    b <- SHIFT(function(k) { cnt <<- k; k("bar") } )
    paste(a, b, "baz")
  })

  expect_identical(a, "foo")
  expect_false(env_has(, "b"))
  expect_identical(out, "foo bar baz")
  expect_identical(cnt("BAR"), "foo BAR baz")
})

test_that("can shift within an if branch", {
  cnt <- NULL

  out <- reset({
    a <- "foo"

    if (FALSE) {
      abort("FALSE")
    } else {
      b <- SHIFT(function(k) {
        cnt <<- k
        k("bar")
      })
      b <- toupper(b)
    }

    paste(a, b, "baz")
  })

  expect_identical(out, "foo BAR baz")
  expect_identical(cnt("bim"), "foo BIM baz")
})
