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
  expect_identical(b, "bar")
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

test_that("can shift within a while loop", {
  cnts <- list()

  out <- reset({
    out <- "foo"
    i <- 0
    while (i < 5) {
      i <- i + 1
      out <- SHIFT(function(cnt) {
        cnts <<- c(cnts, list(cnt))
        cnt(out)
      })
      out <- paste(out, i)
    }
    toupper(out)
  })

  expect_identical(out, "FOO 1 2 3 4 5")
  expect_identical(cnts[[1]]("bar"), "BAR 5")
  expect_identical(cnts[[5]]("bar"), "BAR 5")
})

test_that("shifting within a loop does not grow the call stack", {
  stacks <- list()

  out <- reset({
    i <- 0
    while (i < 5) {
      i <- i + 1
      SHIFT(function(cnt) {
        stacks <<- c(stacks, list(ctxt_stack()))
        cnt(NULL)
      })
    }
  })

  lengths <- map(stacks, length)
  expect_true(all(lengths[[1]] == lengths[-1]))
})

test_that("can shift with empty continuations", {
  expect_error(regex = NA,
    reset({
      if (FALSE) TRUE
      SHIFT(function(k) 0)
    })
  )

  expect_identical(reset(SHIFT(identity))("foo"), "foo")
})

test_that("continuations inherit from flowery namespace", {
  cnt <- reset(SHIFT(identity))
  expect_identical(get_env(cnt), ns_env("flowery"))
})
