context("continuations")

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
  cnt <- reset({
    "discarded"

    if (TRUE) {
      "TRUE-before"
      SHIFT(identity)
      "TRUE-after"
    } else {
      "FALSE"
    }

    "after"
  })

  expect_equal(cnt_body(cnt), quote({"TRUE-after"; "after"}))
})

test_that("can shift within an else branch", {
  cnt <- reset({
    "discarded"

    if (FALSE) {
      "FALSE"
    } else {
      "TRUE-before"
      SHIFT(identity)
      "TRUE-after"
    }

    "after"
  })

  expect_equal(cnt_body(cnt), quote({"TRUE-after"; "after"}))
})

test_that("can shift within nested if-else branches", {
  cnt <- reset({
    "before"
    if (TRUE) {
      "TRUE-before"
      if (FALSE) {
        "TRUE-FALSE"
      } else {
        "TRUE-TRUE-before"
        SHIFT(identity)
        "TRUE-TRUE-after"
      }
      "TRUE-after"
    } else {
      "FALSE"
    }
    "after"
  })

  expect_equal(cnt_body(cnt), quote({"TRUE-TRUE-after"; "TRUE-after"; "after"}))
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

test_that("past discarder takes nested loops into account", {
  discarded <- discard_past(quote({
    "discarded"
    while (TRUE) {
      "inner-before"
      SHIFT(identity)
      "inner-after"
    }
  }))
  expect_equal(discarded, pairlist(
    "inner-after",
    quote(while (TRUE) {
      "inner-before"
      SHIFT(identity)
      "inner-after"
    })
  ))

  discarded <- discard_past(quote({
    "discarded"
    while (TRUE) {
      "outer-before"
      while (TRUE) {
        "inner-before"
        SHIFT(identity)
        "inner-after"
      }
      "outer-after"
    }
  }))

  expect_equal(discarded, pairlist(
    "inner-after",
    quote(while (TRUE) {
      "inner-before"
      SHIFT(identity)
      "inner-after"
    }),
    "outer-after",
    quote(while (TRUE) {
      "outer-before"
      while (TRUE) {
        "inner-before"
        SHIFT(identity)
        "inner-after"
      }
      "outer-after"
    })
  ))
})

test_that("can shift within a nested while loop", {
  cnt <- reset({
    "discarded"
    while (TRUE) {
      "outer-before"
      while (TRUE) {
        "inner-before"
        SHIFT(identity)
        "inner-after"
      }
      "outer-after"
    }
  })

  expected_cnt <- quote({
    "inner-after"
    while (TRUE) {
      "inner-before"
      SHIFT(identity)
      "inner-after"
    }
    "outer-after"
    while (TRUE) {
      "outer-before"
      while (TRUE) {
        "inner-before"
        SHIFT(identity)
        "inner-after"
      }
      "outer-after"
    }
  })

  expect_equal(cnt_body(cnt), expected_cnt)
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

test_that("shift is discarded", {
  block <- quote({
    "bar"
    SHIFT(identity)
    "foo"
  })
  expect_identical(discard_past(block), pairlist("foo"))
})
