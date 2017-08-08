context("lang")

test_that("detects assigned shift", {
  expr <- quote(foo <- SHIFT(NULL))
  expect_true(is_assigned_shift(expr))
})

test_that("detects deep shift", {
  expect_true(has_shift(quote(foo(SHIFT()))))
  expect_true(has_shift(quote(foo(bar, SHIFT()))))
  expect_true(has_shift(quote(SHIFT()())))
})

test_that("can detect lang element in nested if-else branches", {
  block <- quote({
    "TRUE-before"
    if (FALSE) {
      "TRUE-FALSE"
    }
    else {
      "TRUE-TRUE-before"
      SHIFT(identity)
      "TRUE-TRUE-after"
    }
    "TRUE-AFTER"
  })

  expect_true(has_shift(block))
})
