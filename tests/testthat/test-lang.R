context("lang")

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
