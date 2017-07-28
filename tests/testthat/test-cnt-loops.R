
context("loops")

test_that("shifting within repeat and for is disallowed", {
  expect_error(reset(repeat SHIFT(identity)), "Can't shift within a repeat or for loop")
  expect_error(reset(for (x in 1L) SHIFT(identity)), "Can't shift within a repeat or for loop")
})
