
context("loops")

test_that("shifting within repeat and for is disallowed", {
  expect_error(reset(repeat SHIFT(identity)), "Can't shift within a repeat or for loop")
  expect_error(reset(for (x in 1L) SHIFT(identity)), "Can't shift within a repeat or for loop")
})

test_that("next and break are disallowed within loops", {
  expect_error(reset(while(TRUE) { SHIFT(identity); break }), "Can't break or continue")
  expect_error(reset(while(TRUE) { SHIFT(identity); next }), "Can't break or continue")
})
