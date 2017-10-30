context("iter-adapt")

test_that("can adapt with a simple transformation", {
  iter <- as_iterator(1:2)
  iter <- iter_adapt(iter, map_step(`+`, 10L))
  expect_identical(drain(iter), list(11L, 12L))
})

test_that("can adapt with a discarding transformation", {
  iter <- as_iterator(1:4)
  iter <- iter_adapt(iter, discard_step(~ .x %% 2 == 0))
  expect_identical(drain(iter), list(1L, 3L))
})

test_that("can adapt with composed steps", {
  iter <- as_iterator(1:4)
  iter <- iter_adapt(iter, map_step(`+`, 10L), discard_step(~ .x %% 2 == 0))
  expect_identical(drain(iter), list(11L, 13L))
})
