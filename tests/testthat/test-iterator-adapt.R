test_that("can adapt with a simple transformation", {
  iter <- as_iterator(1:2)
  iter <- iter_adapt(iter, iter_map(`+`, 10L))
  expect_identical(collect(iter), list(11L, 12L))
})

test_that("can adapt with a discarding transformation", {
  iter <- as_iterator(1:4)
  iter <- iter_adapt(iter, iter_discard(~ .x %% 2 == 0))
  expect_identical(collect(iter), list(1L, 3L))
})

test_that("can adapt with composed steps", {
  iter <- as_iterator(1:4)
  iter <- iter_adapt(iter, iter_map(`+`, 10L), iter_discard(~ .x %% 2 == 0))
  expect_identical(collect(iter), list(11L, 13L))
})
