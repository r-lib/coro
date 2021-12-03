test_that("iter_map() applies function", {
  expect_identical(into(int(), 1:3, iter_map(`+`, 10L)), 11:13)
})

test_that("iter_discard() discards based on predicate", {
  expect_identical(into(int(), 1:3, iter_discard(~ . %% 2 == 0)), int(1L, 3L))
})

test_that("iter_keep() keeps based on predicate", {
  expect_identical(into(int(), 1:3, iter_keep(~ . %% 2 == 0)), 2L)
})

test_that("can compose steps to form a new step", {
  step <- compose(iter_map(`+`, 1), iter_discard(~ . %% 2 == 0))
  expect_identical(into(int(), 1:6, step), int(3L, 5L, 7L))

  step2 <- compose(iter_discard(~ . %% 2 == 0), iter_map(`+`, 1))
  expect_identical(into(int(), 1:6, step2), int(2L, 4L, 6L))

  step3 <- compose(step, iter_map(`+`, 1))
  expect_identical(into(int(), 1:6, step3), int(4L, 6L, 8L))

  step4 <- compose(iter_map(`+`, 1), step)
  expect_identical(into(int(), 1:6, step4), int(3L, 5L, 7L))
})
