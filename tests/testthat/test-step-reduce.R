context("step-reduce")

test_that("reduce() stops early on reduced input", {
  reducer <- function(result, input) {
    if (input %% 2 == 0) {
      new_reduced(result)
    } else {
      c(result, input)
    }
  }

  expect_identical(reduce(1:5, reducer), 1L)
  expect_identical(reduce(int(1L, 3L, 5:10), reducer), int(1L, 3L, 5L))
})
