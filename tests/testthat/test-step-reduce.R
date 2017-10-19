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

test_that("reduce_steps() calls initial step for initial value", {
  init_step <- function(result, input) {
    if (missing(result) && missing(input)) {
      stop("called for init value")
    }
  }

  step <- compose(map_step(`+`, 1), map_step(`+`, 1))
  expect_error(reduce_steps(NULL, step, init_step), "called for init value")
})

test_that("reduce_steps() calls initial step for result completion", {
  init_step <- function(result, input) {
    if (missing(result) && missing(input)) {
      return(NULL)
    }
    if (missing(input)) {
      stop("called for init completion")
    }
  }

  step <- compose(map_step(`+`, 1), map_step(`+`, 1))
  expect_error(reduce_steps(NULL, step, init_step), "called for init completion")
})

test_that("into() creates vector of requested type", {
  # Also tested indirectly via take tests above
  expect_identical(into(dbl_len(3), 1:3), as_double(1:3))
})

test_that("into() shrinks vector if needed", {
  expect_identical(into(integer(10), 1:3), 1:3)
})

test_that("into() grows vector if needed", {
  expect_identical(into(integer(1), 1:3), 1:3)
})
