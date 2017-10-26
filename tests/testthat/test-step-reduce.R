context("step-reduce")

test_that("reduce() stops early on reduced input", {
  reducer <- function(result, input) {
    if (input %% 2 == 0) {
      box_reduced(result)
    } else {
      c(result, input)
    }
  }

  expect_identical(reduce(1:5, reducer), 1L)
  expect_identical(reduce(int(1L, 3L, 5:10), reducer), int(1L, 3L, 5L))
})

test_that("reduce_steps() calls initial step for initial value", {
  builder <- function(result, input) {
    if (missing(result) && missing(input)) {
      stop("called for init value")
    }
  }

  steps <- compose(map_step(`+`, 1), map_step(`+`, 1))
  expect_error(reduce_steps(NULL, steps, builder), "called for init value")
})

test_that("reduce_steps() calls initial step for result completion", {
  builder <- function(result, input) {
    if (missing(result) && missing(input)) {
      return(NULL)
    }
    if (missing(input)) {
      stop("called for init completion")
    }
  }

  steps <- compose(map_step(`+`, 1), map_step(`+`, 1))
  expect_error(reduce_steps(NULL, steps, builder), "called for init completion")
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

test_that("take variants return correct output type", {
  expect_identical(take(1:10, 2), list(1L, 2L))
  expect_identical(take_lgl(1:10, 2), lgl(1, 2))
  expect_identical(take_int(1:10, 2), int(1, 2))
  expect_identical(take_dbl(1:10, 2), dbl(1, 2))
  expect_identical(take_cpl(1:10, 2), cpl(1, 2))
  expect_identical(take_chr(1:10, 2), chr("1", "2"))
  expect_identical(take_raw(1:10, 2), bytes(1, 2))
})

test_that("take() fails if not enough elements", {
  expect_error(take(1:10, 15), "10 / 15 elements")
})
