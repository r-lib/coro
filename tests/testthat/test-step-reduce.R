test_that("reduce() stops early on reduced input", {
  reducer <- function(result, input) {
    if (input %% 2 == 0) {
      done(result)
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

  steps <- compose(iter_map(`+`, 1), iter_map(`+`, 1))
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

  steps <- compose(iter_map(`+`, 1), iter_map(`+`, 1))
  expect_error(reduce_steps(NULL, steps, builder), "called for init completion")
})

test_that("into() creates vector of requested type", {
  # Also tested indirectly via take tests above
  expect_identical(into(new_double(3), 1:3), dbl(1:3))
})

test_that("into() shrinks vector if needed", {
  expect_identical(into(integer(10), 1:3), 1:3)
})

test_that("into() grows vector if needed", {
  expect_identical(into(integer(1), 1:3), 1:3)
})

test_that("collect(n = ) fails if not enough elements", {
  expect_error(collect(1:10, n = 15), "10 / 15 elements")
})

test_that("can reduce iterators", {
  iter <- as_iterator(1:3)
  out <- reduce_steps(iter, NULL, along_builder(chr()))
  expect_identical(out, as.character(1:3))
})

test_that("reducing exhausted iterators produces empty output", {
  expect_identical(
    collect(function() exhausted()),
    list()
  )
})

test_that("collect() retains `NULL` values", {
  g <- function() {
    i <- 0
    function() {
      i <<- i + 1
      if (i > 3) {
        exhausted()
      } else {
        out[[i]]
      }
    }
  }

  out <- list(1, 2, NULL)
  expect_equal(collect(g()), out)

  out <- list(1, NULL, 2)
  expect_equal(collect(g()), out)
})

test_that("collect() calls `as_iterator()`", {
  local_methods(
    as_iterator.coro_foobar = function(x) {
      called <- FALSE
      function() {
        out <- if (called) exhausted() else "foo"
        called <<- TRUE
        out
      }
    }
  )
  foobar <- structure(list(), class = "coro_foobar")
  expect_equal(
    collect(foobar),
    list("foo")
  )
})

test_that("collect() preserves type (#32)", {
  expect_equal(
    collect(gen(for (i in 1:3) yield(i))),
    as.list(1:3)
  )
  expect_equal(
    wait_for(async_collect(async_generator(function() for (i in 1:3) yield(i))())),
    as.list(1:3)
  )

  expect_equal(
    collect(gen(for (i in 1:3) yield(list(i)))),
    lapply(1:3, list)
  )
  expect_equal(
    wait_for(async_collect(async_generator(function() for (i in 1:3) yield(list(i)))())),
    lapply(1:3, list)
  )

  expect_equal(
    collect(gen(for (i in 1:3) yield(mtcars[i, 1:3]))),
    lapply(1:3, function(i) mtcars[i, 1:3])
  )
  expect_equal(
    wait_for(async_collect(async_generator(function() for (i in 1:3) yield(mtcars[i, 1:3]))())),
    lapply(1:3, function(i) mtcars[i, 1:3])
  )
})
