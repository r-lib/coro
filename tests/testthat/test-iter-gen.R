context("iter-gen")

test_that("can create non-yielding generator functions", {
  gen <- generator("foo")
  expect_identical(drain(gen), list("foo"))
})

test_that("can yield `NULL` without terminating iteration", {
  gen <- generator(NULL)
  expect_null(gen())
  expect_true(is_done(gen))

  gen <- generator(yield(NULL))
  expect_null(gen())
  expect_false(is_done(gen))
  expect_null(gen())
  expect_true(is_done(gen))
})

test_that("short syntax and for loop support", {
  numbers <- 1:6
  odds <- gen(for (x in numbers) if (x %% 2 != 0) yield(x))
  squares <- gen(for (x in odds) yield(x^2))

  out <- dbl()
  iterate(for (x in squares) {
    out <- c(out, x)
  })
  expect_identical(out, dbl(1, 9, 25))
})
