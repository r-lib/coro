
test_that("can create non-yielding generator functions", {
  gen <- generator(function() "foo")
  expect_identical(drain(gen()), list("foo"))
})

test_that("can yield `NULL` without terminating iteration", {
  gen <- gen(NULL)
  expect_null(gen())
  expect_null(gen())
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

test_that("generator prints nicely", {
  zap_env <- function(x) {
    environment(x) <- global_env()
    x
  }

  expect_snapshot({
    print(zap_env(gen({
      while (TRUE) {
        if (TRUE) {
          yield(1)
        }
        return(2)
      }
    })))
  })
})

test_that("can send values to generators", {
  g <- generator(function(x) repeat x <- yield(x))
  expect_identical(g(1)(), 1)
  expect_identical(g(2)(), 2)
})

test_that("state is refreshed", {
  g <- generator(function(x) {
    y <- yield(x)
    yield(x)
  })

  expect_identical(g(1)(), 1)
  expect_identical(g("foo")(), "foo")
})

test_that("generator() takes anonymous functions", {
  fn <- function() NULL
  expect_error(generator(fn), "anonymous")
})

test_that("generators can't yield `NULL`", {
  g <- generator(function() yield())
  expect_error(g()(), "Can't yield `NULL`")

  g <- generator(function() yield(NULL))
  expect_error(g()(), "Can't yield `NULL`")
})

test_that("generator functions inherit from `flowery_generator`", {
  g <- generator(function() NULL)
  expect_true(inherits(g, "flowery_generator"))
  expect_false(inherits(g(), "flowery_generator"))
})

test_that("generator factories can take dots", {
  new_gen <- generator(function(...) yield(list(...)))
  gen1 <- new_gen(x = 1)
  gen2 <- new_gen(x = 2)

  expect_equal(gen1(), list(x = 1))
})

test_that("generators can take missing arguments", {
  new_gen <- generator(function(arg) missing(arg))
  expect_true(new_gen()())
  expect_false(new_gen(1)())
})

test_that("yield within if within for loops properly", {
  new_gen <- generator(function() {
    x <- 1:10
    for (elt in x) {
      if (elt < 5) {
        yield(elt)
      } else {
        return(100L)
      }
    }
  })
  expect_identical(drain_int(new_gen()), c(1:4, 100L))
})
