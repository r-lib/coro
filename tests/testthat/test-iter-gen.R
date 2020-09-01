
test_that("can create non-yielding generator functions", {
  gen <- generator(function() "foo")
  expect_identical(drain(gen), list("foo"))
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
