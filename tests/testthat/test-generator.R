
test_that("can create non-yielding generator functions", {
  gen <- generator(function() "foo")
  expect_identical(collect(gen()), list("foo"))
})

test_that("can yield `NULL` without terminating iteration", {
  gen <- gen(yield(NULL))
  expect_null(gen())
  expect_exhausted(gen())
})

test_that("short syntax and for loop support", {
  numbers <- 1:6
  odds <- gen(for (x in numbers) if (x %% 2 != 0) yield(x))
  squares <- gen(for (x in odds) yield(x^2))

  out <- dbl()
  loop(for (x in squares) {
    out <- c(out, x)
  })

  expect_identical(out, dbl(1, 9, 25))
})

test_that("generator factories print nicely", {
  factory <- generator(function() yield(NULL))
  expect_snapshot(print(factory, reproducible = TRUE))
  expect_snapshot(print(factory, reproducible = TRUE, internals = TRUE))
})

test_that("generator instances prints nicely", {
  factory <- generator(function() yield(NULL))
  instance <- factory()
  expect_snapshot(print(instance, reproducible = TRUE))
  expect_snapshot(print(instance, reproducible = TRUE, internals = TRUE))
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

test_that("generator functions inherit from `coro_generator`", {
  g <- generator(function() NULL)
  expect_true(inherits(g, "coro_generator"))
  expect_false(inherits(g(), "coro_generator"))
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
  expect_identical(collect(new_gen()), list(1L, 2L, 3L, 4L, 100L))
})

test_that("unexpected exits disable generators", {
  g <- gen({
    invokeRestart("foo")
    yield("foo")
  })

  withRestarts(
    foo = function() NULL,
    g()
  )
  expect_error(g(), "disabled because of an unexpected exit")
})

test_that("can use tryCatch()", {
  out <- gen({
    tryCatch(
      error = function(...) "handled", {
        stop("error")
        yield("yield")
      }
    )
  })()
  expect_equal(out, "handled")

  out <- gen({
    tryCatch(
      error = function(...) "handled", {
        stop("error")
        yield("yield")
      }
    )
    "value"
  })()
  expect_equal(out, "value")

  out <- gen({
    if (TRUE) {
      tryCatch(
        error = function(...) "handled", {
          repeat if (TRUE) stop("error")
          yield("yield")
        }
      )
    }
    "value"
  })()
  expect_equal(out, "value")

  out <- gen(tryCatch({ stop("foo"); yield("value") }, error = function(...) "handled"))()
  expect_equal(out, "handled")

  expect_error(
    gen({
      tryCatch(
        foo = function(...) "handled", {
          stop("error")
          yield("yield")
        }
      )
      "value"
    })(),
    regexp = "error"
  )
})

test_that("tryCatch(finally = ) is handled", {
  expect_error(
    gen({
      tryCatch(
        error = function(...) "handled", {
          stop("error")
          yield("yield")
        },
        finally = return("finally")
      )
      "value"
    })(),
    regexp = "not implemented"
  )
})

test_that("can yield within tryCatch()", {
  g <- gen({
    tryCatch(error = function(...) "handled", {
      yield("value")
      stop("error")
    })
  })
  expect_equal(g(), "value")
  expect_equal(g(), "handled")
})

test_that("can assign tryCatch()", {
  g <- gen({
    value <- tryCatch(error = function(...) "handled", {
      yield("value")
      stop("error")
    })
    value
  })
  expect_equal(collect(g), list("value", "handled"))

  # Last expression
  fn <- NULL
  g <- gen({
    fn <<- function() value
    value <- tryCatch(error = function(...) "handled", {
      yield("value")
      stop("error")
    })
  })
  expect_equal(collect(g), list("value", "handled"))
  expect_equal(fn(), "handled")
})

test_that("can't await() within a generator", {
  expect_error(generator(function() await(foo))(), "non-async")
  expect_error(generator(function() for (x in await_each(foo)) NULL)(), "non-async")
})

test_that("reentering the generator forces argument in proper context", {
  g <- generator(function() {
    yield("value")
    "wrong"
  })()
  g()
  expect_error(g(stop("error")), "error")

  g <- generator(function() {
    tryCatch(error = function(...) "handled", {
      yield("value")
      return("wrong")
    })
  })()
  g()
  expect_equal(g(stop("error")), "handled")
})

test_that("exit expressions are suspended and resumed", {
  unwound <- FALSE

  g <- generator(function() {
    on.exit(unwound <<- TRUE)
    yield(1)
    2
  })()

  expect_equal(g(), 1)
  expect_false(unwound)

  expect_equal(g(), 2)
  expect_true(unwound)

  unwound <- FALSE
  expect_exhausted(g())
  expect_false(unwound)
})

test_that("formals of generator factory do not mask private variables", {
  generate <- generator(function(fn = "arg", env = "arg") c(fn, env))
  expect_equal(
    generate()(),
    c("arg", "arg")
  )
})

test_that("yield-assign returns default `NULL`", {
  g <- generator(function() x <- yield("foo"))()
  expect_equal(collect(g), list("foo", NULL))

  g <- generator(function() x <- tryCatch(yield("foo")))()
  expect_equal(collect(g), list("foo", NULL))
})

test_that("trailing yield-assign returns argument", {
  g <- generator(function() x <- yield("foo"))()
  g()
  expect_equal(g("bar"), "bar")

  g <- generator(function() x <- tryCatch(yield("foo")))()
  g()
  expect_equal(g("bar"), "bar")

  g <- generator(function() x <- tryCatch(if (TRUE) yield("foo")))()
  g()
  expect_equal(g("bar"), "bar")

  g <- generator(function() {
    x <- tryCatch(if (TRUE) yield("foo"))
    x
  })()
  g()
  expect_equal(g("bar"), "bar")
})

test_that("generators call as_iterator() method", {
  local_methods(
    `as_iterator.coro_iterator` = function(x) {
      x <- 0L
      function() {
        if (x < 3L) {
          x <<- x + 1L
          x
        } else {
          quote(exhausted)
        }
      }
    }
  )

  i <- structure(list(), class = "coro_iterator")

  out <- NULL
  loop(for (x in i) out <- c(out, x))

  expect_equal(out, 1:3)
})

test_that("can yield-assign with `=` (#29)", {
  g <- generator(function() x = yield("foo"))
  expect_equal(collect(g()), list("foo", NULL))

  i <- g()
  i()
  expect_equal(i("bar"), "bar")
})
