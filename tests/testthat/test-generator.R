test_that("can create non-yielding generator functions", {
  gen <- generator(function() "foo")
  expect_identical(collect(gen()), list())
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
  new_gen <- generator(function(arg) yield(missing(arg)))
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
  expect_identical(collect(new_gen()), list(1L, 2L, 3L, 4L))
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
  out <- collect(gen({
    x <- tryCatch(
      error = function(...) "handled", {
        stop("error")
        yield("yield")
      }
    )
    yield(x)
  }))
  expect_equal(out, list("handled"))

  out <- collect(gen({
    x <- tryCatch(
      error = function(...) "handled", {
        stop("error")
        yield("yield")
      }
    )
    yield(x)
    yield("value")
  }))
  expect_equal(out, list("handled", "value"))

  out <- collect(gen({
    if (TRUE) {
      x <- tryCatch(
        error = function(...) "handled", {
          repeat if (TRUE) stop("error")
          yield("yield")
        }
      )
    }
    yield(x)
    yield("value")
  }))
  expect_equal(out, list("handled", "value"))

  out <- gen(tryCatch({ stop("foo"); yield("value") }, error = function(...) "handled"))()
  expect_equal(out, exhausted())

  # Handlers are matched to the condition class
  expect_error(
    gen({
      tryCatch(
        foo = function(...) "handled", {
          stop("error")
          yield("yield")
        }
      )
      yield("value")
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
    x <- tryCatch(error = function(...) "handled", {
      yield("value")
      stop("error")
    })
    yield(x)
    yield("value")
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
    yield(value)
  })
  expect_equal(collect(g), list("value", "handled"))
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
    x <- tryCatch(error = function(...) "handled", {
      yield("value")
      return("wrong")
    })
    yield(x)
  })()
  g()
  expect_equal(g(stop("error")), "handled")
})

test_that("exit expressions are suspended and resumed", {
  unwound <- FALSE

  g <- generator(function() {
    on.exit(unwound <<- TRUE)
    yield(1)
    yield(2)
  })()

  expect_equal(g(), 1)
  expect_false(unwound)

  expect_equal(g(), 2)
  expect_false(unwound)

  expect_exhausted(g())
  expect_true(unwound)

  unwound <- FALSE
  expect_exhausted(g())
  expect_false(unwound)
})

test_that("formals of generator factory do not mask private variables", {
  generate <- generator(function(fn = "arg", env = "arg") yield(c(fn, env)))
  expect_equal(
    generate()(),
    c("arg", "arg")
  )
})

test_that("yield-assign returns default `NULL`", {
  g <- generator(function() {
    x <- yield("foo")
    yield(x)
  })()
  expect_equal(collect(g), list("foo", NULL))

  g <- generator(function() {
    x <- tryCatch(yield("foo"))
    yield(x)
  })()
  expect_equal(collect(g), list("foo", NULL))
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
          exhausted()
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
  g <- generator(function() {
    x = yield("foo")
    yield(x)
  })
  expect_equal(collect(g()), list("foo", NULL))

  i <- g()
  i()
  expect_equal(i("bar"), "bar")
})

test_that("stepping into generators returns visibly (#46)", {
  generate_abc <- generator(function() {
    yield("a")
    yield("b")
    "c"
  })
  abc <- generate_abc()
  expect_visible(abc())
  expect_visible(abc())
  expect_visible(abc())
  expect_visible(abc())
})

test_that("generators do not cause CMD check notes (#40)", {
  skip_on_cran()
  expect_silent(
    invisible(compiler::cmpfun(
      generator(function() NULL),
      options = list(suppressAll = FALSE)
    ))
  )
})

test_that("on.exit is called when loop breaks early (#52)", {
  called <- NULL
  g <- coro::generator(function() {
    on.exit(called <<- TRUE)
    yield(1)
    yield(2)
  })

  called <- FALSE
  expect_error(
    coro::loop(for (i in g()) {
      stop("boom!")
    })
  )
  expect_true(called)

  called <- FALSE
  iter <- g()
  iter()
  expect_false(called)

  iter(close = TRUE)
  expect_true(called)

  # Only called once
  called <- FALSE
  iter()
  iter(close = TRUE)
  expect_false(called)
})

test_that("for loops in generators close their iterators (#52)", {
  called <- NULL
  g <- coro::generator(function() {
    on.exit(called <<- TRUE)
    yield(1)
    yield(2)
  })
  h <- coro::generator(function() {
    for (i in g()) {
      yield(i)
      stop("foo")
    }
  })

  called <- FALSE
  expect_error(
    collect(h())
  )
  expect_true(called)

  called <- FALSE
  expect_error(loop(for (i in h()) {}))
  expect_true(called)
})

test_that("for loops in generators close their iterators - break (#52)", {
  called <- NULL
  g <- coro::generator(function() {
    on.exit(called <<- TRUE)
    yield(1)
    yield(2)
  })
  h <- coro::generator(function() {
    for (i in g()) {
      yield(i)
      break
    }
  })

  called <- FALSE
  collect(h())
  expect_true(called)

  called <- FALSE
  loop(for (i in h()) {})
  expect_true(called)
})

test_that("Iterators are cleaned up from most nested to least nested", {
  called <- NULL

  g1 <- coro::generator(function() {
    on.exit(called <<- c(called, "g1"))
    yield(1)
    yield(2)
  })
  g2 <- coro::generator(function() {
    on.exit(called <<- c(called, "g2"))
    yield(1)
    yield(2)
  })

  h <- coro::generator(function() {
    on.exit(called <<- c(called, "h"))
    for (i in g1()) {
      for (j in g2()) {
        yield(c(i, j))
        stop("foo")
      }
    }
  })

  expect_error(
    collect(h())
  )
  expect_equal(called, c("g2", "g1", "h"))
})

test_that("disabled generators only clean up once", {
  called <- NULL
  g <- coro::generator(function() {
    on.exit(called <<- c(called, TRUE))
    yield(1)
    stop("foo")
  })()

  expect_equal(g(), 1)

  expect_error(g(), "foo")
  expect_equal(called, TRUE)

  expect_error(g(), "disabled because of an unexpected exit")
  expect_equal(called, TRUE)

  expect_equal(g(close = TRUE), exhausted())
  expect_equal(called, TRUE)

  expect_equal(g(), exhausted())
  expect_equal(called, TRUE)

  expect_equal(g(close = TRUE), exhausted())
  expect_equal(called, TRUE)
})

test_that("generators only clean up once", {
  called <- NULL
  g <- coro::generator(function() {
    on.exit(called <<- c(called, TRUE))
    yield(1)
  })()

  expect_equal(g(), 1)
  expect_null(called)

  expect_equal(g(), exhausted())
  expect_equal(called, TRUE)

  expect_equal(g(close = TRUE), exhausted())
  expect_equal(called, TRUE)

  expect_equal(g(), exhausted())
  expect_equal(called, TRUE)

  expect_equal(g(close = TRUE), exhausted())
  expect_equal(called, TRUE)
})

test_that("returning early doesn't yield values (#51)", {
  g <- generator(function(items) NULL)
  expect_equal(collect(g()), list())

  g <- generator(function(items) {
    if (TRUE) {
      return()
    }
    yield("value")
  })
  expect_equal(collect(g()), list())
})
