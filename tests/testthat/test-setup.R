test_that("setup() can't be called directly or within function arguments", {
  expect_error(setup(1), "called directly")

  f <- generator(function() {
    list(setup(1))
    yield(2)
  })
  expect_error(f()(), "within function arguments")
})

test_that("setup() runs before every step; teardown fires at each step end", {
  the <- new.env()
  the$x <- 0
  log <- character()

  gen <- generator(function() {
    setup({
      old <- the$x
      the$x <- 9
      on.exit(the$x <- old, add = TRUE)
    })
    log <<- c(log, paste0("before1:", the$x))
    yield(1)
    log <<- c(log, paste0("before2:", the$x))
    yield(2)
  })
  g <- gen()

  expect_equal(g(), 1)               # step 1 ran setup, yielded 1
  expect_equal(the$x, 0)             # step 1 teardown restored x
  expect_equal(g(), 2)               # step 2 re-ran setup, yielded 2
  expect_equal(the$x, 0)             # step 2 teardown restored x
  expect_exhausted(g())              # step 3 (after yield 2) ran setup + teardown
  expect_equal(the$x, 0)
  expect_equal(log, c("before1:9", "before2:9"))
})

test_that("setup() teardown is restored around await() (issue #68 reprex)", {
  skip_on_cran()
  the <- new.env()
  the$x <- 0
  seen <- new.env()

  f <- async(function(x) {
    setup({
      old_x <- the$x
      the$x <- x
      on.exit(the$x <- old_x, add = TRUE)
    })
    seen$before <- c(seen$before, the$x)
    await(async_sleep(0))
    seen$after <- c(seen$after, the$x)
    the$x
  })

  out <- wait_for(f(1))
  expect_equal(out, 1)
  expect_equal(seen$before, 1)
  expect_equal(seen$after, 1)
  expect_equal(the$x, 0)
})

test_that("multiple setup() calls stack; teardowns fire in reverse order", {
  log <- character()

  gen <- generator(function() {
    setup(on.exit(log <<- c(log, "teardown-A"), add = TRUE))
    setup(on.exit(log <<- c(log, "teardown-B"), add = TRUE))
    log <<- c(log, "body")
    yield(1)
  })
  g <- gen()

  expect_equal(g(), 1)
  expect_equal(log, c("body", "teardown-B", "teardown-A"))
})

test_that("a failing teardown does not block other teardowns; first error re-raised", {
  log <- character()

  gen <- generator(function() {
    setup({
      on.exit(log <<- c(log, "A-1"), add = TRUE)
      on.exit(stop("boom in A"), add = TRUE)   # both teardowns run; first error re-raised
    })
    setup(on.exit(log <<- c(log, "B"), add = TRUE))
    yield(1)
  })
  g <- gen()

  expect_error(g(), "boom in A")
  expect_equal(log, c("B", "A-1"))
})

test_that("setup() rejects suspension and assignment when the coroutine is compiled", {
  # The state machine is compiled lazily on the first instance call, so these
  # errors surface at `f()` (the trailing `()`), not at factory definition.
  expect_error(generator(function() setup(yield(1)))(), "within `setup\\(\\)`")
  expect_error(async(function() setup(await(1)))(), "within `setup\\(\\)`")
  expect_error(
    generator(function() {
      setup(await_each(1))
      yield(1)
    })(),
    "within `setup\\(\\)`"
  )
  expect_error(
    generator(function() {
      x <- setup(1)
      yield(x)
    })(),
    "Can't assign the result of a `setup` expression"
  )
})

test_that("setup() allows a nested coroutine in its body", {
  gen <- generator(function() {
    setup({
      g2 <- generator(function() yield(1))
    })
    yield("ok")
  })
  expect_equal(gen()(), "ok")
})

# Known shortcomings ----
# These tests document *deliberate* limitations / surprising-but-correct
# behaviors of setup(). They assert the current behavior on purpose.

test_that("KNOWN LIMITATION: plain assignments in setup() are not visible to the body", {
  gen <- generator(function() {
    setup({
      y <- 99
    })
    yield(exists("y", inherits = FALSE))
  })
  expect_false(gen()())
})

test_that("KNOWN LIMITATION: a setup() after a suspend is not retroactive", {
  log <- character()
  gen <- generator(function() {
    log <<- c(log, "step1")
    yield(1)
    setup(log <<- c(log, "setup-registered"))
    yield(2)
  })
  g <- gen()
  g()
  expect_false("setup-registered" %in% log)
  g()
  expect_true("setup-registered" %in% log)
})

test_that("setup() within a loop is an error", {
  # Mixing per-step registration with iteration has opaque semantics, so it is
  # rejected. See the sub-generator workaround in the following test.
  expect_error(
    generator(function() for (i in 1:2) { setup(NULL); yield(i) })(),
    "within a loop"
  )
  expect_error(
    generator(function() while (TRUE) { setup(NULL); yield(1) })(),
    "within a loop"
  )
  expect_error(
    generator(function() repeat { setup(NULL); yield(1) })(),
    "within a loop"
  )
  # Also caught when nested in a branch inside the loop.
  expect_error(
    generator(function() for (i in 1:2) { if (i == 1) setup(NULL); yield(i) })(),
    "within a loop"
  )
})

test_that("per-iteration setup/teardown works by delegating to a sub-generator", {
  the <- new.env()
  the$x <- 0
  seen <- new.env()

  # Factor the per-iteration body into its own generator: its setup() is scoped
  # to the sub-generator's steps, giving per-iteration setup/teardown.
  step <- generator(function(i) {
    setup({
      the$x <- i
      on.exit(the$x <- 0, add = TRUE)
    })
    seen$during <- c(seen$during, the$x)
    yield(i)
  })

  gen <- generator(function() {
    for (i in 1:3) {
      for (x in step(i)) {
        yield(x)
      }
    }
  })

  expect_equal(collect(gen()), list(1L, 2L, 3L))
  expect_equal(seen$during, c(1L, 2L, 3L))   # setup ran with the$x set per iteration
  expect_equal(the$x, 0)                       # ...and restored after each iteration
})

test_that("KNOWN LIMITATION: a teardown error disables the generator", {
  gen <- generator(function() {
    setup(on.exit(stop("teardown boom"), add = TRUE))
    yield(1)
    yield(2)
  })
  g <- gen()
  expect_error(g(), "teardown boom")
  expect_error(g(), "disabled")
})

test_that("KNOWN LIMITATION: an abandoned async promise leaves no final step", {
  skip_on_cran()
  the <- new.env()
  the$x <- 0
  f <- async(function() {
    setup({
      old <- the$x
      the$x <- 1
      on.exit(the$x <- old, add = TRUE)
    })
    await(promises::promise(function(resolve, reject) NULL))
    the$x <- 999
  })
  prom <- f()
  expect_equal(the$x, 0)
})

test_that("setup() compiles to a do_setup() state", {
  expect_snapshot0(generator_body(function() {
    setup({
      old <- the$x
      on.exit(the$x <- old, add = TRUE)
    })
    yield(1)
  }))
})

test_that("setup() supports withr::defer() and withr::local_*() per step", {
  # The documented contract: withr teardown registered in a setup() body is
  # scoped to the step (set up before each step, restored at its end).
  skip_if_not_installed("withr")

  the <- new.env()
  the$x <- 0
  seen <- new.env()

  gen <- generator(function() {
    setup({
      the$x <- 1
      withr::defer(the$x <- 0)
      withr::local_options(coro.setup.flag = "on")
    })
    seen$flag <- c(seen$flag, getOption("coro.setup.flag", "off"))
    yield(the$x)
    seen$flag <- c(seen$flag, getOption("coro.setup.flag", "off"))
    yield(the$x)
  })
  g <- gen()

  expect_equal(g(), 1)                         # withr::defer set the$x to 1
  expect_equal(the$x, 0)                        # ...and restored it at step end
  expect_null(getOption("coro.setup.flag"))     # withr::local_options restored too
  expect_equal(g(), 1)
  expect_equal(the$x, 0)
  expect_null(getOption("coro.setup.flag"))
  expect_equal(seen$flag, c("on", "on"))        # option was set during each step
})
