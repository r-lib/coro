
test_that("generators have return states", {
  expect_snapshot0(generator_body(function() "foo"))
  expect_snapshot0(generator_body(function() return("foo")))
})

test_that("generators have yield states", {
  expect_snapshot0(generator_body(function() yield("foo")))
  expect_snapshot0(generator_body(function() flowery::yield("foo")))
})

test_that("generators support blocks", {
  expect_snapshot0(generator_body(function() {
    "foo"
    "bar"
  }))

  expect_snapshot0(generator_body(function() {
    "foo"
    yield("value")
  }))

  expect_snapshot0(generator_body(function() {
    "foo"
    return("value")
  }))

  expect_snapshot0(generator_body(function() {
    "foo"
    yield("value")
    "bar"
  }))

  expect_snapshot0(generator_body(function() {
    "foo"
    yield("value")
    return("bar")
  }))
})

test_that("generators support repeat loops", {
  expect_snapshot0(generator_body(function() {
    repeat yield("value")
  }))

  expect_snapshot0(generator_body(function() {
    body1()
    repeat {
      yield("value")
      break
    }
    body2()
  }))

  expect_snapshot0(generator_body(function() {
    body1()
    repeat {
      body2()
      yield("value")
      body3()
    }
    body4()
  }))

  expect_snapshot0(generator_body(function() {
    repeat next
  }))

  expect_snapshot0(generator_body(function() {
    body1()
    repeat {
      body2()
      next
      body3()
    }
    body4()
  }))
})

test_that("generators support while loops", {
  expect_snapshot0(generator_body(function() {
    while (loop_condition) {
      body2()
      yield("value")
      body3()
    }
  }))
})

test_that("generators support nested loops", {
  expect_snapshot0(generator_body(function() {
    repeat { repeat yield("foo") }
  }))

  # No srcrefs for inner loops
  expect_snapshot0(generator_body(function() {
    repeat repeat yield("foo")
  }))
  expect_snapshot0(generator_body(function() {
    repeat while (TRUE) yield("foo")
  }))

  expect_snapshot0(generator_body(function() {
    repeat {
      repeat yield("foo")
      "after"
    }
  }))

  expect_snapshot0(generator_body(function() {
    repeat {
      repeat yield("foo")
      break
    }
  }))

  expect_snapshot0(generator_body(function() {
    repeat {
      repeat break
      break
    }
  }))
})

test_that("generators support if-else branches", {
  expect_snapshot0(generator_body(function() {
    body1()
    if (condition) {
      yield("then")
    }
    body2()
  }))

  expect_snapshot0(generator_body(function() {
    body1()
    if (condition) {
      yield("then")
    } else {
      yield("else")
    }
    body2()
  }))

  expect_snapshot0(generator_body(function() {
    body1()
    if (condition) {
      then1()
      yield("then")
      then2()
    } else {
      else1()
      yield("else")
      else2()
    }
    body2()
  }))

  expect_snapshot0(generator_body(function() {
    if (condition) {
      yield("then")
    } else {
      "else"
    }
  }))
})

test_that("break within if", {
  expect_snapshot0(generator_body(function() {
    body1()
    repeat {
      if (condition) {
        break
      }
    }
  }))

  expect_snapshot0(generator_body(function() {
    body1()
    repeat {
      if (condition) {
        break
      }
      body2()
    }
  }))

  expect_snapshot0(generator_body(function() {
    body1()
    if (truth1) if (truth2) yield("value")
    body2()
  }))
})

test_that("generators support if within loops", {
  expect_snapshot0(generator_body(function() {
    repeat {
      if (TRUE) {
        break
      }
    }
  }))

  expect_snapshot0(generator_body(function() {
    repeat {
      body1()
      if (TRUE) {
        break
      } else {
        next
      }
      body2()
    }
    body3()
  }))
})

test_that("can't use break and next outside loop", {
  expect_error(
    generator_body(function() {
      next
    }),
    "within a loop"
  )
  expect_error(
    generator_body(function() {
      if (condition) {
        break
      }
    }),
    "within a loop"
  )
})

test_that("next and break within two layers of if-else", {
  expect_snapshot0(generator_body(function() {
    repeat {
      body1()
      if (TRUE) {
        if (TRUE) next else break
        body2()
      }
      body3()
    }
    body4()
  }))
})

test_that("handle for loops", {
  expect_snapshot0(generator_body(function() for (x in 1:3) yield(x)))
  expect_snapshot0(generator_body(function() for (x in 1:3) for (y in 2:4) yield(list(x, y))))

  expect_snapshot0(generator_body(function() {
    body1()
    for (x in 1:3) yield(x)
    body2()
  }))
})

test_that("handle yield-assign", {
  expect_snapshot0(generator_body(function(x) x <- yield(x)))
  expect_snapshot0(generator_body(function(x) { x <- yield(x) }))
})

test_that("can parse generators without source references", {
  # This reaches both `collect()` and `skip()` in `block_states()`
  src <- "
    generator_body(function() {
      body1()
      while (TRUE) {
        yield('value')
      }
    })"
  expect_error(
    regexp = NA,
    eval(parse(text = src, keep.source = FALSE))
  )
})

test_that("tryCatch() expressions are treated as normal expressions if possible", {
  expect_equal(try_catch_type(quote(tryCatch(error = hnd, foo()))), "expr")
  expect_equal(try_catch_type(quote(tryCatch(error = hnd, yield()))), "tryCatch")
  expect_equal(try_catch_type(quote(tryCatch(error = hnd, { foo() }))), "tryCatch")

  expect_snapshot0(generator_body(function() tryCatch(foo())))
})
