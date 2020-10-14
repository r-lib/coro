
test_that("`{` blocks - one pause with no past or future", {
  expect_snapshot0(generator_body(function() {
    yield(1L)
  }))
})

test_that("`{` blocks - one pause", {
  expect_snapshot0(generator_body(function() {
    "before1"
    "before2"
    yield(1L)
    "after1"
    "after2"
  }))
})

test_that("`{` blocks - no preamble", {
  expect_snapshot0(generator_body(function() {
    yield(1L)
    "after"
  }))
})

test_that("`{` blocks - multiple pauses", {
  expect_snapshot0(generator_body(function() {
    "before"
    yield(1L)
    "during"
    yield(2L)
    "after"
  }))
})

test_that("`{` blocks - consecutive pauses", {
  expect_snapshot0(generator_body(function() {
    "before"
    yield(1L)
    yield(2L)
    "after"
  }))
})

test_that("`{` blocks - return value from pause", {
  expect_snapshot0(generator_body(function(x) {
    "before"
    value <- yield(1L)
    "after"
  }))
})

test_that("`{` blocks - no return value", {
  expect_snapshot0(generator_body(function() {
    yield(1L)
  }))

  expect_snapshot0(generator_body(function() {
    yield(1L)
    yield(2L)
  }))
})

test_that("`{` blocks - nested", {
  expect_snapshot0(generator_body(function() {
    "before1"
    "before2"
    {
      "before-inner"
      yield(1L)
      "after-inner"
    }
    "after1"
    "after2"
  }))
})

test_that("`{` blocks - nested and no past before pause", {
  expect_snapshot0(generator_body(function() {
    {
      "before-inner"
      yield(1L)
      "after-inner"
    }
    "after1"
    "after2"
  }))
})

test_that("`{` blocks - nested and goto after pause", {
  expect_snapshot0(generator_body(function() {
    {
      "before-inner"
      yield(1L)
    }
    "after1"
    "after2"
  }))
})

test_that("`{` blocks - complex nesting", {
  expect_snapshot0(generator_body(function() {
    "before"
    {
      "before-inner"
      yield(1L)
      {
        yield(2L)
        yield(3L)
      }
      "after-inner"
    }
    "after"
  }))
})

test_that("`{` blocks - simple nesting with various continuation states", {
  expect_snapshot0(generator_body(function() {
    {
      {
        yield(1L)
        "after-inner-inner"
      }
    }
    "after"
  }))

  expect_snapshot0(generator_body(function() {
    {
      {
        yield(1L)
      }
      "after-inner"
    }
    "after"
  }))
})

test_that("yield assignment in a loop", {
  expect_snapshot0(generator_body(function() while (1) var <- yield("value")))
})
