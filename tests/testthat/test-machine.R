
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
      body2()
      yield("value")
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
