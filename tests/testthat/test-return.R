
test_that("explicit return is added to blocks", {
  expect_snapshot0(set_returns(function() {
    "foo"
  }))

  expect_snapshot0(set_returns(function() {
    "foo"
    "bar"
  }))

  expect_snapshot0(set_returns(function() { }))
  expect_snapshot0(set_returns(function() {{ }}))
  expect_snapshot0(set_returns(function() {{ "foo"; "bar" }}))
})

test_that("explicit return is added to if else branches", {
  expect_snapshot0(set_returns(function() if (TRUE) "foo"))
  expect_snapshot0(set_returns(function() { if (TRUE) "foo" else "bar" }))

  expect_snapshot0(set_returns(function() {
    "before"
    if (TRUE)
      if (TRUE)
        "foo"
      else
        "bar"
    else
      "baz"
  }))
})

test_that("explicit return is added after loops", {
  expect_snapshot0(set_returns(function() {
    "before"
    repeat "foo"
  }))

  expect_snapshot0(set_returns(function() {
    "before"
    while (TRUE) "foo"
  }))

  expect_snapshot0(set_returns(function() for (i in x) "foo"))
})

test_that("explicit returns are swapped", {
  expect_snapshot0(set_returns(function() return("foo")))
  expect_snapshot0(set_returns(function() { "foo"; return("bar") }))
  expect_snapshot0(set_returns(function() list("foo")))
})

test_that("invisible return is added after trailing yield()", {
  expect_snapshot0(set_returns(function() yield()))
  expect_snapshot0(set_returns(function() if (TRUE) yield()))
})

test_that("setting returns does not mutate expression", {
  handle <- function() generator(function() { while (TRUE) { 1; yield(); 2 } })
  body <- node_car(node_cddr(node_cadr(body(handle))))
  exp <- quote({ while (TRUE) { 1; yield(); 2 } })
  expect_identical(body, exp)
  handle()
  expect_identical(body, exp)
})
