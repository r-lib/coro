
test_that("explicit return is added to blocks", {
  exprs <- set_returns(function() {
    "foo"
  })
  expect_identical(exprs, pairlist(return_state_call("foo")))

  exprs <- set_returns(function() {
    "foo"
    "bar"
  })
  expect_identical(exprs, pairlist("foo", return_state_call("bar")))

  exprs <- set_returns(function() { })
  expect_identical(exprs, pairlist(return_state_call(NULL)))

  exprs <- set_returns(function() {{ }})
  expect_identical(exprs, pairlist(block(return_state_call(NULL))))

  exprs <- set_returns(function() {{ "foo"; "bar" }})
  expect_identical(exprs, pairlist(block("foo", return_state_call("bar"))))
})

test_that("explicit return is added to if else branches", {
  exprs <- set_returns(function() if (TRUE) "foo")
  expect_identical(exprs, pairlist(if_call(TRUE, block(return_state_call("foo")), block(return_invisible_call))))

  exprs <- set_returns(function() { if (TRUE) "foo" else "bar" })
  explicit <- if_call(TRUE, block(return_state_call("foo")), block(return_state_call("bar")))
  expect_identical(exprs, pairlist(explicit))

  exprs <- set_returns(function() {
    "before"
    if (TRUE)
      if (TRUE)
        "foo"
      else
        "bar"
    else
      "baz"
  })
  inner <- if_call(TRUE, block(return_state_call("foo")), block(return_state_call("bar")))
  outer <- if_call(TRUE, block(inner), block(return_state_call("baz")))
  expect_identical(exprs, pairlist("before", outer))
})

test_that("explicit return is added after loops", {
  exprs <- set_returns(function() {
    "before"
    repeat "foo"
  })
  explicit_repeat <- pairlist("before", repeat_call("foo"), return_invisible_call)
  expect_identical(exprs, explicit_repeat)

  exprs <- set_returns(function() {
    "before"
    while (TRUE) "foo"
  })
  explicit_while <- pairlist("before", while_call(TRUE, "foo"), return_invisible_call)
  expect_identical(exprs, explicit_while)

  exprs <- set_returns(function() for (i in x) "foo")
  explicit_for <- pairlist(for_call(quote(i), quote(x), "foo"), return_invisible_call)
  expect_identical(exprs, explicit_for)
})

test_that("explicit returns are swapped", {
  exprs <- set_returns(function() return("foo"))
  expect_identical(exprs, pairlist(return_state_call("foo")))

  exprs <- set_returns(function() { "foo"; return("bar") })
  expect_identical(exprs, pairlist("foo", return_state_call("bar")))

  exprs <- set_returns(function() list("foo"))
  expect_identical(exprs, pairlist(return_state_call(quote(list("foo")))))
})

test_that("invisible return is added after trailing yield()", {
  exprs <- set_returns(function() yield())
  expect_identical(exprs, pairlist(yield_call(), return_invisible_call))

  exprs <- set_returns(function() if (TRUE) yield())
  block <- block(yield_call(), return_invisible_call)
  expect_identical(exprs, pairlist(if_call(TRUE, block, block(return_invisible_call))))
})

test_that("setting returns does not mutate expression", {
  handle <- function() generator(function() { while (TRUE) { 1; yield(); 2 } })
  body <- node_car(node_cddr(node_cadr(body(handle))))
  exp <- quote({ while (TRUE) { 1; yield(); 2 } })
  expect_identical(body, exp)
  handle()
  expect_identical(body, exp)
})
