context("return")

test_that("explicit return is added to blocks", {
  exprs <- set_returns(function() {
    "foo"
  })
  expect_identical(exprs, node_list(return_lang("foo")))

  exprs <- set_returns(function() {
    "foo"
    "bar"
  })
  expect_identical(exprs, node_list("foo", return_lang("bar")))

  exprs <- set_returns(function() { })
  expect_identical(exprs, node_list(return_lang(NULL)))

  exprs <- set_returns(function() {{ }})
  expect_identical(exprs, node_list(block(return_lang(NULL))))

  exprs <- set_returns(function() {{ "foo"; "bar" }})
  expect_identical(exprs, node_list(block("foo", return_lang("bar"))))
})

test_that("explicit return is added to if else branches", {
  exprs <- set_returns(function() if (TRUE) "foo")
  expect_identical(exprs, node_list(if_lang(TRUE, block(return_lang("foo")), block(invisible_lang))))

  exprs <- set_returns(function() { if (TRUE) "foo" else "bar" })
  explicit <- if_lang(TRUE, block(return_lang("foo")), block(return_lang("bar")))
  expect_identical(exprs, node_list(explicit))

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
  inner <- if_lang(TRUE, block(return_lang("foo")), block(return_lang("bar")))
  outer <- if_lang(TRUE, block(inner), block(return_lang("baz")))
  expect_identical(exprs, node_list("before", outer))
})

test_that("explicit return is added after loops", {
  exprs <- set_returns(function() {
    "before"
    repeat "foo"
  })
  explicit_repeat <- node_list("before", repeat_lang("foo"), invisible_lang)
  expect_identical(exprs, explicit_repeat)

  exprs <- set_returns(function() {
    "before"
    while (TRUE) "foo"
  })
  explicit_while <- node_list("before", while_lang(TRUE, "foo"), invisible_lang)
  expect_identical(exprs, explicit_while)

  exprs <- set_returns(function() for (i in x) "foo")
  explicit_for <- node_list(for_lang(quote(i), quote(x), "foo"), invisible_lang)
  expect_identical(exprs, explicit_for)
})

test_that("explicit returns are left alone", {
  exprs <- set_returns(function() return("foo"))
  expect_identical(exprs, node_list(return_lang("foo")))

  exprs <- set_returns(function() { "foo"; return("bar") })
  expect_identical(exprs, node_list("foo", return_lang("bar")))
})

test_that("invisible return is added after trailing yield()", {
  exprs <- set_returns(function() yield())
  expect_identical(exprs, node_list(yield_lang(), invisible_lang))

  exprs <- set_returns(function() if (TRUE) yield())
  block <- block(yield_lang(), invisible_lang)
  expect_identical(exprs, node_list(if_lang(TRUE, block, block(invisible_lang))))
})
