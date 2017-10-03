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
  expect_identical(exprs, node_list(if_lang(TRUE, block(return_lang("foo")))))

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
  invisible_lang <- return_lang(lang("invisible", NULL))

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
