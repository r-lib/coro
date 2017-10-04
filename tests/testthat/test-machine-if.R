context("machine-if")

test_that("`if` blocks - one pause", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      "if-before"
      yield(1L)
      "if-after"
    } else {
      FALSE
    }
    "after"
  })

  inner1 <- if_lang(TRUE, block("if-before", pause_lang("2")), block(FALSE))
  parts1 <- block("before", inner1, goto_lang("3"))
  parts2 <- block("if-after", goto_lang("3"))
  parts3 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2, parts3))
})

test_that("`if` blocks - inner block", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      "if-before"
      {
        "inner-before"
        yield(1L)
        "inner-after"
      }
      "if-after"
    } else {
      FALSE
    }
    "after"
  })

  inner1 <- block("inner-before", pause_lang("2"))
  inner1 <- if_lang(TRUE, block("if-before", inner1), block(FALSE))
  parts1 <- block("before", inner1, goto_lang("4"))
  parts2 <- block("inner-after", goto_lang("3"))
  parts3 <- block("if-after", goto_lang("4"))
  parts4 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2, parts3, parts4))
})

test_that("`if` blocks - nested", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      "if-before"
      if (FALSE) yield(1L)
      "if-after"
    } else {
      "foo"
    }
  })

  inner1 <- if_lang(FALSE, block(pause_lang("2")))
  inner1 <- block("if-before", inner1, goto_lang("2"))
  inner1 <- if_lang(TRUE, inner1, block(return_lang("foo")))
  parts1 <- block("before", inner1)
  parts2 <- block(return_lang("if-after"))

  expect_equal(parts, node_list(parts1, parts2))
})

test_that("`if` blocks - nested and trailing pause", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      "if-before"
      if (FALSE) yield(1L)
    } else {
      "foo"
    }
  })

  inner1 <- if_lang(FALSE, block(pause_lang("2")), block(invisible_lang))
  inner1 <- block("if-before", inner1)
  inner1 <- if_lang(TRUE, inner1, block(return_lang("foo")))
  parts1 <- block("before", inner1)
  parts2 <- block(invisible_lang)

  expect_equal(parts, node_list(parts1, parts2))
})
