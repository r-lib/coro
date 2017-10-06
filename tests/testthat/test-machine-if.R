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

test_that("`else` blocks - one pause", {
  parts <- machine_parts(function() {
    "before"
    if (FALSE) {
      FALSE
    } else {
      "else-before"
      yield(1L)
      "else-after"
    }
    "after"
  })

  inner1 <- if_lang(FALSE, block(FALSE), block("else-before", pause_lang("2")))
  parts1 <- block("before", inner1, goto_lang("3"))
  parts2 <- block("else-after", goto_lang("3"))
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

  inner1 <- if_lang(FALSE, block(pause_lang("2")), block(return_invisible_lang))
  inner1 <- block("if-before", inner1)
  inner1 <- if_lang(TRUE, inner1, block(return_lang("foo")))
  parts1 <- block("before", inner1)
  parts2 <- block(return_invisible_lang)

  expect_equal(parts, node_list(parts1, parts2))
})

test_that("`if` blocks - multiply nested and all trailing", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      "if-before"
      if (FALSE) {
        if (FALSE) {
          yield(2L)
          "if-3-after"
        }
        "if-2-after"
      }
    } else {
      FALSE
    }
  })

  parts1 <- expr({
    "before"
    if (TRUE) {
      "if-before"
      if (FALSE) {
        if (FALSE) {
          !! pause_lang("2")
        }
        !! goto_lang("3")
      } else {
        !! return_invisible_lang
      }
    } else {
      return(FALSE)
    }
  })
  parts2 <- block("if-3-after", goto_lang("3"))
  parts3 <- block(return_lang("if-2-after"))

  expect_equal(parts, node_list(parts1, parts2, parts3))
})

test_that("`if`-`else` blocks - trailing", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      "if-before"
      yield(1L)
      "if-after"
    } else {
      "else-before"
      yield(1L)
      "else-after"
    }
  })

  parts1 <- expr({
    "before"
    if (TRUE) {
      "if-before"
      !! pause_lang("2")
    } else {
      "else-before"
      !! pause_lang("3")
    }
  })
  parts2 <- block(return_lang("if-after"))
  parts3 <- block(return_lang("else-after"))

  expect_equal(parts, node_list(parts1, parts2, parts3))
})

test_that("`if`-`else` blocks - non trailing", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      "if-before"
      yield(1L)
      "if-after"
    } else {
      "else-before"
      yield(2L)
      "else-after"
    }
    "after"
  })

  inner1 <- if_lang(TRUE, block("if-before", pause_lang("2")), block("else-before", pause_lang("3")))
  parts1 <- block("before", inner1)
  parts2 <- block("if-after", goto_lang("4"))
  parts3 <- block("else-after", goto_lang("4"))
  parts4 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2, parts3, parts4))
})

test_that("`if`-`else` blocks - same continuation", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      yield(1L)
    } else {
      yield(2L)
    }
    "after"
  })

  parts1 <- expr({
    "before"
    if (TRUE) {
      !! pause_lang("2")
    } else {
      !! pause_lang("2")
    }
  })
  parts2 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2))
})

test_that("`if`-`else` blocks - continuation in `if`", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      yield(1L)
      "if-after"
    } else {
      yield(2L)
    }
    "after"
  })

  parts1 <- expr({
    "before"
    if (TRUE) {
      !! pause_lang("2")
    } else {
      !! pause_lang("3")
    }
  })
  parts2 <- block("if-after", goto_lang("3"))
  parts3 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2, parts3))
})

test_that("`if`-`else` blocks - continuation in `else`", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      yield(1L)
    } else {
      yield(2L)
      "else-after"
    }
    "after"
  })

  parts1 <- expr({
    "before"
    if (TRUE) {
      !! pause_lang("3")
    } else {
      !! pause_lang("2")
    }
  })
  parts2 <- block("else-after", goto_lang("3"))
  parts3 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2, parts3))
})

test_that("`if` blocks - doubly nested with continuation", {
  parts <- machine_parts(function() {
    if (TRUE) {
      if (TRUE) {
        yield(1L)
        "if-3-after"
      }
    }
    "after"
  })

  parts1 <- expr({
    if (TRUE) {
      if (TRUE) {
        !! pause_lang("2")
      }
      !! goto_lang("3")
    }
    !! goto_lang("3")
  })
  parts2 <- block("if-3-after", goto_lang("3"))
  parts3 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2, parts3))
})

test_that("`if`-`else` blocks - multiply nested and not trailing", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      "if-before"
      if (TRUE) {
        if (TRUE) {
          yield(1L)
          "if-3-after"
        }
        "if-2-after"
      } else {
        if (FALSE) {
          FALSE
        } else {
          yield(2L)
        }
      }
    } else {
      FALSE
    }
    "after"
  })

  parts1 <- expr({
    "before"
    if (TRUE) {
      "if-before"
      if (TRUE) {
        if (TRUE) {
          !! pause_lang("2")
        }
        !! goto_lang("3")
      } else {
        if (FALSE) {
          FALSE
        } else {
          !! pause_lang("4")
        }
        !! goto_lang("4")
      }
    } else {
      FALSE
    }
    !! goto_lang("4")
  })
  parts2 <- block("if-3-after", goto_lang("3"))
  parts3 <- block("if-2-after", goto_lang("4"))
  parts4 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2, parts3, parts4))
})
