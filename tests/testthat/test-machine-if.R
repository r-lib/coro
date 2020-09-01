
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

  inner1 <- if_call(TRUE, block("if-before", pause_call("2", 1L)), block(FALSE))
  parts1 <- block("before", inner1, goto_call("3"))
  parts2 <- block("if-after", goto_call("3"))
  parts3 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3))
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

  inner1 <- if_call(FALSE, block(FALSE), block("else-before", pause_call("2", 1L)))
  parts1 <- block("before", inner1, goto_call("3"))
  parts2 <- block("else-after", goto_call("3"))
  parts3 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3))
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

  inner1 <- block("inner-before", pause_call("2", 1L))
  inner1 <- if_call(TRUE, block("if-before", inner1), block(FALSE))
  parts1 <- block("before", inner1, goto_call("4"))
  parts2 <- block("inner-after", goto_call("3"))
  parts3 <- block("if-after", goto_call("4"))
  parts4 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3, parts4))
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

  inner1 <- if_call(FALSE, block(pause_call("2", 1L)))
  inner1 <- block("if-before", inner1, goto_call("2"))
  inner1 <- if_call(TRUE, inner1, block(return_call("foo")))
  parts1 <- block("before", inner1)
  parts2 <- block(return_call("if-after"))

  expect_equal(parts, pairlist(parts1, parts2))
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

  inner1 <- if_call(FALSE, block(pause_call("2", 1L)), block(return_invisible_call))
  inner1 <- block("if-before", inner1)
  inner1 <- if_call(TRUE, inner1, block(return_call("foo")))
  parts1 <- block("before", inner1)
  parts2 <- block(return_invisible_call)

  expect_equal(parts, pairlist(parts1, parts2))
})

test_that("`if` blocks - multiply nested and all trailing", {
  parts <- machine_parts(function() {
    "before"
    if (TRUE) {
      "if-before"
      if (FALSE) {
        if (FALSE) {
          yield(1L)
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
          !!pause_call("2", 1L)
        }
        !!goto_call("3")
      } else {
        !!return_invisible_call
      }
    } else {
      return(FALSE)
    }
  })
  parts2 <- block("if-3-after", goto_call("3"))
  parts3 <- block(return_call("if-2-after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3))
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
      yield(2L)
      "else-after"
    }
  })

  parts1 <- expr({
    "before"
    if (TRUE) {
      "if-before"
      !!pause_call("2", 1L)
    } else {
      "else-before"
      !!pause_call("3", 2L)
    }
  })
  parts2 <- block(return_call("if-after"))
  parts3 <- block(return_call("else-after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3))
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

  inner1 <- if_call(TRUE, block("if-before", pause_call("2", 1L)), block("else-before", pause_call("3", 2L)))
  parts1 <- block("before", inner1)
  parts2 <- block("if-after", goto_call("4"))
  parts3 <- block("else-after", goto_call("4"))
  parts4 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3, parts4))
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
      !!pause_call("2", 1L)
    } else {
      !!pause_call("2", 2L)
    }
  })
  parts2 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2))
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
      !!pause_call("2", 1L)
    } else {
      !!pause_call("3", 2L)
    }
  })
  parts2 <- block("if-after", goto_call("3"))
  parts3 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3))
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
      !!pause_call("3", 1L)
    } else {
      !!pause_call("2", 2L)
    }
  })
  parts2 <- block("else-after", goto_call("3"))
  parts3 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3))
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
        !!pause_call("2", 1L)
      }
      !!goto_call("3")
    }
    !!goto_call("3")
  })
  parts2 <- block("if-3-after", goto_call("3"))
  parts3 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3))
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
          !!pause_call("2", 1L)
        }
        !!goto_call("3")
      } else {
        if (FALSE) {
          FALSE
        } else {
          !!pause_call("4", 2L)
        }
        !!goto_call("4")
      }
    } else {
      FALSE
    }
    !!goto_call("4")
  })
  parts2 <- block("if-3-after", goto_call("3"))
  parts3 <- block("if-2-after", goto_call("4"))
  parts4 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3, parts4))
})
