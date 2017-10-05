context("machine-loop")

test_that("`repeat` - one pause", {
  parts <- machine_parts(function() {
    "before"
    repeat {
      "loop-before"
      yield(1L)
      "loop-after"
    }
    "after"
  })

  parts1 <- block("before", goto_lang("2"))
  parts2 <- block("loop-before", pause_lang("3"))
  parts3 <- block("loop-after", goto_lang("2"))
  parts4 <- block(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2, parts3, parts4))
})

test_that("`repeat` - no continuation", {
  parts <- machine_parts(function() {
    "before"
    repeat yield(1L)
    "after"
  })

  parts1 <- block("before", goto_lang("2"))
  parts2 <- block(pause_lang("2"))
  parts3 <- block(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("`repeat` - pause within `if`", {
  parts <- machine_parts(function() {
    "before"
    repeat {
      "loop-before"
      if (TRUE) yield(1L)
      "loop-after"
    }
    "after"
  })

  parts1 <- block("before", goto_lang("2"))
  parts2 <- expr({
    "loop-before"
    if (TRUE) {
      !! pause_lang("3")
    }
    !! goto_lang("3")
  })
  parts3 <- block("loop-after", goto_lang("2"))
  parts4 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2, parts3, parts4))
})

test_that("`repeat` - nested loop", {
  parts <- machine_parts(function() {
    "before"
    repeat {
      "loop-before"
      repeat yield(1L)
      "loop-after"
    }
    "after"
  })

  parts1 <- block("before", goto_lang("2"))
  parts2 <- block("loop-before", goto_lang("3"))
  parts3 <- block(pause_lang("3"))
  parts4 <- block("loop-after", goto_lang("2"))
  parts5 <- block(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2, parts3, parts4, parts5))
})
