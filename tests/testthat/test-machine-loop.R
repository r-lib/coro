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

  parts1 <- block("before", block("loop-before", pause_lang("2")))
  parts2 <- block("loop-after", goto_lang("1"))
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

  parts1 <- expr({
    "before"
    {
      "loop-before"
      if (TRUE) {
        !! pause_lang("2")
      }
      !! goto_lang("2")
    }
  })
  parts2 <- block("loop-after", goto_lang("1"))
  parts3 <- block(return_lang("after"))

  expect_equal(parts, node_list(parts1, parts2, parts3))
})
