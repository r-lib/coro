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

  parts <- machine_parts(function() {
    "before"
    repeat {{ yield(1L) }}
    "after"
  })

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

test_that("`repeat` - non-yielding", {
  parts <- machine_parts(function() {
    "before"
    repeat NULL
    yield(1L)
    "after"
  })

  parts1 <- block("before", repeat_lang(NULL), pause_lang("2"))
  parts2 <- block(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2))
})

test_that("loops - single `next`", {
  parts <- machine_parts(function() {
    repeat next
  })

  parts1 <- block(goto_lang("2"))
  parts2 <- block(goto_lang("2"))
  parts3 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("loops - single `next` with past and future", {

  parts <- machine_parts(function() {
    repeat {
      "loop-before"
      yield(1L)
      "loop-after"
      next
      "next-after"
      yield(2L)
      "loop-final"
    }
  })

  parts1 <- block(goto_lang("2"))
  parts2 <- block("loop-before", pause_lang("3"))
  parts3 <- block("loop-after", goto_lang("2"))
  parts4 <- block("next-after", pause_lang("5"))
  parts5 <- block("loop-final", goto_lang("2"))
  parts6 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3, parts4, parts5, parts6))
})

test_that("loops - single `break`", {
  parts <- machine_parts(function() {
    repeat break
  })

  parts1 <- block(goto_lang("2"))
  parts2 <- block(goto_lang("3"))
  parts3 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("loops - `next` and `break` within `if`-`else`", {
  parts <- machine_parts(function() {
    repeat {
      "loop-after"
      if (TRUE) break
      else next
      "next-after"
    }
  })

  parts1 <- block(goto_lang("2"))
  parts2 <- block("loop-after", if_lang(TRUE, block(goto_lang("4")), block(goto_lang("2"))))
  parts3 <- block("next-after", goto_lang("2"))
  parts4 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3, parts4))
})

test_that("loops - `break` and `next` with past and future", {
  parts <- machine_parts(function() {
    repeat {
      "loop-before"
      yield(1L)
      "loop-after"
      break
      "break-after"
      next
      "next-after"
      yield(2L)
      "loop-final"
    }
  })

  parts1 <- block(goto_lang("2"))
  parts2 <- block("loop-before", pause_lang("3"))
  parts3 <- block("loop-after", goto_lang("7"))
  parts4 <- block("break-after", goto_lang("2"))
  parts5 <- block("next-after", pause_lang("6"))
  parts6 <- block("loop-final", goto_lang("2"))
  parts7 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3, parts4, parts5, parts6, parts7))
})

test_that("loops - goto loop start after `if` or `else`", {
  parts <- machine_parts(function() {
    repeat if (TRUE) yield()
  })

  parts1 <- block(goto_lang("2"))
  parts2 <- block(if_lang(TRUE, block(pause_lang("2"))), goto_lang("2"))
  parts3 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3))


  parts <- machine_parts(function() {
    repeat if (TRUE) yield() else FALSE
  })

  parts2 <- block(if_lang(TRUE, block(pause_lang("2")), FALSE), goto_lang("2"))

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("`while` - single pause no past or future", {
  parts <- machine_parts(function() {
    while (TRUE) yield(1L)
  })

  parts1 <- block(if_lang(TRUE, block(goto_lang("2")), block(goto_lang("3"))))
  parts2 <- block(pause_lang("1"))
  parts3 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("`while` - pause within `if`", {
  parts <- machine_parts(function() {
    while (TRUE) {
      if (FALSE) yield(1L)
    }
  })

  parts1 <- block(if_lang(TRUE, block(goto_lang("2")), block(goto_lang("3"))))
  parts2 <- block(if_lang(FALSE, block(pause_lang("1"))), goto_lang("1"))
  parts3 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("`while` - pause within `if` with future", {
  parts <- machine_parts(function() {
    while (TRUE) {
      if (FALSE) {
        yield(1L)
        "after-pause"
      }
    }
  })

  parts1 <- block(if_lang(TRUE, block(goto_lang("2")), block(goto_lang("4"))))
  parts2 <- block(if_lang(FALSE, block(pause_lang("3"))), goto_lang("1"))
  parts3 <- block("after-pause", goto_lang("1"))
  parts4 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3, parts4))
})

test_that("`while` - past before loop", {
  parts <- machine_parts(function() {
    "before"
    while (TRUE) {
      "loop-before"
      yield(1L)
      "loop-after"
    }
  })

  parts1 <- block("before")
  parts2 <- block(if_lang(TRUE, block(goto_lang("3")), block(goto_lang("5"))))
  parts3 <- block("loop-before", pause_lang("4"))
  parts4 <- block("loop-after", goto_lang("2"))
  parts5 <- block(return_invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3, parts4, parts5))
})

test_that("`for` - one pause with no past or future", {
  parts <- machine_parts(function() {
    for (i in x) yield(1L)
  })

  for_parts <- new_for_parts(1L, quote(i), quote(x))
  parts1 <- node_car(for_parts)
  parts2 <- node_cadr(for_parts)
  parts3 <- block(pause_lang("2"))
  parts4 <- block(return_invisible_lang)

  expect_identical(node_list_tail_car(parts1), goto_lang("2"))
  expect_identical(node_list_tail_car(parts2), goto_lang("3"))
  expect_identical(parts, node_list(parts1, parts2, parts3, parts4))
})
