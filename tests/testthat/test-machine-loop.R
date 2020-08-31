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

  parts1 <- block("before", goto_call("2"))
  parts2 <- block("loop-before", pause_call("3", 1L))
  parts3 <- block("loop-after", goto_call("2"))
  parts4 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4))
})

test_that("`repeat` - no continuation", {
  parts <- machine_parts(function() {
    "before"
    repeat yield(1L)
    "after"
  })

  parts1 <- block("before", goto_call("2"))
  parts2 <- block(pause_call("2", 1L))
  parts3 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2, parts3))

  parts <- machine_parts(function() {
    "before"
    repeat {{ yield(1L) }}
    "after"
  })

  expect_identical(parts, pairlist(parts1, parts2, parts3))
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

  parts1 <- block("before", goto_call("2"))
  parts2 <- expr({
    "loop-before"
    if (TRUE) {
      !!pause_call("3", 1L)
    }
    !!goto_call("3")
  })
  parts3 <- block("loop-after", goto_call("2"))
  parts4 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3, parts4))
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

  parts1 <- block("before", goto_call("2"))
  parts2 <- block("loop-before", goto_call("3"))
  parts3 <- block(pause_call("3", 1L))
  parts4 <- block("loop-after", goto_call("2"))
  parts5 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4, parts5))
})

test_that("`repeat` - non-yielding", {
  parts <- machine_parts(function() {
    "before"
    repeat NULL
    yield(1L)
    "after"
  })

  parts1 <- block("before", repeat_call(NULL), pause_call("2", 1L))
  parts2 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2))
})

test_that("`repeat` - non-yielding but other control flow constructs", {
  skip("TODO?")
  parts <- machine_parts(function() {
    "before"
    repeat if (TRUE) break else next
    yield(1L)
    "after"
  })

  inner1 <- repeat_call(if_call(TRUE, break_call(), next_call()))
  parts1 <- block("before", inner1, pause_call("2", 1L))
  parts2 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2))
})

test_that("loops - single `next`", {
  parts <- machine_parts(function() {
    repeat {
      next
      yield(1L)
    }
  })

  parts1 <- block(goto_call("2"))
  parts2 <- block(goto_call("2"))
  parts3 <- block(pause_call("2", 1L))
  parts4 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4))
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

  parts1 <- block(goto_call("2"))
  parts2 <- block("loop-before", pause_call("3", 1L))
  parts3 <- block("loop-after", goto_call("2"))
  parts4 <- block("next-after", pause_call("5", 2L))
  parts5 <- block("loop-final", goto_call("2"))
  parts6 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4, parts5, parts6))
})

test_that("loops - single `break`", {
  parts <- machine_parts(function() {
    repeat {
      break
      yield(1L)
    }
  })

  parts1 <- block(goto_call("2"))
  parts2 <- block(goto_call("4"))
  parts3 <- block(pause_call("2", 1L))
  parts4 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4))
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

  parts1 <- block(goto_call("2"))
  parts2 <- block("loop-after", if_call(TRUE, block(goto_call("4")), block(goto_call("2"))))
  parts3 <- block("next-after", goto_call("2"))
  parts4 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4))
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

  parts1 <- block(goto_call("2"))
  parts2 <- block("loop-before", pause_call("3", 1L))
  parts3 <- block("loop-after", goto_call("7"))
  parts4 <- block("break-after", goto_call("2"))
  parts5 <- block("next-after", pause_call("6", 2L))
  parts6 <- block("loop-final", goto_call("2"))
  parts7 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4, parts5, parts6, parts7))
})

test_that("loops - goto loop start after `if` or `else`", {
  parts <- machine_parts(function() {
    repeat if (TRUE) yield()
  })

  parts1 <- block(goto_call("2"))
  parts2 <- block(if_call(TRUE, block(pause_call("2"))), goto_call("2"))
  parts3 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3))


  parts <- machine_parts(function() {
    repeat if (TRUE) yield(1L) else FALSE
  })

  parts2 <- block(if_call(TRUE, block(pause_call("2", 1L)), FALSE), goto_call("2"))

  expect_identical(parts, pairlist(parts1, parts2, parts3))
})

test_that("`while` - single pause no past or future", {
  parts <- machine_parts(function() {
    while (TRUE) yield(1L)
  })

  parts1 <- block(if_call(TRUE, block(goto_call("2")), block(goto_call("3"))))
  parts2 <- block(pause_call("1", 1L))
  parts3 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3))
})

test_that("`while` - pause within `if`", {
  parts <- machine_parts(function() {
    while (TRUE) {
      if (FALSE) yield(1L)
    }
  })

  parts1 <- block(if_call(TRUE, block(goto_call("2")), block(goto_call("3"))))
  parts2 <- block(if_call(FALSE, block(pause_call("1", 1L))), goto_call("1"))
  parts3 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3))
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

  parts1 <- block(if_call(TRUE, block(goto_call("2")), block(goto_call("4"))))
  parts2 <- block(if_call(FALSE, block(pause_call("3", 1L))), goto_call("1"))
  parts3 <- block("after-pause", goto_call("1"))
  parts4 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4))
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

  parts1 <- block("before", goto_call("2"))
  parts2 <- block(if_call(TRUE, block(goto_call("3")), block(goto_call("5"))))
  parts3 <- block("loop-before", pause_call("4", 1L))
  parts4 <- block("loop-after", goto_call("2"))
  parts5 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4, parts5))
})

test_that("`while` - pause after loop", {
  parts <- machine_parts(function() {
    while (TRUE) {
      yield(1L)
      "loop-after"
    }
    yield(2L)
  })

  parts1 <- block(if_call(TRUE, block(goto_call("2")), block(goto_call("4"))))
  parts2 <- block(pause_call("3", 1L))
  parts3 <- block("loop-after", goto_call("1"))
  parts4 <- block(pause_call("5", 2L))
  parts5 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4, parts5))
})

test_that("`while` - complex control flow", {
  parts <- machine_parts(function() {
    "before"
    while (TRUE) break
    while (TRUE) {
      "loop-before"
      yield(1L)
      "loop-after"
      if (TRUE) {
        "break-before"
        break
        "break-after"
      } else {
        "yield-2-before"
        yield(2L)
        "yield-2-after"
      }
      "next-before"
      next
      "loop-end"
    }
    "after"
  })

  parts1 <- block("before", goto_call("2"))
  parts2 <- block(if_call(TRUE, block(goto_call("3")), block(goto_call("4"))))
  parts3 <- block(goto_call("4"))
  parts4 <- block(if_call(TRUE, block(goto_call("5")), block(goto_call("11"))))
  parts5 <- block("loop-before", pause_call("6", 1L))
  inner6 <- if_call(TRUE, block("break-before", goto_call("11")), block("yield-2-before", pause_call("8", 2L)))
  parts6 <- block("loop-after", inner6)
  parts7 <- block("break-after", goto_call("9"))
  parts8 <- block("yield-2-after", goto_call("9"))
  parts9 <- block("next-before", goto_call("4"))
  parts10 <- block("loop-end", goto_call("4"))
  parts11 <- block(return_call("after"))

  expect_equal(parts, pairlist(parts1, parts2, parts3, parts4, parts5, parts6, parts7, parts8, parts9, parts10, parts11))
})

test_that("`while` - top level break", {
  parts <- machine_parts(function() {
    while (TRUE) {
      "before-break"
      break
    }
  })

  parts1 <- block(if_call(TRUE, block(goto_call("2")), block(goto_call("3"))))
  parts2 <- block("before-break", goto_call("3"))
  parts3 <- block(return_invisible_call)

  expect_equal(parts, pairlist(parts1, parts2, parts3))
})

test_that("`for` - top level break (#7)", {
  parts <- machine_parts(function() {
    for (i in x) break
  })

  parts1 <- block(goto_call("4"))
  parts2 <- block(return_invisible_call)

  expect_equal(unstructure(node_cddr(parts)), pairlist(parts1, parts2))
})

test_that("`for` - one pause with no past or future", {
  parts <- machine_parts(function() {
    for (i in x) yield(1L)
  })

  for_parts <- new_for_parts(1L, quote(i), quote(x))
  parts1 <- node_car(for_parts)
  parts2 <- node_cadr(for_parts)
  parts3 <- block(pause_call("2", 1L))
  parts4 <- block(return_invisible_call)

  expect_identical(node_list_tail_car(parts1), goto_call("2"))
  cond_branches <- node_cddr(node_cadr(parts2))
  expect_identical(node_list_tail_car(node_car(cond_branches)), goto_call("3"))

  expect_equal(parts, pairlist(parts1, parts2, parts3, parts4))
})

test_that("`for` - one pause with past and future", {
  parts <- machine_parts(function() {
    "before"
    for (i in x) {
      "for-before"
      yield(1L)
      "for-after"
    }
    "after"
  })

  for_parts <- new_for_parts(1L, quote(i), quote(x), "5")
  parts1 <- new_block(new_node("before", node_cdar(for_parts)))
  parts2 <- node_cadr(for_parts)
  parts3 <- block("for-before", pause_call("4", 1L))
  parts4 <- block("for-after", goto_call("2"))
  parts5 <- block(return_call("after"))

  expect_identical(node_list_tail_car(parts1), goto_call("2"))
  cond_branches <- node_cddr(node_cadr(parts2))
  expect_identical(node_list_tail_car(node_car(cond_branches)), goto_call("3"))

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4, parts5))
})

test_that("`for` - one pause within `if` and one `break` within `else`", {

  parts <- machine_parts(function() {
    for (i in x) {
      "for-before"
      if (TRUE) yield(1L)
      else break
      "if-after"
      next
      "for-after"
    }
  })

  for_parts <- new_for_parts(1L, quote(i), quote(x), "6")
  parts1 <- node_car(for_parts)
  parts2 <- node_cadr(for_parts)
  parts3 <- block("for-before", if_call(TRUE, block(pause_call("4", 1L)), block(goto_call("6"))))
  parts4 <- block("if-after", goto_call("2"))
  parts5 <- block("for-after", goto_call("2"))
  parts6 <- block(return_invisible_call)

  expect_equal(parts, pairlist(parts1, parts2, parts3, parts4, parts5, parts6))
})
