
test_that("`repeat` - one pause", {
  expect_snapshot0(generator_body(function() {
    "before"
    repeat {
      "loop-before"
      yield(1L)
      "loop-after"
    }
    "after"
  }))
})

test_that("`repeat` - no continuation", {
  expect_snapshot0(generator_body(function() {
    "before"
    repeat yield(1L)
    "after"
  }))

  expect_snapshot0(generator_body(function() {
    "before"
    repeat {{ yield(1L) }}
    "after"
  }))
})

test_that("`repeat` - pause within `if`", {
  expect_snapshot0(generator_body(function() {
    "before"
    repeat {
      "loop-before"
      if (TRUE) yield(1L)
      "loop-after"
    }
    "after"
  }))
})

test_that("`repeat` - nested loop", {
  expect_snapshot0(generator_body(function() {
    "before"
    repeat {
      "loop-before"
      repeat yield(1L)
      "loop-after"
    }
    "after"
  }))
})

test_that("`repeat` - non-yielding", {
  expect_snapshot0(generator_body(function() {
    "before"
    repeat NULL
    yield(1L)
    "after"
  }))
})

test_that("`repeat` - non-yielding but other control flow constructs", {
  # FIXME
  expect_snapshot0(generator_body(function() {
    "before"
    repeat if (TRUE) break else next
    yield(1L)
    "after"
  }))
})

test_that("loops - single `next`", {
  expect_snapshot0(generator_body(function() {
    repeat {
      next
      yield(1L)
    }
  }))
})

test_that("loops - single `next` with past and future", {
  expect_snapshot0(generator_body(function() {
    repeat {
      "loop-before"
      yield(1L)
      "loop-after"
      next
      "next-after"
      yield(2L)
      "loop-final"
    }
  }))
})

test_that("loops - single `break`", {
  expect_snapshot0(generator_body(function() {
    repeat {
      break
      yield(1L)
    }
  }))
})

test_that("loops - `next` and `break` within `if`-`else`", {
  expect_snapshot0(generator_body(function() {
    repeat {
      "loop-after"
      if (TRUE) break
      else next
      "next-after"
    }
  }))
})

test_that("loops - `break` and `next` with past and future", {
  expect_snapshot0(generator_body(function() {
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
  }))
})

test_that("loops - goto loop start after `if` or `else`", {
  expect_snapshot0(generator_body(function() {
    repeat if (TRUE) yield()
  }))

  expect_snapshot0(generator_body(function() {
    repeat if (TRUE) yield(1L) else FALSE
  }))
})

test_that("`while` - single pause no past or future", {
  expect_snapshot0(generator_body(function() {
    while (TRUE) yield(1L)
  }))
})

test_that("`while` - pause within `if`", {
  expect_snapshot0(generator_body(function() {
    while (TRUE) {
      if (FALSE) yield(1L)
    }
  }))
})

test_that("`while` - pause within `if` with future", {
  expect_snapshot0(generator_body(function() {
    while (TRUE) {
      if (FALSE) {
        yield(1L)
        "after-pause"
      }
    }
  }))
})

test_that("`while` - past before loop", {
  expect_snapshot0(generator_body(function() {
    "before"
    while (TRUE) {
      "loop-before"
      yield(1L)
      "loop-after"
    }
  }))
})

test_that("`while` - pause after loop", {
  expect_snapshot0(generator_body(function() {
    while (TRUE) {
      yield(1L)
      "loop-after"
    }
    yield(2L)
  }))
})

test_that("`while` - complex control flow", {
  expect_snapshot0(generator_body(function() {
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
  }))
})

test_that("`while` - top level break", {
  expect_snapshot0(generator_body(function() {
    while (TRUE) {
      "before-break"
      break
    }
  }))
})

test_that("`for` - top level break (#7)", {
  expect_snapshot0(generator_body(function() {
    for (i in x) break
  }))
})

test_that("`for` - one pause with no past or future", {
  expect_snapshot0(generator_body(function() {
    for (i in x) yield(1L)
  }))
})

test_that("`for` - one pause with past and future", {
  expect_snapshot0(generator_body(function() {
    "before"
    for (i in x) {
      "for-before"
      yield(1L)
      "for-after"
    }
    "after"
  }))
})

test_that("`for` - one pause within `if` and one `break` within `else`", {
  expect_snapshot0(generator_body(function() {
    for (i in x) {
      "for-before"
      if (TRUE) yield(1L)
      else break
      "if-after"
      next
      "for-after"
    }
  }))
})

test_that("`return()` deep in control flow", {
  expect_snapshot0(generator_body(function() { while (TRUE) if (TRUE) return(1L) else yield(2L) }))
})

test_that("nested loops break to outer loops", {
  expect_snapshot0(generator_body(
    function() {
      "before"

      while (TRUE) {
        "while before if"
        if (i > 3) {
          "while-if before break"
          break
        }
        "while after if"

        while (TRUE) {
          "while-while before if"
          if (j > 3) {
            "while-while-if before break"
            break
          }
          "while-while after if"
        }

        "while after while"
      }

      "after"
    }
  ))


  expect_snapshot0(generator_body(
    function() {
      "before"

      while (TRUE) {
        "while before if"
        if (i > 3) {
          "while-if before break"
          break
        }
        "while after if"

        while (TRUE) {
          "while-while before if"
          if (j > 3) {
            "while-while-if before break"
            break
          }
          "while-while after if"
        }
      }

      "after"
    }
  ))

  expect_snapshot0(generator_body(
    function() {
      while (1) {
        while (2) {
          yield(1)
          break
        }
      }
    }
  ))
})
