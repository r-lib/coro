test_that("`if` blocks - one pause", {
  expect_snapshot0(generator_body(function() {
    "before"
    if (TRUE) {
      "if-before"
      yield(1L)
      "if-after"
    } else {
      FALSE
    }
    "after"
  }))
})

test_that("`else` blocks - one pause", {
  expect_snapshot0(generator_body(function() {
    "before"
    if (FALSE) {
      FALSE
    } else {
      "else-before"
      yield(1L)
      "else-after"
    }
    "after"
  }))
})

test_that("`if` blocks - inner block", {
  skip()
  expect_snapshot0(generator_body(function() {
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
  }))
})

test_that("`if` blocks - nested", {
  expect_snapshot0(generator_body(function() {
    "before"
    if (TRUE) {
      "if-before"
      if (FALSE) yield(1L)
      "if-after"
    } else {
      "foo"
    }
  }))
})

test_that("`if` blocks - nested and trailing pause", {
  expect_snapshot0(generator_body(function() {
    "before"
    if (TRUE) {
      "if-before"
      if (FALSE) yield(1L)
    } else {
      "foo"
    }
  }))
})

test_that("`if` blocks - multiply nested and all trailing", {
  expect_snapshot0(generator_body(function() {
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
  }))
})

test_that("`if`-`else` blocks - trailing", {
  expect_snapshot0(generator_body(function() {
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
  }))
})

test_that("`if`-`else` blocks - non trailing", {
  expect_snapshot0(generator_body(function() {
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
  }))
})

test_that("`if`-`else` blocks - same continuation", {
  expect_snapshot0(generator_body(function() {
    "before"
    if (TRUE) {
      yield(1L)
    } else {
      yield(2L)
    }
    "after"
  }))
})

test_that("`if`-`else` blocks - continuation in `if`", {
  expect_snapshot0(generator_body(function() {
    "before"
    if (TRUE) {
      yield(1L)
      "if-after"
    } else {
      yield(2L)
    }
    "after"
  }))
})

test_that("`if`-`else` blocks - continuation in `else`", {
  expect_snapshot0(generator_body(function() {
    "before"
    if (TRUE) {
      yield(1L)
    } else {
      yield(2L)
      "else-after"
    }
    "after"
  }))
})

test_that("`if` blocks - doubly nested with continuation", {
  expect_snapshot0(generator_body(function() {
    if (TRUE) {
      if (TRUE) {
        yield(1L)
        "if-3-after"
      }
    }
    "after"
  }))
})

test_that("`if`-`else` blocks - multiply nested and not trailing", {
  expect_snapshot0(generator_body(function() {
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
  }))
})
