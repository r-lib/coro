context("machine-block")

test_that("`{` blocks - one pause with no past or future", {
  parts <- machine_parts(function() {
    yield(1L)
  })

  parts1 <- block(pause_call("2", 1L))
  parts2 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2))
})

test_that("`{` blocks - one pause", {
  parts <- machine_parts(function() {
    "before1"
    "before2"
    yield(1L)
    "after1"
    "after2"
  })

  parts1 <- block("before1", "before2", pause_call("2", 1L))
  parts2 <- block("after1", return_call("after2"))

  expect_identical(parts, pairlist(parts1, parts2))
})

test_that("`{` blocks - no preamble", {
  parts <- machine_parts(function() {
    yield(1L)
    "after"
  })

  parts1 <- block(pause_call("2", 1L))
  parts2 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2))
})

test_that("`{` blocks - multiple pauses", {
  parts <- machine_parts(function() {
    "before"
    yield(1L)
    "during"
    yield(2L)
    "after"
  })

  parts1 <- block("before", pause_call("2", 1L))
  parts2 <- block("during", pause_call("3", 2L))
  parts3 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2, parts3))
})

test_that("`{` blocks - consecutive pauses", {
  parts <- machine_parts(function() {
    "before"
    yield(1L)
    yield(2L)
    "after"
  })

  parts1 <- block("before", pause_call("2", 1L))
  parts2 <- block(pause_call("3", 2L))
  parts3 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2, parts3))
})

test_that("`{` blocks - no return value", {
  parts <- machine_parts(function() {
    yield(1L)
  })

  parts1 <- block(pause_call("2", 1L))
  parts2 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2))


  parts <- machine_parts(function() {
    yield(1L)
    yield(2L)
  })

  parts1 <- block(pause_call("2", 1L))
  parts2 <- block(pause_call("3", 2L))
  parts3 <- block(return_invisible_call)

  expect_identical(parts, pairlist(parts1, parts2, parts3))
})

test_that("`{` blocks - nested", {
  parts <- machine_parts(function() {
    "before1"
    "before2"
    {
      "before-inner"
      yield(1L)
      "after-inner"
    }
    "after1"
    "after2"
  })

  parts1_block <- block("before-inner", pause_call("2", 1L))
  parts1 <- block("before1", "before2", parts1_block)
  parts2 <- block("after-inner", goto_call("3"))
  parts3 <- block("after1", return_call("after2"))

  expect_identical(parts, pairlist(parts1, parts2, parts3))
})

test_that("`{` blocks - nested and no past before pause", {
  parts <- machine_parts(function() {
    {
      "before-inner"
      yield(1L)
      "after-inner"
    }
    "after1"
    "after2"
  })

  parts1 <- block("before-inner", pause_call("2", 1L))
  parts2 <- block("after-inner", goto_call("3"))
  parts3 <- block("after1", return_call("after2"))

  expect_identical(parts, pairlist(parts1, parts2, parts3))
})

test_that("`{` blocks - nested and goto after pause", {
  parts <- machine_parts(function() {
    {
      "before-inner"
      yield(1L)
    }
    "after1"
    "after2"
  })

  parts1 <- block("before-inner", pause_call("2", 1L))
  parts2 <- block("after1", return_call("after2"))

  expect_identical(parts, pairlist(parts1, parts2))
})

test_that("`{` blocks - complex nesting", {
  parts <- machine_parts(function() {
    "before"
    {
      "before-inner"
      yield(1L)
      {
        yield(2L)
        yield(3L)
      }
      "after-inner"
    }
    "after"
  })

  inner1 <- block("before-inner", pause_call("2", 1L))
  parts1 <- block("before", inner1)
  parts2 <- block(pause_call("3", 2L))
  parts3 <- block(pause_call("4", 3L))
  parts4 <- block("after-inner", goto_call("5"))
  parts5 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2, parts3, parts4, parts5))
})

test_that("`{` blocks - simple nesting with various continuation states", {
  parts <- machine_parts(function() {
    {
      {
        yield(1L)
        "after-inner-inner"
      }
    }
    "after"
  })

  parts1 <- block(pause_call("2", 1L))
  parts2 <- block("after-inner-inner", goto_call("3"))
  parts3 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2, parts3))


  parts <- machine_parts(function() {
    {
      {
        yield(1L)
      }
      "after-inner"
    }
    "after"
  })

  parts1 <- block(pause_call("2", 1L))
  parts2 <- block("after-inner", goto_call("3"))
  parts3 <- block(return_call("after"))

  expect_identical(parts, pairlist(parts1, parts2, parts3))
})
