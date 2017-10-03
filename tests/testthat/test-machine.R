context("machine")

test_that("`{` blocks - one pause", {
  parts <- machine_parts(function() {
    "before1"
    "before2"
    yield()
    "after1"
    "after2"
  })

  parts1 <- block("before1", "before2", pause_lang("2"))
  parts2 <- block("after1", return_lang("after2"))

  expect_identical(parts, node_list(parts1, parts2))
})

test_that("`{` blocks - no preamble", {
  parts <- machine_parts(function() {
    yield()
    "after"
  })

  parts1 <- block(pause_lang(2))
  parts2 <- block(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2))
})

test_that("`{` blocks - multiple pauses", {
  parts <- machine_parts(function() {
    "before"
    yield()
    "during"
    yield()
    "after"
  })

  parts1 <- block("before", pause_lang("2"))
  parts2 <- block("during", pause_lang("3"))
  parts3 <- block(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("`{` blocks - consecutive pauses", {
  parts <- machine_parts(function() {
    "before"
    yield()
    yield()
    "after"
  })

  parts1 <- block("before", pause_lang("2"))
  parts2 <- block(pause_lang("3"))
  parts3 <- block(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("`{` blocks - no return value", {
  parts <- machine_parts(function() {
    yield()
  })

  parts1 <- block(pause_lang("2"))
  parts2 <- block(invisible_lang)

  expect_identical(parts, node_list(parts1, parts2))


  parts <- machine_parts(function() {
    yield()
    yield()
  })

  parts1 <- block(pause_lang("2"))
  parts2 <- block(pause_lang("3"))
  parts3 <- block(invisible_lang)

  expect_identical(parts, node_list(parts1, parts2, parts3))
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

  parts1_block <- block("before-inner", pause_lang("2"))
  parts1 <- block("before1", "before2", parts1_block)
  parts2 <- block("after-inner", goto_lang("3"))
  parts3 <- block("after1", return_lang("after2"))

  expect_identical(parts, node_list(parts1, parts2, parts3))
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

  parts1 <- block("before-inner", pause_lang("2"))
  parts2 <- block("after-inner", goto_lang("3"))
  parts3 <- block("after1", return_lang("after2"))

  expect_identical(parts, node_list(parts1, parts2, parts3))
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

  parts1 <- block("before-inner", pause_lang("2"))
  parts2 <- block("after1", return_lang("after2"))

  expect_identical(parts, node_list(parts1, parts2))
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

  inner1 <- block("before-inner", pause_lang("2"))
  parts1 <- block("before", inner1)
  parts2 <- block(pause_lang("3"))
  parts3 <- block(pause_lang("4"))
  parts4 <- block("after-inner", goto_lang("5"))
  parts5 <- block(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2, parts3, parts4, parts5))
})
