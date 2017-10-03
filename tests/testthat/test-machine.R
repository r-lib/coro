context("machine")

test_that("`{` blocks - one pause", {
  parts <- machine_parts(function() {
    "before1"
    "before2"
    yield()
    "after1"
    "after2"
  })

  parts1 <- node_list("before1", "before2", pause_lang("2"))
  parts2 <- tail_list("after1", return_lang("after2"))

  expect_identical(parts, node_list(parts1, parts2))
})

test_that("`{` blocks - no preamble", {
  parts <- machine_parts(function() {
    yield()
    "after"
  })

  parts1 <- node_list(pause_lang(2))
  parts2 <- tail_list(return_lang("after"))

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

  parts1 <- node_list("before", pause_lang("2"))
  parts2 <- node_list("during", pause_lang("3"))
  parts3 <- tail_list(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("`{` blocks - consecutive pauses", {
  parts <- machine_parts(function() {
    "before"
    yield()
    yield()
    "after"
  })

  parts1 <- node_list("before", pause_lang("2"))
  parts2 <- node_list(pause_lang("3"))
  parts3 <- tail_list(return_lang("after"))

  expect_identical(parts, node_list(parts1, parts2, parts3))
})

test_that("`{` blocks - no return value", {
  parts <- machine_parts(function() {
    yield()
  })

  parts1 <- node_list(pause_lang("2"))
  parts2 <- tail_list(return_lang(NULL))

  expect_identical(parts, node_list(parts1, parts2))


  parts <- machine_parts(function() {
    yield()
    yield()
  })

  parts1 <- node_list(pause_lang("2"))
  parts2 <- node_list(pause_lang("3"))
  parts3 <- tail_list(return_lang(NULL))

  expect_identical(parts, node_list(parts1, parts2, parts3))
})
