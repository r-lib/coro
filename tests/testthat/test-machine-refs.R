
test_that("references are propagated with yield", {
  expect_snapshot(print_parts_refs(function() {
    yield(1)
    2
  }))

  expect_snapshot(print_parts_refs(function() {
    1
    yield(2)
    3
  }))
})

test_that("references are propagated in repeat loops", {
  expect_snapshot(print_parts_refs(function() {
    repeat yield(1)
  }))

  expect_snapshot(print_parts_refs(function() {
    repeat {
      1
      break
      3
    }
  }))
})

test_that("references are propagated in while loops", {
  expect_snapshot(print_parts_refs(function() {
    while (TRUE) next
    while (TRUE) break
  }))

  expect_snapshot(print_parts_refs(function() {
    1
    while (TRUE) {
      2
      next
      3
      break
      4
    }
    5
  }))
})
