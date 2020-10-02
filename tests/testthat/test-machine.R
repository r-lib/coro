
test_that("generators have return states", {
  expect_snapshot0(generator_body(function() "foo"))
  expect_snapshot0(generator_body(function() return("foo")))
})

test_that("generators have yield states", {
  expect_snapshot0(generator_body(function() yield("foo")))
  expect_snapshot0(generator_body(function() flowery::yield("foo")))
})
