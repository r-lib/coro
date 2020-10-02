
test_that("generators have return states", {
  expect_snapshot0(generator_body(function() "foo"))
  expect_snapshot0(generator_body(function() return("foo")))
})
