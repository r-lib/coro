test_that("setup() can't be called directly or within function arguments", {
  expect_error(setup(1), "called directly")

  f <- generator(function() {
    list(setup(1))
    yield(2)
  })
  expect_error(f()(), "within function arguments")
})
