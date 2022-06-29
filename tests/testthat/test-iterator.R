test_that("can use reticulate iterators", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  skip_on_ci()

  reticulate::py_run_string("
def first_n(n):
    num = 1
    while num <= n:
        yield num
        num += 1
")

  expect_equal(
    collect(reticulate::py$first_n(3)),
    as.list(1:3)
  )

  out <- NULL
  loop(for (x in reticulate::py$first_n(3)) {
    out <- c(out, x)
  })
  expect_equal(out, 1:3)


})


test_that("can use reticulate iteratables", {
  skip_if_not_installed("reticulate")
  skip_on_cran()
  skip_on_ci()

  i <- 0L
  loop(for (el in reticulate::py_eval("range(3)")) {
    i <- i + el
  })
  expect_equal(i, 3L)
})


