context("node-list")

test_that("node list pokers poke list tails", {
  l <- node_list(1L, 2L)

  node_list_poke_car(l, 20L)
  expect_identical(l, node_list(1L, 20L))

  node_list_poke_cdr(l, node_list(3L))
  expect_identical(l, node_list(1L, 20L, 3L))
})

test_that("node list pokers handle empty list", {
  expect_identical(node_list_poke_car(NULL, 1L), node_list(1L))
  expect_identical(node_list_poke_cdr(NULL, node_list(1L)), node_list(1L))
})


context("attrs")

test_that("poke_attr() modifies attributes in place", {
  x <- 1:3
  poked <- poke_attr(x, "foo", 1L)

  expect_true(is_reference(x, poked))
  expect_identical(attributes(x), list(foo = 1L))

  poked <- poke_attr(x, "foo", 2L)
  expect_true(is_reference(x, poked))
  expect_identical(attributes(x), list(foo = 2L))
})

test_that("poke_attr() removes attributes in place", {
  x <- 1:3
  poked <- poke_attr(x, "foo", NULL)

  expect_null(attributes(poked))
  expect_true(is_reference(poked, x))

  x <- poke_attr(x, "foo", 1L)
  poked <- poke_attr(x, "foo", NULL)

  expect_null(attr(poked, "foo"))
  expect_true(is_reference(poked, x))
})
