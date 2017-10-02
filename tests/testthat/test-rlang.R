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
