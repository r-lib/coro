
test_that("node_list_walk_at() applies to correct cell", {
  x <- pairlist(1L, 2L, 3L)
  out <- node_list_walk_at(x, node_cdr(x), node_poke_car, 20L)
  expect_identical(node_cadr(x), 20L)

  expect_error(node_list_walk_at(x, pairlist(1)), "Can't find `.at`")
})

test_that("node_list_walk_at_parent() applies to correct cell", {
  x <- pairlist(1L, 2L, 3L)
  out <- node_list_walk_at_parent(x, node_cdr(x), node_poke_car, 10L)
  expect_identical(node_car(x), 10L)

  expect_error(node_list_walk_at_parent(x, pairlist(1)), "Can't find `.at`")
  expect_error(node_list_walk_at_parent(x, x), "`.at` has no parent")
})
