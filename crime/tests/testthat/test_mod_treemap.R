testa_baseline("treemap", "treemapServer")

test_that("UI do treemap com ids namespaced e conditionalPanel", {
  ui <- as.character(treemapUI("treemap"))
  expect_match(ui, 'data-ns-prefix="treemap-"', fixed = TRUE)
  expect_match(ui, 'id="treemap-tree_map"', fixed = TRUE)
  expect_match(ui, 'id="treemap-ano_tree"', fixed = TRUE)
})
