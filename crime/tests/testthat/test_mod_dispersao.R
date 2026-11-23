testa_baseline("dispersao", "dispersaoServer")

test_that("UI da dispersão com ids namespaced, popover e conditionalPanel", {
  ui <- as.character(dispersaoUI("dispersao"))
  expect_match(ui, 'data-ns-prefix="dispersao-"', fixed = TRUE)
  expect_match(ui, 'id="dispersao-dispersao"', fixed = TRUE)
  expect_match(ui, '"dispersao-q_kmeans"', fixed = TRUE)  # bsButton + bsPopover
  expect_false(grepl('"q_kmeans"', ui, fixed = TRUE))
})

test_that("k-means desenha sem erro", {
  inputs <- modifyList(cenarios$dispersao$inputs, list(checkbox_kmeans = TRUE, n_grupos_kmeans = 3))
  set.seed(1)
  res <- captura(get("dispersaoServer"), inputs, "dispersao")
  expect_type(res$dispersao, "list")
})
