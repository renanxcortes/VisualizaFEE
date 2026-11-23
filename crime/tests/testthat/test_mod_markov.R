testa_baseline("markov", "markovServer")

test_that("UI do Markov com ids namespaced e conditionalPanels", {
  ui <- as.character(markovUI("markov"))
  expect_match(ui, 'id="markov-mapas_markov"', fixed = TRUE)
  expect_match(ui, 'id="markov-crime_markov"', fixed = TRUE)
  expect_match(ui, 'id="markov-tabela_markov_estratificada"', fixed = TRUE)
  n_cond <- lengths(regmatches(ui, gregexpr("data-display-if", ui)))
  n_ns <- lengths(regmatches(ui, gregexpr('data-ns-prefix="markov-"', ui)))
  expect_gt(n_cond, 0)
  expect_equal(n_ns, n_cond)  # todo conditionalPanel tem o prefixo
})
