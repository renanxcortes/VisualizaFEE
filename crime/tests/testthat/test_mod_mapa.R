testa_baseline("mapa", "mapaServer")

test_that("UI do mapa com ids namespaced, popover, tooltip e conditionalPanels", {
  ui <- as.character(mapaUI("mapa"))
  expect_match(ui, 'data-ns-prefix="mapa-"', fixed = TRUE)
  for (id in c('id="mapa-ano_mapa"', 'id="mapa-mapa_final"', 'id="mapa-gauge_moran"',
               '"mapa-q_moran"'))
    expect_match(ui, id, fixed = TRUE)
  # bsTooltip do gauge aponta para o id com namespace
  expect_match(ui, "mapa-gauge_moran.*tooltip|tooltip.*mapa-gauge_moran")
  expect_false(grepl('"q_moran"', ui, fixed = TRUE))
  # todos os conditionalPanel (6) têm o prefixo
  expect_equal(lengths(regmatches(ui, gregexpr('data-ns-prefix="mapa-"', ui))), 6)
})
