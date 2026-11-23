testa_baseline("compara_crimes", "comparaCrimesServer")
testa_baseline("compara_municipios", "comparaMunicipiosServer")

test_that("UIs das séries com ids namespaced", {
  ui_cc <- as.character(comparaCrimesUI("compara_crimes"))
  expect_match(ui_cc, 'id="compara_crimes-ts_compara_crime_cidades"', fixed = TRUE)
  expect_match(ui_cc, 'id="compara_crimes-crimes_compara_crimes"', fixed = TRUE)
  ui_cm <- as.character(comparaMunicipiosUI("compara_municipios"))
  expect_match(ui_cm, 'data-ns-prefix="compara_municipios-"', fixed = TRUE)
  expect_match(ui_cm, 'id="compara_municipios-ts_compara_cidades"', fixed = TRUE)
})
