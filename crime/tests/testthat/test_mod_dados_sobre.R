test_that("dados: tabela igual ao baseline e download em latin1", {
  shiny::testServer(dadosServer, {
    # "options" fica de fora: o código original chama base::options() (bug pré-existente),
    # então esse campo depende do estado global da sessão R e não do módulo.
    sem_options <- function(x) x[names(x) != "options"]
    expect_identical(sem_options(normaliza(output$tabela)), sem_options(baseline$dados$tabela))
    arq <- output$downloadData
    expect_equal(nrow(read.csv(arq, fileEncoding = "latin1")), nrow(base_crime))
  })
})

test_that("UIs de dados e sobre com ids namespaced", {
  ui_dados <- as.character(dadosUI("dados"))
  expect_match(ui_dados, 'id="dados-tabela"', fixed = TRUE)
  expect_match(ui_dados, 'id="dados-downloadData"', fixed = TRUE)
  expect_match(as.character(sobreUI("sobre")), "Sobre o VisualizaCrimeRS", fixed = TRUE)
})
