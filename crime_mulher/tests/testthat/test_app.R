# Testes do app (textos adaptados do app de crime e carregamento de ui/server).
ui  <- readLines("../../ui.R", encoding = "UTF-8")
srv <- readLines("../../server.R", encoding = "UTF-8")
glb <- readLines("../../global.R", encoding = "UTF-8")

test_that("sem resquícios do app de crime", {
  txt <- c(ui, srv, glb)
  for (s in c("VisualizaCrimeRS", "Homicídio Doloso", "\"Roubo", "Taxa por 100.000", "base_crimevis_2025.rds"))
    expect_false(any(grepl(s, txt, fixed = TRUE)), info = s)
})

test_that("textos e base novos", {
  expect_true(any(grepl("VisualizaViolênciaMulherRS", ui, fixed = TRUE)))
  expect_true(any(grepl("Taxa por 100 mil mulheres", ui, fixed = TRUE)))
  expect_true(any(grepl("Taxa por 100 mil mulheres", srv, fixed = TRUE)))
  expect_true(any(grepl("População feminina", c(ui, srv), fixed = TRUE)))
  expect_true(any(grepl("indicadores-da-violencia-contra-a-mulher", ui, fixed = TRUE)))
  expect_true(any(grepl("base_crimemulher_2025.rds", glb, fixed = TRUE)))
  expect_true(any(grepl("Feminicídio Tentado.*2013", ui)))  # aviso: Tentado começa em 2013
})

test_that("k-means protegido contra poucos pontos distintos", {
  expect_true(any(grepl("Há menos municípios distintos do que grupos", srv, fixed = TRUE)))
})

test_that("ui e server carregam", {
  old <- setwd("../.."); on.exit(setwd(old))
  suppressPackageStartupMessages(source("global.R", encoding = "UTF-8"))
  expect_s3_class(source("ui.R", encoding = "UTF-8")$value, "shiny.tag.list")
  expect_type(source("server.R", encoding = "UTF-8")$value, "closure")
})

test_that("tema rosa", {
  css <- readLines("../../estilocrimemulher.css")
  expect_true(any(grepl("#D63384", css, ignore.case = TRUE)))
  expect_true(any(grepl("estilocrimemulher.css", ui, fixed = TRUE)))
  expect_false(any(grepl("estilocrime.css", ui, fixed = TRUE)))
})

test_that("treemap avisa quando o crime não tem dados no ano", {
  ini <- grep("output$tree_map", srv, fixed = TRUE)
  bloco <- srv[ini:(ini + 10)]
  expect_true(any(grepl("Sem dados disponíveis para este crime neste ano.", bloco, fixed = TRUE)))
})

test_that("mensagens de validação ficam visíveis (só os erros inesperados são escondidos)", {
  expect_true(any(grepl(".shiny-output-error-validation { visibility: visible; }", ui, fixed = TRUE)))
})

# Casos-limite do servidor: mensagens de validate() em vez de erro
servidor <- local({
  old <- setwd("../.."); on.exit(setwd(old))
  suppressPackageStartupMessages(source("global.R", encoding = "UTF-8"))
  srv_fun <- source("server.R", encoding = "UTF-8")$value
  function(input, output, session) srv_fun(input, output)  # testServer exige o argumento session
})

msg_validacao <- function(expr) {
  tryCatch({ force(expr); NA_character_ }, shiny.silent.error = function(e) conditionMessage(e),
           error = function(e) paste("ERRO:", conditionMessage(e)))
}

test_that("Feminicídio Tentado em 2012 gera aviso de falta de dados", {
  shiny::testServer(servidor, {
    session$setInputs(ano_mapa = 2012, crime_mapa = "Feminicídio Tentado", tipo_dado_mapa = "ocorre_radio_mapa",
                      sens_mapa = 3, tipo_estrutura_espacial = "queen", n_vizinhos_moran = 2,
                      ano_tree = 2012, crime_tree = "Feminicídio Tentado", tipo_dado_tree = "ocorre_radio_tree",
                      ano_disp = 2012, crimex = "Feminicídio Consumado", crimey = "Feminicídio Tentado",
                      dispersao_mun_pop_control = c(0, 800000), tipo_dado_disp = "ocorre_radio_disp",
                      checkbox_kmeans = FALSE, n_grupos_kmeans = 3)
    expect_match(msg_validacao(output$mapa_final), "Sem dados disponíveis")
    expect_match(msg_validacao(output$tree_map), "Sem dados disponíveis")
    expect_match(msg_validacao(output$dispersao), "não possui dados")
  })
})

test_that("k-means com mais grupos do que pontos distintos gera aviso", {
  shiny::testServer(servidor, {
    session$setInputs(ano_disp = 2025, crimex = "Feminicídio Consumado", crimey = "Feminicídio Tentado",
                      dispersao_mun_pop_control = c(0, 5000), tipo_dado_disp = "ocorre_radio_disp",
                      checkbox_kmeans = TRUE, n_grupos_kmeans = 10)
    expect_match(msg_validacao(output$dispersao), "Há menos municípios distintos do que grupos")
    session$setInputs(n_grupos_kmeans = 3, dispersao_mun_pop_control = c(0, 800000))
    expect_true(is.na(msg_validacao(output$dispersao)))
  })
})

# Achados da revisão final: Markov e dispersão em casos-limite
test_that("Markov espaço-temporal avisa quando não há municípios sem vizinhos com crime (Ameaça)", {
  shiny::testServer(servidor, {
    session$setInputs(crime_markov = "Ameaça", anos_markov = c(2013, 2025))
    for (o in c("tabela_markov_estratificada", "odds_ratio_estratificado", "teste_chi_homog_spat"))
      expect_match(msg_validacao(output[[o]]), "Todos os municípios têm vizinhos com ocorrência", info = o)
  })
})

test_that("evolução anual espaço-temporal não quebra com estratos incompletos (Estupro)", {
  shiny::testServer(servidor, {
    session$setInputs(crime_markov = "Estupro", anos_markov = c(2012, 2025))
    expect_true(is.na(msg_validacao(output$evolucao_odds_espaco_temporal)))
    session$setInputs(crime_markov = "Ameaça")
    expect_true(is.na(msg_validacao(output$evolucao_odds_espaco_temporal)))
  })
})

test_that("Markov com ano inicial igual ao final gera aviso", {
  shiny::testServer(servidor, {
    session$setInputs(crime_markov = "Estupro", anos_markov = c(2020, 2020),
                      anos_markov_janela_1 = c(2015, 2015), anos_markov_janela_2 = c(2018, 2025))
    for (o in c("mapas_markov", "tabela_bruta", "estatistica_chi", "tabela_markov_simples", "odds_ratio_simples",
                "tabela_markov_estratificada", "odds_ratio_estratificado", "teste_chi_homog_spat"))
      expect_match(msg_validacao(output[[o]]), "Escolha dois anos diferentes", info = o)
    expect_match(msg_validacao(output$teste_chi_homog_temporal), "Escolha dois anos diferentes")
  })
})

test_that("dispersão com o mesmo crime nos dois eixos gera aviso", {
  shiny::testServer(servidor, {
    session$setInputs(ano_disp = 2025, crimex = "Ameaça", crimey = "Ameaça",
                      dispersao_mun_pop_control = c(0, 800000), tipo_dado_disp = "ocorre_radio_disp",
                      checkbox_kmeans = FALSE, n_grupos_kmeans = 3)
    expect_match(msg_validacao(output$dispersao), "Escolha dois crimes diferentes")
  })
})

test_that("número máximo de vizinhos do Moran é menor que o número de municípios", {
  expect_true(any(grepl('numericInput("n_vizinhos_moran", "Número de vizinhos:", 2, min = 1, max = 496)', ui, fixed = TRUE)))
})

test_that("arquivo baixado tem nome próprio deste app", {
  expect_true(any(grepl("base_violencia_mulher", srv, fixed = TRUE)))
  expect_false(any(grepl("'base_crime', '.csv'", srv, fixed = TRUE)))
})
