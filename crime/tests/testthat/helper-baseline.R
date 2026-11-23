# Helpers dos testes: carregam o app e comparam outputs dos módulos com o
# "baseline" gravado a partir do server.R original (antes da modularização).
suppressPackageStartupMessages(library(testthat))

# testthat roda os helpers com o diretório de trabalho em tests/testthat
raiz_app <- normalizePath("../..")

local({
  old <- setwd(raiz_app); on.exit(setwd(old))
  # loadSupport sobe global.R e os arquivos da pasta R/ (módulos) no ambiente global
  suppressPackageStartupMessages(shiny::loadSupport(raiz_app, renv = globalenv(), globalrenv = globalenv()))
})

ano_max <- max(base_crime$Ano)
ano_min <- min(base_crime$Ano)
ano_meio <- floor(mean(c(ano_min, ano_max)))

# Cada cenário: módulo, inputs (com os defaults da UI) e outputs a comparar
cenarios <- local({
  mapa <- list(ano_mapa = ano_max, crime_mapa = "Homicídio Doloso", sens_mapa = 1.9,
               checkbox_moran = TRUE, n_vizinhos_moran = 2)
  markov <- list(crime_markov = "Roubo de Veículos", anos_markov = c(ano_min, ano_max),
                 checkbox_inclui_teste_temporal = TRUE,
                 anos_markov_janela_1 = c(ano_min, ano_meio),
                 anos_markov_janela_2 = c(ano_meio, ano_max),
                 checkbox_inclui_evolucao_instantaneo_markoviana = TRUE)
  cc <- list(cidade_compara_crime = "Porto Alegre", crimes_compara_crimes = c("Homicídio Doloso", "Roubo"))
  cm <- list(cidades_compara = "Porto Alegre", crime_compara = "Roubo",
             tipo_dado_compara_municipio = "taxa_radio_compara_municipio",
             checkbox_inclui_rs_rmpa_ts = TRUE)
  tree <- list(ano_tree = ano_max, crime_tree = "Homicídio Doloso")
  out_mapa <- c("mapa_final", "gauge_moran", "mapinha_grafo")
  out_cc <- c("ts_compara_crime_cidades", "ts_compara_crime_rs")
  list(
    mapa_ocorre = list(modulo = "mapa", outputs = out_mapa, inputs = c(mapa, list(
      tipo_dado_mapa = "ocorre_radio_mapa", tipo_estrutura_espacial = "Municípios que fazem fronteira"))),
    mapa_taxa = list(modulo = "mapa", outputs = out_mapa, inputs = c(mapa, list(
      tipo_dado_mapa = "taxa_radio_mapa", tipo_estrutura_espacial = "Municipios mais próximos"))),
    mapa_pop = list(modulo = "mapa", outputs = "mapa_final", inputs = c(mapa, list(
      tipo_dado_mapa = "pop_radio_mapa", tipo_estrutura_espacial = "Municípios que fazem fronteira"))),
    markov_temporal = list(modulo = "markov", inputs = c(markov, list(tipo_analise_markov = "radio_matriz_markov")),
      outputs = c("mapas_markov", "tabela_markov_simples", "odds_ratio_simples", "tabela_bruta",
                  "estatistica_chi", "teste_chi_homog_temporal", "anos_janelas", "evolucao_odds_temporal")),
    markov_espacial = list(modulo = "markov", inputs = c(markov, list(tipo_analise_markov = "radio_matriz_markov_estratificada")),
      outputs = c("tabela_markov_estratificada", "odds_ratio_estratificado", "teste_chi_homog_spat",
                  "evolucao_odds_espaco_temporal")),
    compara_crimes_ocorre = list(modulo = "compara_crimes", outputs = out_cc,
      inputs = c(cc, list(tipo_dado_compara_crime = "ocorre_radio_compara_crime"))),
    compara_crimes_taxa = list(modulo = "compara_crimes", outputs = out_cc,
      inputs = c(cc, list(tipo_dado_compara_crime = "taxa_radio_compara_crime"))),
    compara_mun_rs = list(modulo = "compara_municipios", outputs = "ts_compara_cidades",
      inputs = c(cm, list(radio_estado_rmpa = "radio_rs"))),
    compara_mun_rmpa = list(modulo = "compara_municipios", outputs = "ts_compara_cidades",
      inputs = c(cm, list(radio_estado_rmpa = "radio_rmpa"))),
    dispersao = list(modulo = "dispersao", outputs = "dispersao", inputs = list(
      ano_disp = ano_max, dispersao_mun_pop_control = c(0, 1500000), crimex = "Homicídio Doloso",
      crimey = "Roubo", tipo_dado_disp = "ocorre_radio_disp", checkbox_kmeans = FALSE, n_grupos_kmeans = 3)),
    treemap_ocorre = list(modulo = "treemap", outputs = "tree_map",
      inputs = c(tree, list(tipo_dado_tree = "ocorre_radio_tree"))),
    treemap_pop = list(modulo = "treemap", outputs = "tree_map",
      inputs = c(tree, list(tipo_dado_tree = "pop_radio_tree"))),
    dados = list(modulo = "dados", outputs = "tabela", inputs = list())
  )
})

# Remove campos com ids aleatórios dos htmlwidgets para a comparação ser estável
campos_aleatorios <- c("attrs", "cur_data", "visdat", "base_url", "elementId")
limpa <- function(x) {
  if (!is.list(x)) return(x)
  if (!is.null(names(x))) x <- x[!names(x) %in% campos_aleatorios]
  lapply(x, limpa)
}
normaliza <- function(valor) {
  if (is.list(valor) && !is.null(valor$src)) return("plot")
  if (is.character(valor) && length(valor) == 1 && startsWith(valor, "{")) {
    js <- jsonlite::fromJSON(valor, simplifyVector = FALSE)
    if (!is.null(js$x)) return(limpa(js$x))
  }
  valor
}

captura <- function(server_fun, inputs, outputs) {
  res <- NULL
  shiny::testServer(server_fun, {
    if (length(inputs)) do.call(session$setInputs, inputs)
    res <<- lapply(setNames(outputs, outputs), function(o)
      normaliza(tryCatch(output[[o]], error = function(e) paste("ERRO:", conditionMessage(e)))))
  })
  res
}

arq_baseline <- file.path(raiz_app, "tests", "testthat", "fixtures", "baseline.rds")
baseline <- if (file.exists(arq_baseline)) readRDS(arq_baseline) else NULL

# Roda todos os cenários de um módulo e compara com o baseline.
# server_nome é uma string para que a falta do módulo vire falha de teste, não erro de carga.
testa_baseline <- function(modulo, server_nome) {
  for (nome in names(Filter(function(c) c$modulo == modulo, cenarios))) {
    test_that(paste("baseline:", nome), {
      c <- cenarios[[nome]]
      set.seed(1)
      expect_identical(captura(get(server_nome), c$inputs, c$outputs), baseline[[nome]])
    })
  }
}
