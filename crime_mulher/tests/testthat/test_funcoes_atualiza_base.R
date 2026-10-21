# Testes das funções de leitura das planilhas de violência contra a mulher da SSP-RS.
# Rodar a partir de crime_mulher/:
#   Rscript -e 'testthat::test_dir("tests/testthat")'
suppressPackageStartupMessages(library(tidyverse))
source("../../funcoes_atualiza_base.R", encoding = "UTF-8")

crosswalk_key <- readRDS("../../../crime/base_crimevis_2025.rds") %>%
  distinct(Mun, CodIBGE) %>%
  mutate(key = normaliza(Mun)) %>%
  select(CodIBGE, key)

test_that("le_ano_ssp_mulher 2025 traz 497 municípios e os totais da SSP", {
  d <- le_ano_ssp_mulher(baixa_arquivo_ssp_mulher("2025"), 2025L)
  expect_setequal(unique(d$Crime), indicadores)
  expect_true(all(table(d$Crime) == 497))
  expect_equal(d$Qtd[d$MunSSP == "ALVORADA" & d$Crime == "Ameaça"], 769)
  expect_equal(d$Qtd[d$MunSSP == "ALEGRETE" & d$Crime == "Feminicídio Consumado"], 1)
  expect_equal(sum(d$Qtd[d$Crime == "Feminicídio Tentado"]), 263)
})

test_that("le_ano_ssp_mulher 2018 ignora a linha vazia e NÃO INFORMADO", {
  d <- le_ano_ssp_mulher(baixa_arquivo_ssp_mulher("2018"), 2018L)
  expect_false(any(grepl("INFORMADO", d$MunSSP)))
  expect_true(all(table(d$Crime) == 497))
})

test_that("le_2012_2017_ssp_mulher padroniza Feminicídio e omite Tentado 2012", {
  d <- le_2012_2017_ssp_mulher(baixa_arquivo_ssp_mulher("2012_2017"))
  expect_setequal(unique(d$Ano), 2012:2017)
  expect_setequal(unique(d$Crime), indicadores)
  expect_false(any(grepl("Femicídio", d$Crime)))
  expect_equal(nrow(filter(d, Crime == "Feminicídio Tentado", Ano == 2012)), 0)
})

test_that("le_geral_ssp_mulher traz os totais RS", {
  g25 <- le_geral_ssp_mulher("2025")
  expect_equal(g25$Qtd[g25$Crime == "Total"], 53411)
  expect_equal(g25$Qtd[g25$Crime == "Feminicídio Consumado"], 80)
  g12 <- le_geral_ssp_mulher("2012_2017")
  expect_equal(g12$Qtd[g12$Ano == 2012 & g12$Crime == "Total"], 73647)
  expect_equal(nrow(filter(g12, Ano == 2012, Crime == "Feminicídio Tentado")), 0)
})

test_that("associa_codibge para quando falta município", {
  df <- tibble(MunSSP = c("PORTO ALEGRE", "CIDADE INEXISTENTE"), Qtd = 1)
  expect_error(associa_codibge(df, crosswalk_key), "CIDADE INEXISTENTE")
  ok <- associa_codibge(tibble(MunSSP = "DR MAURICIO CARDOSO", Qtd = 1), crosswalk_key)
  expect_false(anyNA(ok$CodIBGE))
})

test_that("pasta de cache não depende do usuário e aceita SSP_MULHER_CACHE", {
  e <- new.env()
  sys.source("../../funcoes_atualiza_base.R", envir = e, keep.source = FALSE, toplevel.env = e)
  expect_equal(e$cache_dir_mulher, file.path(dirname(tempdir()), "ssp_mulher_samples"))
  Sys.setenv(SSP_MULHER_CACHE = "D:/outra/pasta"); on.exit(Sys.unsetenv("SSP_MULHER_CACHE"))
  e2 <- new.env()
  sys.source("../../funcoes_atualiza_base.R", envir = e2, keep.source = FALSE, toplevel.env = e2)
  expect_equal(e2$cache_dir_mulher, "D:/outra/pasta")
})

base_ok <- readRDS("../../base_crimemulher_2025.rds")

test_that("confere_municipios aceita a base e para quando falta município ou há duplicata", {
  expect_silent(confere_municipios(base_ok))
  sem_um <- base_ok[!(base_ok$Mun == "Porto Alegre" & base_ok$Ano == 2020 & base_ok$Crime == "Estupro"), ]
  expect_error(confere_municipios(sem_um), "2020.*Estupro")
  duplicada <- rbind(base_ok, base_ok[1, ])
  expect_error(confere_municipios(duplicada), "duplicad")
})

test_that("confere_totais_geral aceita a base e para quando o total RS diverge da SSP", {
  expect_silent(confere_totais_geral(base_ok))
  alterada <- base_ok
  i <- which(alterada$Ano == 2021 & alterada$Crime == "Ameaça")[1]
  alterada$Qtd[i] <- alterada$Qtd[i] + 5
  expect_error(confere_totais_geral(alterada), "2021.*Ameaça")
})
