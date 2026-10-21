# Testes da base gerada por atualiza_base_crime_mulher.R (base_crimemulher_2025.rds).
suppressPackageStartupMessages(library(tidyverse))
source("../../funcoes_atualiza_base.R", encoding = "UTF-8")

b <- readRDS("../../base_crimemulher_2025.rds")

test_that("estrutura e níveis de Crime com grafia exata", {
  expect_named(b, c("Mun", "CodIBGE", "Ano", "Crime", "Qtd", "Populacao"))
  expect_s3_class(b$Crime, "factor")
  expect_setequal(levels(b$Crime), c(indicadores, "Total"))
  expect_setequal(unique(b$Ano), 2012:2025)
  expect_false(anyNA(b))
  expect_true("Lesão Corporal" %in% levels(b$Crime))
})

test_that("Tentado 2012 ausente e municípios por ano", {
  expect_equal(nrow(filter(b, Crime == "Feminicídio Tentado", Ano == 2012)), 0)
  n <- b %>% filter(Crime == "Ameaça") %>% count(Ano)
  expect_true(all(n$n == 497))  # Pinto Bandeira já aparece em 2012 na SSP
  expect_equal(nrow(distinct(b, CodIBGE, Ano, Crime)), nrow(b))
})

test_that("Total é a soma dos indicadores disponíveis", {
  soma <- b %>% filter(Crime != "Total") %>% group_by(CodIBGE, Ano) %>% summarize(S = sum(Qtd), .groups = "drop")
  tot  <- b %>% filter(Crime == "Total") %>% select(CodIBGE, Ano, Qtd) %>% inner_join(soma, by = c("CodIBGE", "Ano"))
  expect_equal(nrow(tot), nrow(soma))
  expect_equal(tot$Qtd, tot$S)
})

test_that("Populacao é a população feminina da RIPSA", {
  pop <- read.csv("../../estimativas_24_09_2026.csv", encoding = "UTF-8") %>%
    filter(Classe == "Total", CodIBGE == 4314902, Ano %in% c(2012, 2025))
  poa <- b %>% filter(CodIBGE == 4314902, Crime == "Ameaça") %>% arrange(Ano)
  expect_equal(poa$Populacao[poa$Ano == 2025], pop$Mulheres[pop$Ano == 2025])
  expect_equal(poa$Populacao[poa$Ano == 2012], pop$Mulheres[pop$Ano == 2012])
  expect_false(isTRUE(all.equal(poa$Populacao[poa$Ano == 2025], pop$Total[pop$Ano == 2025])))
})

test_that("totais RS batem com a aba Geral", {
  rs <- b %>% group_by(Ano, Crime = as.character(Crime)) %>% summarize(RS = sum(Qtd), .groups = "drop")
  geral <- map_dfr(names(urls_ssp_mulher), le_geral_ssp_mulher)
  cmp <- full_join(rs, geral, by = c("Ano", "Crime")) %>%
    # 2018: "NÃO INFORMADO" (1 caso em Ameaça e 1 em Lesão Corporal) fica fora da base
    mutate(esperado = Qtd - (Ano == 2018) * case_when(Crime %in% c("Ameaça", "Lesão Corporal") ~ 1,
                                                      Crime == "Total" ~ 2,
                                                      TRUE ~ 0))
  expect_equal(nrow(cmp), 14 * 6 - 1)
  expect_false(anyNA(cmp$RS)); expect_false(anyNA(cmp$Qtd))
  expect_equal(cmp$RS, cmp$esperado, info = paste(capture.output(print(filter(cmp, RS != esperado))), collapse = "\n"))
})
