####################################################################
# Funções de leitura das planilhas de indicadores de violência contra
# a mulher da SSP-RS, usadas por atualiza_base_crime_mulher.R.
#
# Fonte: https://www.ssp.rs.gov.br/indicadores-da-violencia-contra-a-mulher
#
# Formatos:
# - 2018 a 2025: um arquivo por ano, uma aba por indicador, municípios
#   nas linhas e meses + "Total" nas colunas. Aba "Geral" com o total RS.
# - 2012 a 2017: um único arquivo; cada aba de indicador tem uma coluna
#   por ano. A aba "Geral" empilha um bloco por ano (só total RS).
#   Nesse arquivo o termo usado é "Femicídio" e não há Feminicídio
#   Tentado em 2012.
####################################################################

library(tidyverse)
library(readxl)
library(stringi)

# Pasta local para cache dos .xlsx baixados (não versionada no git)
cache_dir_mulher <- "C:/Users/renan/AppData/Local/Temp/ssp_mulher_samples"

urls_ssp_mulher <- c(
  "2012_2017" = "https://www.ssp.rs.gov.br/upload/arquivos/202307/12160259-site-violencia-contra-as-mulheres-2012-a-2017-atualizado-em-09-janeiro-2018-publicacao.xlsx",
  "2018" = "https://www.ssp.rs.gov.br/upload/arquivos/202307/12160426-site-violencia-contra-as-mulheres-2018-atualizado-em-07-janeiro-2019-publicacao.xlsx",
  "2019" = "https://www.ssp.rs.gov.br/upload/arquivos/202307/12160432-site-violencia-contra-as-mulheres-2019-atualizado-em-21-fevereiro-2020-publicacao.xlsx",
  "2020" = "https://www.ssp.rs.gov.br/upload/arquivos/202307/12163332-site-violencia-contra-as-mulheres-2020-atualizado-em-05-maio-2023-publicacao.xlsx",
  "2021" = "https://admin.ssp.rs.gov.br/upload/arquivos/202312/04105912-site-violencia-contra-as-mulheres-2021-atualizado-em-04-dez-2023.xlsx",
  "2022" = "https://www.ssp.rs.gov.br/upload/arquivos/202501/17161756-indicadores-de-violencia-contra-a-mulher-geral-e-por-municipio-2022-1.xlsx",
  "2023" = "https://admin.ssp.rs.gov.br/upload/arquivos/202601/15143232-site-violencia-contra-as-mulheres-2023-atualizado-em-05-janeiro-2026-publicacao.xlsx",
  "2024" = "https://admin.ssp.rs.gov.br/upload/arquivos/202601/15143237-site-violencia-contra-as-mulheres-2024-atualizado-em-05-janeiro-2026-publicacao.xlsx",
  "2025" = "https://www.ssp.rs.gov.br/upload/arquivos/202610/08160049-site-violencia-contra-as-mulheres-2025-atualizado-em-06-de-outubro-de-2026-publicacao.xlsx"
)

# Nomes canônicos dos indicadores usados pelo app
indicadores <- c("Feminicídio Consumado", "Feminicídio Tentado", "Ameaça", "Estupro", "Lesão Corporal")

# Nomes de município que não batem exatamente entre a planilha da SSP
# (CAIXA ALTA sem acento) e a base do app de crime, mesmo depois de
# normalizar (maiúsculas + remover acento).
name_exceptions <- c(
  "DR MAURICIO CARDOSO" = "DOUTOR MAURICIO CARDOSO",
  "ENTRE IJUIS"          = "ENTRE-IJUIS",
  "FAZENDA VILA NOVA"    = "FAZENDA VILANOVA",
  # Grafias próprias das planilhas de violência contra a mulher
  "BARAO DO COTEGIPE"    = "BARAO DE COTEGIPE",
  "NAO ME TOQUE"         = "NAO-ME-TOQUE",
  "XANGRILA"             = "XANGRI-LA"
)

normaliza <- function(x) stri_trans_general(toupper(trimws(x)), "Latin-ASCII")

# "Femicídio" (2012-2017) -> "Feminicídio"
padroniza_indicador <- function(x) sub("^Femic", "Feminic", trimws(x))

baixa_arquivo_ssp_mulher <- function(chave) {
  dir.create(cache_dir_mulher, showWarnings = FALSE, recursive = TRUE)
  destino <- file.path(cache_dir_mulher, paste0(chave, ".xlsx"))
  if (!file.exists(destino)) {
    download.file(urls_ssp_mulher[[chave]], destino, mode = "wb", quiet = TRUE)
  }
  destino
}

# Lê uma aba inteira como texto; os números são convertidos depois.
le_aba_bruta <- function(arq, aba) {
  suppressMessages(read_excel(arq, sheet = aba, col_names = FALSE, col_types = "text"))
}

# Linhas de município: nome preenchido e valor numérico na coluna escolhida.
# Descarta preâmbulo, linhas vazias, notas, a linha "Geral" e "NÃO INFORMADO".
linhas_municipio <- function(bruto, col_valor, linha_cabecalho) {
  tibble(MunSSP = trimws(bruto[[1]]), Qtd = suppressWarnings(as.numeric(bruto[[col_valor]]))) %>%
    slice(-seq_len(linha_cabecalho)) %>%
    filter(!is.na(MunSSP), !is.na(Qtd), !grepl("INFORMADO|^TOTAL|^GERAL$", normaliza(MunSSP)))
}

le_ano_ssp_mulher <- function(arq, ano) {
  map_dfr(indicadores, function(indicador) {
    bruto <- le_aba_bruta(arq, indicador)
    # O número de linhas de preâmbulo varia; localizamos o cabeçalho.
    linha_cabecalho <- which(trimws(bruto[[1]]) == "Município")[1]
    col_total <- which(trimws(unlist(bruto[linha_cabecalho, ])) == "Total")[1]

    linhas_municipio(bruto, col_total, linha_cabecalho) %>%
      mutate(Ano = as.integer(ano), Crime = indicador) %>%
      select(MunSSP, Ano, Crime, Qtd)
  })
}

le_2012_2017_ssp_mulher <- function(arq) {
  map_dfr(indicadores, function(indicador) {
    bruto <- le_aba_bruta(arq, sub("^Feminic", "Femic", indicador))
    linha_cabecalho <- which(trimws(bruto[[1]]) == "Município")[1]
    cabecalho <- trimws(unlist(bruto[linha_cabecalho, ]))
    # Só as colunas de ano; "Total" (soma dos seis anos), "% vítimas/total"
    # e as colunas de população ficam de fora.
    cols_ano <- which(cabecalho %in% as.character(2012:2017))

    map_dfr(cols_ano, function(col) {
      linhas_municipio(bruto, col, linha_cabecalho) %>%
        mutate(Ano = as.integer(cabecalho[col]), Crime = indicador)
    }) %>%
      select(MunSSP, Ano, Crime, Qtd)
  })
}

# Totais RS da aba "Geral" (a linha "Geral" vira Crime == "Total").
le_geral_ssp_mulher <- function(chave) {
  bruto <- le_aba_bruta(baixa_arquivo_ssp_mulher(chave), "Geral")
  rotulo <- trimws(bruto[[1]])

  # Ano de cada linha: o do último título "... no Ano de AAAA" acima dela
  # (o arquivo 2012-2017 empilha um bloco por ano).
  ano_titulo <- as.integer(str_match(rotulo, "no Ano de (\\d{4})")[, 2])
  ano_linha <- repete_ultimo(ano_titulo)

  # Coluna "Total" do cabeçalho de cada bloco
  linhas_cab <- which(rotulo == "Município")
  col_total <- map_int(linhas_cab, ~ which(trimws(unlist(bruto[.x, ])) == "Total")[1])
  col_linha <- repete_ultimo(replace(rep(NA_integer_, length(rotulo)), linhas_cab, col_total))

  valor <- map2_dbl(seq_along(rotulo), col_linha,
                    ~ if (is.na(.y)) NA_real_ else suppressWarnings(as.numeric(bruto[[.y]][.x])))

  tibble(Ano = ano_linha, Crime = padroniza_indicador(rotulo), Qtd = valor) %>%
    mutate(Crime = ifelse(Crime == "Geral", "Total", Crime)) %>%
    filter(Crime %in% c(indicadores, "Total"), !is.na(Qtd))
}

# Repete o último valor não-NA para baixo
repete_ultimo <- function(x) {
  idx <- cummax(seq_along(x) * !is.na(x))
  x[replace(idx, idx == 0, NA)]
}

associa_codibge <- function(df, crosswalk_key) {
  df$key <- normaliza(df$MunSSP)
  df$key <- ifelse(df$key %in% names(name_exceptions), name_exceptions[df$key], df$key)

  df <- df %>% left_join(crosswalk_key, by = "key")
  if (any(is.na(df$CodIBGE))) {
    stop("Município(s) sem correspondência: ",
         paste(unique(df$MunSSP[is.na(df$CodIBGE)]), collapse = ", "))
  }

  df %>% select(-MunSSP, -key) %>% relocate(CodIBGE)
}
