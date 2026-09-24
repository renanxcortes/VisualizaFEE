####################################################################
# Atualiza a base de dados de criminalidade (base_crime) com os
# indicadores por município da SSP-RS de 2017 a 2025, e recalcula
# a população de todos os anos (2002-2025) com base nas estimativas
# da RIPSA. Gera base_crimevis_2025.rds, usada pelo global.R.
#
# Fonte dos dados criminais: https://www.ssp.rs.gov.br/indicadores-criminais
# Fonte da população: estimativas_24_09_2026.csv (Rede Interagencial de
# Informações para a Saúde - RIPSA)
# https://www.gov.br/saude/pt-br/composicao/seidigi/demas/ripsa
####################################################################

library(tidyverse)
library(readxl)
library(stringi)

# Pasta local para cache dos .xlsx baixados (não versionada no git)
cache_dir <- "C:/Users/renan/AppData/Local/Temp/ssp_samples"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

urls_ssp <- c(
  "2017" = "https://ssp.rs.gov.br/upload/arquivos/202101/13165939-site-geral-e-municipios-2017-publicacao-atualizado-em-05-jan-2021.xlsx",
  "2018" = "https://ssp.rs.gov.br/upload/arquivos/202402/14190057-indicadores-criminais-geral-e-por-municipio-2018.xlsx",
  "2019" = "https://ssp.rs.gov.br/upload/arquivos/202401/11110144-indicadores-criminais-geral-e-por-municipio-2019.xlsx",
  "2020" = "https://ssp.rs.gov.br/upload/arquivos/202312/04103652-site-geral-e-municipios-ano-2020-atualizado-em-04-dez-2023-dados-cvli-atualizado-publicacao.xlsx",
  "2021" = "https://ssp.rs.gov.br/upload/arquivos/202312/04103647-site-geral-e-municipios-ano-2021-atualizado-em-04-dez-2023-dados-cvli-atualizado-publicacao.xlsx",
  "2022" = "https://ssp.rs.gov.br/upload/arquivos/202401/11110150-indicadores-criminais-geral-e-por-municipio-2022.xlsx",
  "2023" = "https://admin.ssp.rs.gov.br/upload/arquivos/202503/28094229-site-geral-e-munici-pios-ano-2023-atualizado-em-05-mar-2025-dados-cvli-atualizado-publicacao.xlsx",
  "2024" = "https://admin.ssp.rs.gov.br/upload/arquivos/202601/15143248-site-geral-e-municipios-ano-2024-atualizado-em-05-jan-2026-dados-cvli-atualizado-publicacao.xlsx",
  "2025" = "https://www.ssp.rs.gov.br/upload/arquivos/202609/10160731-site-geral-e-municipios-ano-2025-atualizado-em-04-set-2026-dados-cvli-atualizado-publicacao.xlsx"
)

# Nomes de coluna da SSP-RS -> nomes de crime canônicos usados pelo app.
# As 10 primeiras já existiam na base 2002-2016 (só o nome mudou).
# As 5 últimas são indicadores novos, sem dado antes de 2017.
crime_map <- c(
  "Delitos Relacionados à Armas e Munições" = "Delitos Relacionados à Armas e Munições",
  "Estelionato"                             = "Estelionato",
  "Furtos"                                  = "Furto",
  "Furto de Veículo"                        = "Furto de Veículos",
  "Homicídio  Doloso"                       = "Homicídio Doloso",
  "Latrocínio"                              = "Latrocínio",
  "Entorpecentes - Posse"                   = "Posse de Entorpecentes",
  "Roubos"                                  = "Roubo",
  "Roubo de Veículo"                        = "Roubo de Veículos",
  "Entorpecentes - Tráfico"                 = "Tráfico de Entorpecentes",
  "Total de vítimas de Homicidio Doloso"    = "Total de Vítimas de Homicídio Doloso",
  "Abigeato*"                               = "Abigeato",
  "Vítimas de Latrocínio"                   = "Vítimas de Latrocínio",
  "Vítimas de Lesão Corp. Seg. Morte"       = "Vítimas de Lesão Corporal Seguida de Morte",
  "Vítimas de CVLI"                         = "Total de Vítimas de CVLI",       # nome usado em 2018-2019
  "Total de Vítimas de CVLI*"               = "Total de Vítimas de CVLI"       # nome usado a partir de 2020
)

# Nomes de município que não batem exatamente entre a planilha da SSP
# (CAIXA ALTA sem acento) e a base já existente, mesmo depois de
# normalizar (maiúsculas + remover acento).
name_exceptions <- c(
  "DR MAURICIO CARDOSO" = "DOUTOR MAURICIO CARDOSO",
  "ENTRE IJUIS"          = "ENTRE-IJUIS",
  "FAZENDA VILA NOVA"    = "FAZENDA VILANOVA"
)

normaliza <- function(x) stri_trans_general(toupper(trimws(x)), "Latin-ASCII")

baixa_arquivo_ssp <- function(ano) {
  destino <- file.path(cache_dir, paste0(ano, ".xlsx"))
  if (!file.exists(destino)) {
    download.file(urls_ssp[[as.character(ano)]], destino, mode = "wb", quiet = TRUE)
  }
  destino
}

le_ano_ssp <- function(ano, crosswalk_key) {
  arq <- baixa_arquivo_ssp(ano)

  # A planilha é uma exportação de tabela dinâmica do Excel; o número de
  # linhas de preâmbulo varia de ano pra ano, então localizamos a linha
  # de cabeçalho procurando por "Municípios" em vez de fixar o skip.
  bruto <- read_excel(arq, sheet = as.character(ano), col_names = FALSE, n_max = 20)
  linha_cabecalho <- which(trimws(bruto[[1]]) == "Municípios")[1]

  df <- read_excel(arq, sheet = as.character(ano), skip = linha_cabecalho - 1)
  names(df)[1] <- "MunSSP"

  linha_total <- which(toupper(trimws(df$MunSSP)) == "TOTAL RS")
  df <- df[seq_len(linha_total - 1), ]

  df$key <- normaliza(df$MunSSP)
  df$key <- ifelse(df$key %in% names(name_exceptions), name_exceptions[df$key], df$key)

  df <- df %>% left_join(crosswalk_key, by = "key")
  if (any(is.na(df$CodIBGE))) {
    stop("Município(s) sem correspondência em ", ano, ": ",
         paste(df$MunSSP[is.na(df$CodIBGE)], collapse = ", "))
  }

  df <- df %>% select(-MunSSP, -key)

  nomes <- names(df)
  reconhecidos <- nomes %in% names(crime_map)
  nomes[reconhecidos] <- crime_map[nomes[reconhecidos]]
  names(df) <- nomes

  df %>%
    pivot_longer(-CodIBGE, names_to = "Crime", values_to = "Qtd") %>%
    mutate(Ano = ano) %>%
    select(CodIBGE, Ano, Crime, Qtd)
}


# 1. Base histórica (2002-2016) -------------------------------------------
base_old <- readRDS("base_crimevis_2016_pop_ok.rds")
base_old$Mun   <- stri_conv(as.character(base_old$Mun), "latin1", "UTF-8")
base_old$Crime <- stri_conv(as.character(base_old$Crime), "latin1", "UTF-8")
base_old$Crime <- as.character(base_old$Crime)

crosswalk <- base_old %>% distinct(Mun, CodIBGE)
crosswalk_key <- crosswalk %>% mutate(key = normaliza(Mun)) %>% select(CodIBGE, key)


# 2. Anos novos (2017-2024) da SSP-RS --------------------------------------
base_novo <- map_dfr(2017:2025, le_ano_ssp, crosswalk_key = crosswalk_key)

# Junta os nomes "bonitos" (com acento, mesma grafia da base histórica)
base_novo <- base_novo %>%
  left_join(crosswalk, by = "CodIBGE") %>%
  select(Mun, CodIBGE, Ano, Crime, Qtd)


# 3. População (2002-2024), única fonte para toda a série ------------------
pop <- read.csv("estimativas_24_09_2026.csv", encoding = "UTF-8", stringsAsFactors = FALSE) %>%
  filter(Classe == "Total") %>%
  transmute(CodIBGE, Ano, Populacao = as.integer(Total))


# 4. Combina tudo ------------------------------------------------------------
base_crime_atualizada <- bind_rows(
  base_old %>% select(Mun, CodIBGE, Ano, Crime, Qtd),
  base_novo
) %>%
  left_join(pop, by = c("CodIBGE", "Ano"))

if (any(is.na(base_crime_atualizada$Populacao))) {
  faltantes <- base_crime_atualizada %>% filter(is.na(Populacao)) %>% distinct(Mun, Ano)
  stop("Sem população para ", nrow(faltantes), " combinação(ões) de município/ano. Ex.: ",
       paste(head(paste(faltantes$Mun, faltantes$Ano), 5), collapse = "; "))
}

base_crime_atualizada <- base_crime_atualizada %>%
  mutate(Crime = as.factor(Crime)) %>%
  arrange(Ano, Crime, Mun)

cat("Linhas:", nrow(base_crime_atualizada), "\n")
cat("Anos:", paste(range(base_crime_atualizada$Ano), collapse = "-"), "\n")
cat("Tipos de crime:", nlevels(base_crime_atualizada$Crime), "\n")
cat("Municípios:", n_distinct(base_crime_atualizada$CodIBGE), "\n")

saveRDS(base_crime_atualizada, "base_crimevis_2025.rds")
cat("Salvo em base_crimevis_2025.rds\n")
