####################################################################
# Gera a base de violência contra a mulher (base_crimemulher_2025.rds),
# usada pelo global.R, com os indicadores por município da SSP-RS de
# 2012 a 2025 e a população feminina de cada município/ano.
#
# Fonte dos dados: https://www.ssp.rs.gov.br/indicadores-da-violencia-contra-a-mulher
# Fonte da população: estimativas_24_09_2026.csv (Rede Interagencial de
# Informações para a Saúde - RIPSA), coluna "Mulheres"
# https://www.gov.br/saude/pt-br/composicao/seidigi/demas/ripsa
#
# Observações:
# - A SSP não publica Feminicídio Tentado para 2012; essas linhas ficam
#   fora da base (não são NA nem 0).
# - Em 2018, Ameaça e Lesão Corporal têm uma linha "NÃO INFORMADO" (1 caso
#   cada) que entra no total "Geral" da SSP mas não tem município. Ela é
#   descartada, então o total RS de 2018 desses indicadores fica 1 caso
#   abaixo do publicado pela SSP (e o "Total", 2 casos abaixo).
# - "Total" é a soma dos indicadores disponíveis no município/ano.
# - Pinto Bandeira (instalado em 2013) entra em 2012, ao contrário da base
#   do app de crime: a SSP publica o município nesse ano (1 caso de Lesão
#   Corporal) e a RIPSA tem sua população, então os totais RS batem com a SSP.
####################################################################

source("funcoes_atualiza_base.R", encoding = "UTF-8")


# 1. Correspondência Mun <-> CodIBGE (mesma grafia da base do app de crime)
crosswalk <- readRDS("../crime/base_crimevis_2025.rds") %>%
  distinct(Mun, CodIBGE) %>%
  mutate(Mun = as.character(Mun))
crosswalk_key <- crosswalk %>% mutate(key = normaliza(Mun)) %>% select(CodIBGE, key)


# 2. Indicadores da SSP-RS ----------------------------------------------------
base_ssp <- bind_rows(
  le_2012_2017_ssp_mulher(baixa_arquivo_ssp_mulher("2012_2017")),
  map_dfr(2018:2025, ~ le_ano_ssp_mulher(baixa_arquivo_ssp_mulher(as.character(.x)), .x))
) %>%
  associa_codibge(crosswalk_key)

base_total <- base_ssp %>%
  group_by(CodIBGE, Ano) %>%
  summarize(Qtd = sum(Qtd), .groups = "drop") %>%
  mutate(Crime = "Total")

base_ssp <- bind_rows(base_ssp, base_total)


# 3. População feminina (RIPSA) ----------------------------------------------
pop <- read.csv("estimativas_24_09_2026.csv", encoding = "UTF-8", stringsAsFactors = FALSE) %>%
  filter(Classe == "Total") %>%
  transmute(CodIBGE, Ano, Populacao = as.integer(Mulheres))


# 4. Combina tudo ------------------------------------------------------------
base_crime_mulher <- base_ssp %>%
  left_join(crosswalk, by = "CodIBGE") %>%
  left_join(pop, by = c("CodIBGE", "Ano")) %>%
  mutate(Ano = as.numeric(Ano)) %>%
  select(Mun, CodIBGE, Ano, Crime, Qtd, Populacao)

if (any(is.na(base_crime_mulher$Populacao))) {
  faltantes <- base_crime_mulher %>% filter(is.na(Populacao)) %>% distinct(Mun, Ano)
  stop("Sem população para ", nrow(faltantes), " combinação(ões) de município/ano. Ex.: ",
       paste(head(paste(faltantes$Mun, faltantes$Ano), 5), collapse = "; "))
}

base_crime_mulher <- base_crime_mulher %>%
  mutate(Crime = factor(Crime, levels = c(indicadores, "Total"))) %>%
  arrange(Ano, Crime, Mun)

cat("Linhas:", nrow(base_crime_mulher), "\n")
cat("Anos:", paste(range(base_crime_mulher$Ano), collapse = "-"), "\n")
cat("Tipos de crime:", nlevels(base_crime_mulher$Crime), "\n")
cat("Municípios:", n_distinct(base_crime_mulher$CodIBGE), "\n")

# 5. Conferências antes de salvar (param o script se algo não bater) --------
confere_municipios(base_crime_mulher)
confere_totais_geral(base_crime_mulher)
cat("Conferências OK: 497 municípios por indicador/ano e totais RS iguais aos da SSP\n")

saveRDS(base_crime_mulher, "base_crimemulher_2025.rds")
cat("Salvo em base_crimemulher_2025.rds\n")
