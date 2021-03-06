###########
# Pacotes #
###########
library(shinythemes)
library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(flexdashboard)
library(zoo)
library(shinyBS)
library(formattable)
library(DT)
library(collapsibleTree)
library(magrittr)
###########
###########
###########

# Lista de Ícones aqui: http://fontawesome.io/icons/
# Excelente link de consulta para os gráficos do plotly: https://plot.ly/r/reference/#layout-shapes
# Excelente link para o modeBar do plotly https://stackoverflow.com/questions/37437808/how-to-custom-or-display-modebar-in-plotly


options(shiny.sanitize.errors = FALSE)

# Lendo quatro bases: dados brutos, com ajuste sazonal, pib bruto e pib com ajuste sazonal
base <- readRDS("base_ciclo_V21.rds")
base_SA <- readRDS("base_ciclo_SA_V21.rds")

base_pib <- readRDS("base_pib_ciclo_V4_SEM_MES_MEDIANO.rds")
base_pib_sa <- readRDS("base_pib_ciclo_sa_V4_SEM_MES_MEDIANO.rds")

# Lendo o tradutor
tradutor <- readRDS("tradutor_ciclo_V23.rds")

# Conversão de string para Shiny server
#tradutor %<>%
#  mutate_at(.vars = vars(NomeVar, 
#                         NomeVarTabela, 
#                         DescVar_Tooltips, 
#                         DescVar_Original, 
#                         Categoria,
#                         UnidadeMedida,
#                         Explicacao),
#            .funs = function(x) {stri_conv(as.character(x), "latin1", "UTF-8")})


# Funções de Variação: s: parâmetro de variação sazonal e k é o lag da diferença da aceleração
s <- 12
k <- 1

# Lista de séries que são contra cíclicas:
series_contra_ci <- c("s_1005", "s_1006", "s_1016")

# Lista de séries que devem ser avaliadas em pontos percentuais, pois são medidas em pontos percentuais:
series_pp <- c("s_1005", "s_1016")

# Número de casas decimais de tooltips:
casas_decimais <- 1

calcula_crescimento                 <- function(x, s) { (x / lag(x, s) - 1) * 100 }
calcula_crescimento_pp              <- function(x, s) { x - lag(x, s) }
calcula_crescimento_acum_S_meses    <- function(x, s) { aux <- rollapply(x, width = s, FUN = sum); return(c(rep(NA, s - 1),(aux / lag(aux, s) - 1) * 100)) } # Adiciona NA's para retornar um vetor de mesmo tamanho
calcula_crescimento_acum_S_meses_pp <- function(x, s) { aux <- rollapply(x, width = s, FUN = sum); return(c(rep(NA, s - 1),(aux - lag(aux, s)))) }
calcula_crescimento_acum_S_meses_JM    <- function(x, s) { aux <- rollapply(x, width = s, FUN = sum); return(c(rep(NA, s - 1),(aux / lag(aux, s = 1) - 1) * 100)) } # Adiciona NA's para retornar um vetor de mesmo tamanho
calcula_crescimento_acum_S_meses_pp_JM <- function(x, s) { aux <- rollapply(x, width = s, FUN = sum); return(c(rep(NA, s - 1),(aux - lag(aux, s = 1)))) }

calcula_aceleracao                  <- function(x, s, k) { aux <- diff(calcula_crescimento(x, s), k); return(c(NA, aux)) } # Adiciona um NA pois estava resultando em um vetor de tamanho diferente no mutate_each a seguir
calcula_aceleracao_pp               <- function(x, s, k) { aux <- diff(calcula_crescimento_pp(x, s), k); return(c(NA, aux)) }
calcula_aceleracao_acum_S_meses     <- function(x, s, k) { aux <- diff(calcula_crescimento_acum_S_meses(x, s), k); return(c(NA, aux)) }
calcula_aceleracao_acum_S_meses_pp  <- function(x, s, k) { aux <- diff(calcula_crescimento_acum_S_meses_pp(x, s), k); return(c(NA, aux)) }

retorna_mes <- Vectorize(function(num_mes) {switch(as.character(num_mes),
                                                   "1" = "Janeiro",
                                                   "2" = "Fevereiro",
                                                   "3" = "Março",
                                                   "4" = "Abril",
                                                   "5" = "Maio",
                                                   "6" = "Junho",
                                                   "7" = "Julho",
                                                   "8" = "Agosto",
                                                   "9" = "Setembro",
                                                   "10" = "Outubro",
                                                   "11" = "Novembro",
                                                   "12" = "Dezembro")})

# Transforma uma string "yyyy-mm-dd" para "NomeMês-Ano"
transforma_dia_para_mes_ano <- function(x) {paste0(retorna_mes(as.numeric(str_sub(x, 6, 7))), "-", str_sub(x,1,4)) } 


# Função que pega a base bruta, adiciona as variações e os metadados da série (fonte, descrição, etc.)
transforma_base <- function(x) {
  x %>% 
    mutate_each(funs(Cresci_Tx_Mensal_12M = calcula_crescimento(., s = 12), 
                     Acelera_Tx_Mensal_12M = calcula_aceleracao(., s = 12, k), 
                     
                     Cresci_pp_Mensal_12M = calcula_crescimento_pp(., s = 12), 
                     Acelera_pp_Mensal_12M = calcula_aceleracao_pp(., s = 12, k),
                     
                     Cresci_Tx_Acum_12M = calcula_crescimento_acum_S_meses(., s = 12), 
                     Acelera_Tx_Acum_12M = calcula_aceleracao_acum_S_meses(., s = 12, k), 
                     
                     Cresci_pp_Acum_12M = calcula_crescimento_acum_S_meses_pp(., s = 12), 
                     Acelera_pp_Acum_12M = calcula_aceleracao_acum_S_meses_pp(., s = 12, k), 
                     
                     Cresci_Tx_Mensal_1M = calcula_crescimento(., s = 1), 
                     Acelera_Tx_Mensal_1M = calcula_aceleracao(., s = 1, k), 
                     
                     Cresci_pp_Mensal_1M = calcula_crescimento_pp(., s = 1), 
                     Acelera_pp_Mensal_1M = calcula_aceleracao_pp(., s = 1, k),
                     
                     Cresci_Tx_Acum_3M_JM = calcula_crescimento_acum_S_meses_JM(., s = 3), 
                     Acelera_Tx_Acum_3M_JM = calcula_aceleracao_acum_S_meses(., s = 3, k), 
                     
                     Cresci_pp_Acum_3M_JM = calcula_crescimento_acum_S_meses_pp_JM(., s = 3), 
                     Acelera_pp_Acum_3M_JM = calcula_aceleracao_acum_S_meses_pp(., s = 3, k)), -date, -month, -quarter, -year) %>%
    
    gather(serie_tipo, valor, -date, -month, -quarter, -year) %>%
    mutate(serie_tipo = ifelse(nchar(serie_tipo) == 6, paste0(serie_tipo, "_Indice"), serie_tipo)) %>% # Mantém o mesmo nome salvando por cima
    separate(serie_tipo, into = c("Serie", "Tipo"), sep = 7) %>%
    spread(Tipo, valor) %>%
    mutate(Serie = str_sub(Serie, end = 6), 
           Data = dmy(date), 
           date_my = transforma_dia_para_mes_ano(Data),
           
           Cresci_Mensal_12M  = ifelse(Serie %in% series_pp, Cresci_pp_Mensal_12M, Cresci_Tx_Mensal_12M),
           Acelera_Mensal_12M = ifelse(Serie %in% series_pp, Acelera_pp_Mensal_12M, Acelera_Tx_Mensal_12M),
           
           Cresci_Acum_12M  = ifelse(Serie %in% series_pp, Cresci_pp_Acum_12M, Cresci_Tx_Acum_12M),
           Acelera_Acum_12M = ifelse(Serie %in% series_pp, Acelera_pp_Acum_12M, Acelera_Tx_Acum_12M),
           
           Cresci_Mensal_1M  = ifelse(Serie %in% series_pp, Cresci_pp_Mensal_1M, Cresci_Tx_Mensal_1M),
           Acelera_Mensal_1M = ifelse(Serie %in% series_pp, Acelera_pp_Mensal_1M, Acelera_Tx_Mensal_1M),
           
           Cresci_Acum_3M_JM  = ifelse(Serie %in% series_pp, Cresci_pp_Acum_3M_JM, Cresci_Tx_Acum_3M_JM),
           Acelera_Acum_3M_JM = ifelse(Serie %in% series_pp, Acelera_pp_Acum_3M_JM, Acelera_Tx_Acum_3M_JM),
           
           Cresci_Mensal_12M = ifelse(Serie %in% series_contra_ci, -1 * Cresci_Mensal_12M, Cresci_Mensal_12M),
           Acelera_Mensal_12M = ifelse(Serie %in% series_contra_ci, -1 * Acelera_Mensal_12M, Acelera_Mensal_12M),
           
           Cresci_Acum_12M = ifelse(Serie %in% series_contra_ci, -1 * Cresci_Acum_12M, Cresci_Acum_12M),
           Acelera_Acum_12M = ifelse(Serie %in% series_contra_ci, -1 * Acelera_Acum_12M, Acelera_Acum_12M),
           
           Cresci_Mensal_1M = ifelse(Serie %in% series_contra_ci, -1 * Cresci_Mensal_1M, Cresci_Mensal_1M),
           Acelera_Mensal_1M = ifelse(Serie %in% series_contra_ci, -1 * Acelera_Mensal_1M, Acelera_Mensal_1M),
           
           Cresci_Acum_3M_JM = ifelse(Serie %in% series_contra_ci, -1 * Cresci_Acum_3M_JM, Cresci_Acum_3M_JM),
           Acelera_Acum_3M_JM = ifelse(Serie %in% series_contra_ci, -1 * Acelera_Acum_3M_JM, Acelera_Acum_3M_JM)
           
    ) %>%
    select(
      -Cresci_pp_Mensal_12M, 
      -Cresci_Tx_Mensal_12M, 
      -Acelera_pp_Mensal_12M, 
      -Acelera_Tx_Mensal_12M, 
      -Cresci_pp_Acum_12M, 
      -Cresci_Tx_Acum_12M, 
      -Acelera_pp_Acum_12M, 
      -Acelera_Tx_Acum_12M, 
      -Cresci_pp_Mensal_1M, 
      -Cresci_Tx_Mensal_1M, 
      -Acelera_pp_Mensal_1M, 
      -Acelera_Tx_Mensal_1M, 
      -Cresci_pp_Acum_3M_JM,
      -Cresci_Tx_Acum_3M_JM,
      -Acelera_pp_Acum_3M_JM, 
      -Acelera_Tx_Acum_3M_JM) %>%
    inner_join(tradutor, by = c("Serie" = "CodVar")) %>%
    arrange(Data) %>%
    filter(CategoriaPIB != "Fora")
}



base_transformada <- transforma_base(base)
base_transformada_SA <- transforma_base(base_SA)


# Supondo que nenhuma é contra-cíclica
series_contra_ci <- NA
base_transformada_raw <- transforma_base(base)
base_transformada_SA_raw <- transforma_base(base_SA)


# Cria bases amigáveis para download
base_transformada_download <- base_transformada_raw %>%
                              select(NomeVar, 
                                     Fonte, 
                                     Categoria, 
                                     CategoriaPIB, 
                                     month, 
                                     year, 
                                     Indice, 
                                     Cresci_Mensal_12M, 
                                     Cresci_Acum_12M, 
                                     Acelera_Acum_12M) %>%
                              rename(Mes = month, 
                                     Ano = year, 
                                     `Crescimento Mensal em 12 Meses` = Cresci_Mensal_12M, 
                                     `Crescimento Acumulado em 12 Meses` = Cresci_Acum_12M, 
                                     `Aceleração Acumulada em 12 Meses` = Acelera_Acum_12M)


base_transformada_SA_download <- base_transformada_SA_raw %>%
                                 select(NomeVar, 
                                        Fonte, 
                                        Categoria, 
                                        CategoriaPIB, 
                                        month, 
                                        year, 
                                        Indice,
                                        Cresci_Mensal_1M,
                                        Cresci_Acum_3M_JM) %>%
                                 rename(Mes = month, Ano = year, 
                                        `Crescimento Mensal contra mês anterior` = Cresci_Mensal_1M, 
                                        `Crescimento Acumulado com média móvel de 3 meses` = Cresci_Acum_3M_JM)




# Cria uma base que possui dados e variações brutas sem ajuste sazonal e com ajuste sazonal 
aux1 <- base_transformada_raw %>%
        select(month, year, Cresci_Mensal_12M, Cresci_Acum_12M, Indice, NomeVarTabela, Categoria, Serie, Ordem) # Pega somente nesse a Categoria, pq, se não, no join ele cria um .x e .y posterior

aux2 <- base_transformada_SA_raw %>%
        select(month, year, Cresci_Mensal_1M, Cresci_Acum_3M_JM, NomeVarTabela)

tabela_variacoes_pre <- full_join(aux1, aux2, by = c("NomeVarTabela", "month", "year")) %>% 
                        filter(!is.na(Indice))




# Cria uma base auxiliar só para pegar as datas que não aparecem NA
aux_datas <- base_transformada %>%
             select(date_my, Data, Acelera_Mensal_12M, Cresci_Mensal_12M, Indice) %>%
             gather(Tipo_Transformacao, Valor, Acelera_Mensal_12M, Cresci_Mensal_12M, Indice) %>%
             na.omit() %>%
             arrange(Data)



