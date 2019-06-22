# Lista de Ícones aqui: http://fontawesome.io/icons/

library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)
#require(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
#library(radarchart)
library(flexdashboard)
library(zoo)
library(formattable)
library(DT)
library(collapsibleTree)

options(shiny.sanitize.errors = FALSE)



# base <- read_csv2("base_dados.csv")
base <- readRDS("base_ciclo_V14.rds")
base_SA <- readRDS("base_ciclo_SA_V14.rds")
#base_HP <- readRDS("base_hp.rds")

#base_pib <- readRDS("base_pib_ciclo_V3.rds")
#base_pib_sa <- readRDS("base_pib_ciclo_sa_V3.rds")

base_pib <- readRDS("base_pib_ciclo_V3_SEM_MES_MEDIANO.rds")
base_pib_sa <- readRDS("base_pib_ciclo_sa_V3_SEM_MES_MEDIANO.rds")

tradutor_eng <- readRDS("tradutor_eng_ciclo_V24.rds")
tradutor_eng$NomeVar <- stri_conv(as.character(tradutor_eng$NomeVar), "latin1", "UTF-8")
tradutor_eng$NomeVarTabela <- stri_conv(as.character(tradutor_eng$NomeVarTabela), "latin1", "UTF-8")
tradutor_eng$DescVar_Tooltips <- stri_conv(as.character(tradutor_eng$DescVar_Tooltips), "latin1", "UTF-8")
tradutor_eng$DescVar_Original <- stri_conv(as.character(tradutor_eng$DescVar_Original), "latin1", "UTF-8")
tradutor_eng$Categoria <- stri_conv(as.character(tradutor_eng$Categoria), "latin1", "UTF-8")
tradutor_eng$UnidadeMedida <- stri_conv(as.character(tradutor_eng$UnidadeMedida), "latin1", "UTF-8")
tradutor_eng$Explicacao <- stri_conv(as.character(tradutor_eng$Explicacao), "latin1", "UTF-8")


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

calcula_nota <- function(x) { (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) }

retorna_mes <- Vectorize(function(num_mes) {switch(as.character(num_mes),
                                                   "1" = "January",
                                                   "2" = "February",
                                                   "3" = "March",
                                                   "4" = "April",
                                                   "5" = "May",
                                                   "6" = "June",
                                                   "7" = "July",
                                                   "8" = "August",
                                                   "9" = "September",
                                                   "10" = "October",
                                                   "11" = "November",
                                                   "12" = "December")})

transforma_dia_para_mes_ano <- function(x) {paste0(retorna_mes(as.numeric(str_sub(x, 6, 7))),"-",str_sub(x,1,4)) } # Pega uma string "yyyy-mm-dd" e vira "NomeMês-Ano"

aplica_cor_monitor <- Vectorize(function(a,c) { if(a > 0 & c > 0) return("#279327") else
                                      if(a > 0 & c < 0) return("#ffff27") else
                                      if(a < 0 & c > 0) return("#ffb327") else
                                      if(a < 0 & c < 0) return("#ff2727") else
									  if(a == 0 | c == 0) return("#b3b3b3")}) # "black"

									 
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
                     Acelera_pp_Acum_3M_JM = calcula_aceleracao_acum_S_meses_pp(., s = 3, k), 
                     
                     Nota = calcula_nota(.)), -date, -month, -quarter, -year) %>%
    
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
           Acelera_Acum_3M_JM = ifelse(Serie %in% series_contra_ci, -1 * Acelera_Acum_3M_JM, Acelera_Acum_3M_JM),
		   
		   Nota = ifelse(Serie %in% series_contra_ci, 1 - Nota, Nota)
           
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
    inner_join(tradutor_eng, by = c("Serie" = "CodVar")) %>%
    arrange(Data) %>%
    filter(CategoriaPIB != "Fora")
}



base_transformada <- transforma_base(base)
base_transformada_SA <- transforma_base(base_SA)

# Supondo que nenhuma é contra-cíclica
series_contra_ci <- NA
base_transformada_raw <- transforma_base(base)
base_transformada_SA_raw <- transforma_base(base_SA)

# base_transformada_HP <- transforma_base(base_HP)


base_transformada_download_eng <- base_transformada_raw %>%
           select(-DescVar_Tooltips, 
			-date, 
		   -quarter, 
		   -Serie, 
		   -Nota, 
		   -Data, 
		   -date_my,
		   -NomeVarTabela, 
		   -DescVar_Tooltips, 
		   -DescVar_Original, 
		   -Cresci_Mensal_1M, 
		   -Acelera_Mensal_1M, 
		   -Acelera_Mensal_12M,
		   -Cresci_Acum_3M_JM, 
		   -Acelera_Acum_3M_JM) %>%
		   select(NomeVar:CategoriaPIB, month:Acelera_Acum_12M) %>%
		   rename(Mes = month, 
				  Ano = year, 
				  `Crescimento Mensal em 12 Meses` = Cresci_Mensal_12M, 
				  `Crescimento Acumulado em 12 Meses` = Cresci_Acum_12M, 
				  `Aceleração Acumulada em 12 Meses` = Acelera_Acum_12M)

names(base_transformada_download_eng) <- c("Name", "Source", "Category", "Timing", "Month", "Year", "Value", "Monthly Growth (12 months)", "Cumulative Growth (12 months)", "Cumulative Acceleration (12 months)")
		   
base_transformada_SA_download_eng <- base_transformada_SA_raw %>%
select(-DescVar_Tooltips, 
			-date, 
		   -quarter, 
		   -Serie, 
		   -Nota, 
		   -Data, 
		   -date_my,
		   -NomeVarTabela, 
		   -DescVar_Tooltips, 
		   -DescVar_Original,
		   -Cresci_Mensal_12M,
		   -Acelera_Mensal_12M,
		   -Cresci_Acum_12M,
		   -Acelera_Acum_12M,
		   -Acelera_Mensal_1M,
		   -Acelera_Acum_3M_JM) %>%
		   select(NomeVar:CategoriaPIB, month:Cresci_Acum_3M_JM) %>%
           rename(Mes = month, 
				  Ano = year, 
				  `Crescimento Mensal contra mês anterior` = Cresci_Mensal_1M, 
				  `Crescimento Acumulado com média móvel de 3 meses` = Cresci_Acum_3M_JM)
           		   
names(base_transformada_SA_download_eng) <- c("Name", "Source", "Category", "Timing", "Month", "Year", "Value", "Monthly Growth (1 month)", "Cumulative Growth (3 months moving average)")


aux1 <- base_transformada_raw %>%
        select(month, year, Cresci_Mensal_12M, Cresci_Acum_12M, Indice, NomeVarTabela, Categoria, Serie, Ordem) # Pega somente nesse a Categoria, pq, se não, no join ele cria um .x e .y posterior

aux2 <- base_transformada_SA_raw %>%
        select(month, year, Cresci_Mensal_1M, Cresci_Acum_3M_JM, NomeVarTabela)

tabela_brasileirao_pre <- full_join(aux1, aux2, by = c("NomeVarTabela", "month", "year")) %>% 
                          #replace_na(list(Cresci_Mensal_1M = as.numeric("")))
                          #na.omit()
						  filter(!is.na(Indice))

  
# Cria uma base auxiliar só para pegar as datas que aparecem NA em todos  
aux_datas <- base_transformada %>%
             select(date_my, Data, Acelera_Mensal_12M, Cresci_Mensal_12M, Indice, Nota) %>%
             gather(Tipo_Transformacao, Valor, Acelera_Mensal_12M, Cresci_Mensal_12M, Indice, Nota) %>%
             na.omit() %>%
			 arrange(Data)

shinyServer(function(input, output) {


  output$frame <- renderUI({
    my_test <- tags$iframe(src="http://www.fee.tche.br/barra/index.html", 
	height="31px", 
	width = "100%", 
	borderbottom = "1px",
	solid = "#6CB3D4",     
	bordertop = "0px",
	borderleft = "0px",
	borderright = "0px",
    margin = "0px",
    padding= "0px")
    print(my_test)
    #my_test
  })
  
  
  
  output$plot_series <- renderPlotly({
    
	#series_escolhidas <- input$series_compara_hierarquia
	if(stri_conv(as.character(input$series_compara_hierarquia), "UTF-8", "latin1") == "Intention of Household Consumption") series_escolhidas <- "Intention of Household Consumption" else series_escolhidas <- input$series_compara_hierarquia # Hardcodeado porque ele estava quebrando o server no input 'Comércio - Confiança'
	#if(stri_conv(as.character(input$series_compara_hierarquia), "UTF-8", "latin1") == 'Tempo Desemprego') series_escolhidas <- 'Tempo Desemprego' else series_escolhidas <- input$series_compara_hierarquia # Hardcodeado porque ele estava quebrando o server no input 'Comércio - Confiança'
	
	
	if (!input$check_series_sazonal) {base_analise <- base_transformada; subtitulo_serie <- "Not Seasonally Adjusted"} else
	                                 {base_analise <- base_transformada_SA; subtitulo_serie <- "Seasonally Adjusted"}
	
	base_aux <- base_analise %>% 
                filter(NomeVar == series_escolhidas) %>%
	            select(Indice, Data, NomeVar, NomeVarTabela, UnidadeMedida, DescVar_Tooltips, date_my)  %>% # Pega só o índice pra não dar problema no na.omit posterior das recessões
				na.omit()
	 
	if (!input$check_recessoes) {

    base_aux %>% 
      plot_ly(x = ~Data, y = ~Indice, type = 'scatter', mode = 'lines', color = ~NomeVar,
	  	      line = list(width = 3),
	          hoverinfo = "text",
              text = paste("", base_aux$DescVar_Tooltips, "<br>",
                             "Valor: ", round(base_aux$Indice, casas_decimais), "<br>",
							 "Date: ", base_aux$date_my)) %>%
      layout(title = paste0(unique(base_aux$NomeVar), "<br>", subtitulo_serie),
	         #legend = list(orientation = 'h'),
	         xaxis = list(title = ""),
			 yaxis = list(title = unique(base_aux$UnidadeMedida)),
			 plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
	         paper_bgcolor = "transparent"
			 ) %>%
			 config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
	  
	}
	
	else {
	  
	  # Vetor sem períodos anteriores a existencia de dados
	  picos<-c("1989-06-01","1994-12-01","1997-10-01","2000-12-01","2002-10-01","2008-07-01","2014-02-01")
	  vales<-c("1991-12-01","1995-09-01","1999-02-01","2001-09-01","2003-06-01","2009-01-01","2016-12-01")        #as.character(max(aux_datas$Data)))
	  
	  opa_rec <- 0.25 # Opacidade recessão
	  opa_rec_final <- 0.1 # Recessão ainda em aberto
	  col_rec <- "gray" # Cor recessão
	  
	  x_min_serie <- min(na.omit(base_aux)$Data) # Controle do eixo x, pois os retangulos de todas as recessões são incluídos
	  x_max_serie <- max(na.omit(base_aux)$Data)
	  
	  base_aux %>%
	    plot_ly(x = ~Data, y = ~Indice, type = 'scatter', mode = 'lines', color = ~NomeVar,
		        line = list(width = 3),
			    hoverinfo = "text",
                text = paste("", base_aux$DescVar_Tooltips, "<br>",
                             "Value: ", round(base_aux$Indice, casas_decimais), "<br>",
							 "Data: ", base_aux$date_my)) %>%
	    layout(title = paste0(unique(base_aux$NomeVar), "<br>", subtitulo_serie),
	           #legend = list(orientation = 'h'),
	           xaxis = list(title = "", range = c(x_min_serie, x_max_serie)),
	           yaxis = list(title = unique(base_aux$UnidadeMedida)),
	           shapes = list(
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[1], x1 = vales[1], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[2], x1 = vales[2], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[3], x1 = vales[3], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[4], x1 = vales[4], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[5], x1 = vales[5], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[6], x1 = vales[6], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[7], x1 = vales[7], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below")#,
				 
				 #list(type = "rect",
	             #     fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	             #     x0 = picos[7], x1 = picos[7], xref = "x",
	             #     y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	             #     layer = "below")
	             
	           ),
	           
	    		 plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
	             paper_bgcolor = "transparent"
		) %>%
	    config(
	      #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
	      #displaylogo = FALSE, # o displaylogo é o logo do Plotly
	      modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
	  
	  
	}
	  
  
    })

  output$fonte_series <- renderText({
  #series_escolhidas <- input$series_compara_hierarquia
  if(input$series_compara_hierarquia != "Intention of Household Consumption") {
  series_escolhidas <- input$series_compara_hierarquia
  aux <- base_transformada %>% filter(NomeVar == series_escolhidas)
  paste0("Source: ", unique(aux$Fonte))} else {paste0("Source: Fecomércio")}
  
  })
  
  #output$explicacao_series <- renderText({
  #series_escolhidas <- input$series_compara_hierarquia
  ##if(stri_conv(as.character(input$series_compara_hierarquia), "UTF-8", "latin1") == "Comércio Confiança") series_escolhidas <- "Comércio - Confiança" else series_escolhidas <- input$series_compara_hierarquia # Hardcodeado porque ele estava quebrando o server no input 'Comércio - Confiança'
  #aux <- base_transformada %>% filter(NomeVar %in% series_escolhidas)
  #paste0("Descrição: ", unique(aux$Explicacao))
  #})
  
  
  output$explicacao_series <- renderText({
  if(input$series_compara_hierarquia != "Intention of Household Consumption") {
  series_escolhidas <- input$series_compara_hierarquia
  aux <- base_transformada %>% filter(NomeVar == series_escolhidas)
  paste0("Description: ", unique(aux$Explicacao))} else {paste0("Description: Intent of Household Consumption (IHC) in Rio Grande do Sul.")}
  })
  
  

  output$plot_monitor <- renderPlotly({
  
    if(input$check_antecedente)  {a <- "Leading"} else {a <- NA}
	if(input$check_coincidente) {b <- "Coincident"} else {b <- NA}
	if(input$check_defasada)     {d <- "Lagging"} else {d <- NA}

    #input_data <- input$data_monitor

    input_ano <- lubridate::year(ymd(input$data_slider_monitor))
    input_mes <- lubridate::month(ymd(input$data_slider_monitor))
	input_categoria <- c(a,b,d)
	
	
	
	aux_eixos_fixos <- base_transformada #%>% filter(Data >= (lubridate::ymd(input$data_slider_monitor))) # Retiro dois meses como prevenção para os eixos pegarem todo mundo  - months(4)

    base_aux <- base_transformada %>%
                  # filter(date == input_data) %>%
                    filter(year == input_ano, month == input_mes, CategoriaPIB %in% input_categoria, Categoria != "Price Indices") %>%
                    na.omit()
	
	#if(!input$check_visualiza_grupo_monitor) {base_aux <- base_aux_pre}
	

    
	#if(input$check_visualiza_grupo_monitor & input$categoria_monitor == "radio_antecedente") { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Antecedente")}
	#if(input$check_visualiza_grupo_monitor & input$categoria_monitor == "radio_coincidente") { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Coincidente")}
	#if(input$check_visualiza_grupo_monitor & input$categoria_monitor == "radio_defasada")    { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Defasada")}
	

	
#	if(input$check_incluir_PIB_monitor & !input$check_acumulado_12M_monitor) { base_pib_aux <- base_pib %>% 
#	                                                                                           filter(Ano == input_ano, month == input_mes) %>% 
#	                                                                                           mutate(PIB_X = Acelera_Trimestral_4T, PIB_Y = Cresc_Trimestral_4T)
#	                                                                           #texto_flecha_pib <- "<b>PIB</b>"
#                                                                               #booleano_flecha <- T 
#																			   if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>PIB</b>"; booleano_flecha <- T}
#																			   }
										  
#	if(!input$check_incluir_PIB_monitor & !input$check_acumulado_12M_monitor){ base_pib_aux <- base_pib %>% 
#	                                                                                           filter(Ano == input_ano, month == input_mes) %>% 
#	                                                                                           mutate(PIB_X = NA, PIB_Y = NA)
#                                                                               #texto_flecha_pib <- ""
#                                                                               #booleano_flecha <- F 
#																			   if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>PIB</b>"; booleano_flecha <- T}
#																			   }
																			   
    if(input$check_incluir_PIB_monitor) { base_pib_aux <- base_pib %>% 
														   filter(Ano == input_ano, month == input_mes) %>% 
														   mutate(PIB_X = Acelera_Acum_4T, PIB_Y = Cresc_Acum_4T) %>%
														   select(PIB_X, PIB_Y)
										  if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>GDP</b>"; booleano_flecha <- T}
										  }
										  
	if(!input$check_incluir_PIB_monitor){ base_pib_aux <- base_pib %>% 
													   filter(Ano == input_ano, month == input_mes) %>% 
													   mutate(PIB_X = NA, PIB_Y = NA)
													  #texto_flecha_pib <- ""
													  #booleano_flecha <- F 
													  if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>GDP</b>"; booleano_flecha <- T}
													  }
	

	# Parâmetros Gráficos
	opacidade_cor <- 0.3 #input$opacidade_cor
	cores_estagios <- c("red", "yellow", "green", "orange") # Sentido horário

    # Excelente link de consulta para os gráficos do plotly: https://plot.ly/r/reference/#layout-shapes
	# Excelente link para o modeBar do plotly https://stackoverflow.com/questions/37437808/how-to-custom-or-display-modebar-in-plotly
	
	f <- list(size = 14, color = "black")
	
    #if (!input$check_acumulado_12M_monitor) {
    #base_aux <- rename(base_aux, 
	#                   Variavel_X = Acelera_Mensal_12M, 
	#                   Variavel_Y = Cresci_Mensal_12M)
	#tipo_variacao <- "Mês contra mesmo mês do ano anterior"
	#}
	
	#if (input$check_acumulado_12M_monitor) {
    base_aux <- rename(base_aux, 
	                   Variavel_X = Acelera_Acum_12M, 
	                   Variavel_Y = Cresci_Acum_12M)
	tipo_variacao <- "12 months cumulative variation" # "Variação acum. em 12 meses"
	#}
	
	
    if (!input$check_monitor_eixos_fixos) {
    limite_cores_x <- 1.05 * max(abs(c(base_aux$Variavel_X, base_pib_aux$PIB_X)), na.rm = T) # Percentual a mais do que o máximo filtrado (e o PIB)
    limite_cores_y <- 1.05 * max(abs(c(base_aux$Variavel_Y, base_pib_aux$PIB_Y)), na.rm = T)
	}
	
	
	#if (input$check_monitor_eixos_fixos & !input$check_acumulado_12M_monitor) {
	#limite_cores_x <- max(abs(base_transformada$Acelera_Mensal_12M), na.rm = T) # Percentual a mais do que o máximo filtrado
    #limite_cores_y <- max(abs(base_transformada$Cresci_Mensal_12M), na.rm = T)
	#}
	
	if (input$check_monitor_eixos_fixos) {
	#limite_cores_x <- 1.1 * max(abs(base_transformada$Acelera_Acum_12M), na.rm = T) # Percentual a mais do que o máximo filtrado
    #limite_cores_y <- 1.1 * max(abs(base_transformada$Cresci_Acum_12M), na.rm = T)
	limite_cores_x <- 1.05 * max(c(abs(aux_eixos_fixos$Acelera_Acum_12M), abs(base_pib_aux$PIB_X)), na.rm = T) # Percentual a mais do que o máximo filtrado
    limite_cores_y <- 1.05 * max(c(abs(aux_eixos_fixos$Cresci_Acum_12M), abs(base_pib_aux$PIB_Y)), na.rm = T)
	}

    # Maneira 1
    base_aux %>%
      plot_ly(x = ~Variavel_X, y = ~Variavel_Y, type = 'scatter', mode = 'markers', color = ~NomeVar,
              marker = list(size = 13, sizeref = .15, color = ifelse(base_aux$CategoriaPIB == "Coincident", "#004B82", 
															  ifelse(base_aux$CategoriaPIB == "Leading", "#004B82", "#004B82")),
															  symbol = 1:length(base_aux$NomeVar) + 12), # Controla o tamanho do marker # color = "#004B82" color = ifelse(base_aux$CategoriaPIB == "Coincidente", "#004B82", ifelse(base_aux$CategoriaPIB == "Antecedente", "#FF0000", "#00FF00"))
              hoverinfo = "text",
              text = paste("", base_aux$DescVar_Tooltips, "<br>",
                           "Growth: ", round(base_aux$Variavel_Y, casas_decimais), "<br>",
                           "Acceleration: ", round(base_aux$Variavel_X, casas_decimais)),
              showlegend = TRUE) %>%

      layout(title = paste0("Dynamics in ", retorna_mes(input_mes) , " of ", input_ano, " — ", tipo_variacao),
             shapes = list(
               list(type = "rect",
                    fillcolor = cores_estagios[1], line = list(color = cores_estagios[1]), opacity = opacidade_cor,
                    x0 = -limite_cores_x, x1 = 0, xref = "x",
                    y0 = -limite_cores_y, y1 = 0, yref = "y",
                    layer = "below"),
               list(type = "rect",
                    fillcolor = cores_estagios[2], line = list(color = cores_estagios[2]), opacity = opacidade_cor,
                    x0 = -limite_cores_x, x1 = 0, xref = "x",
                    y0 = 0, y1 = limite_cores_y, yref = "y",
                    layer = "below"),
               list(type = "rect",
                    fillcolor = cores_estagios[3], line = list(color = cores_estagios[3]), opacity = opacidade_cor,
                    x0 = 0, x1 = limite_cores_x, xref = "x",
                    y0 = 0, y1 = limite_cores_y, yref = "y",
                    layer = "below"),
               list(type = "rect",
                    fillcolor = cores_estagios[4], line = list(color = cores_estagios[4]), opacity = opacidade_cor,
                    x0 = 0, x1 = limite_cores_x, xref = "x",
                    y0 = -limite_cores_y, y1 = 0, yref = "y",
                    layer = "below")),

             xaxis = list(
               zeroline = FALSE,
               showline = FALSE,
               showgrid = FALSE,
			   range = c(-limite_cores_x, limite_cores_x), 
               title = "Acceleration (p.p.)",
			   titlefont = f
             ),

             yaxis = list(
               #scaleanchor = "x",
               zeroline = FALSE,
               showline = FALSE,
               showgrid = FALSE,
			   range = c(-limite_cores_y, limite_cores_y), 
               title = "Growth (%)",
			   titlefont = f
             ),
			 
			 #,
             #legend = list(orientation = 'h'),
			 plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
	         paper_bgcolor = "transparent"
			 
			 )%>%
			 
	add_annotations(
    x = base_pib_aux$PIB_X,
    y = base_pib_aux$PIB_Y,
    #xref = "paper",
    #yref = "paper",
    text = texto_flecha_pib,
    showarrow = booleano_flecha) %>%
  
    add_markers(
    x = base_pib_aux$PIB_X,
    y = base_pib_aux$PIB_Y,
    text = paste0("GDP","<br>",
                  "Growth:", round(base_pib_aux$PIB_Y, casas_decimais), "<br>",
                  "Acceleration:", round(base_pib_aux$PIB_X, casas_decimais)),
    xanchor = 'center',
    marker = list(color = "black", symbol = 500),
    showlegend = FALSE) %>%
			 

	
	         config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))

  })
  
  
  
  

  
  



output$plot_series_teste <- renderPlotly({


	if(input$series_compara_teste == "Panorama"){
		series_escolhidas <- unique(base_transformada$NomeVar)	
	} 
		
	if (!input$check_series_sazonal) {base_analise <- base_transformada; subtitulo_serie <- "Sem ajuste Sazonal"} else
	                                 {base_analise <- base_transformada_SA; subtitulo_serie <- "Com ajuste Sazonal"}
	
	base_aux <- base_analise %>% 
                filter(NomeVar %in% series_escolhidas) %>%
	            select(Indice, Data, NomeVar, DescVar_Tooltips, date_my) # Pega só o índice pra não dar problema no na.omit posterior das recessões
	
	base_aux$UnidadeMedida <- stri_conv(as.character(base_aux$UnidadeMedida), "latin1", "UTF-8")
	
	if (!input$check_recessoes) {

    base_aux %>% 
      plot_ly(x = ~Data, y = ~Indice, type = 'scatter', mode = 'lines', color = ~NomeVar,
	  	      line = list(width = 3),
	          hoverinfo = "text",
              text = paste("", base_aux$DescVar_Tooltips, "<br>",
                             "Valor: ", round(base_aux$Indice, casas_decimais), "<br>",
							 "Data: ", base_aux$date_my)) %>%
      layout(title = paste0("Evolução Temporal de Indicadores do Ciclo de Negócios", "<br>", subtitulo_serie),
	         #legend = list(orientation = 'h'),
	         xaxis = list(title = ""),
			 plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
	         paper_bgcolor = "transparent"
			 ) %>%
			 config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
	  
	}
	
	else {
	  
	  # Vetor sem períodos anteriores a existencia de dados
	  picos<-c("1989-06-01","1994-12-01","1997-10-01","2000-12-01","2002-10-01","2008-07-01","2014-02-01")
	  vales<-c("1991-12-01","1995-09-01","1999-02-01","2001-09-01","2003-06-01","2009-01-01",as.character(max(aux_datas$Data)))
	  
	  opa_rec <- 0.25 # Opacidade recessão
	  col_rec <- "gray" # Cor recessão
	  
	  x_min_serie <- min(na.omit(base_aux)$Data) # Controle do eixo x, pois os retangulos de todas as recessões são incluídos
	  x_max_serie <- max(na.omit(base_aux)$Data)
	  
	  base_aux %>%
	    plot_ly(x = ~Data, y = ~Indice, type = 'scatter', mode = 'lines', color = ~NomeVar,
		        line = list(width = 3),
			    hoverinfo = "text",
                text = paste("", base_aux$DescVar_Tooltips, "<br>",
                             "Valor: ", round(base_aux$Indice, casas_decimais), "<br>",
							 "Data: ", base_aux$date_my)) %>%
	    layout(title = paste0("Evolução Temporal de Indicadores do Ciclo de Negócios", "<br>", subtitulo_serie),
	           #legend = list(orientation = 'h'),
	           xaxis = list(title = "", range = c(x_min_serie, x_max_serie)),
	           
	           shapes = list(
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[1], x1 = vales[1], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[2], x1 = vales[2], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[3], x1 = vales[3], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[4], x1 = vales[4], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[5], x1 = vales[5], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[6], x1 = vales[6], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[7], x1 = vales[7], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below")
	             
	           ),
	           
	    		 plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
	             paper_bgcolor = "transparent"
		) %>%
	    config(
	      #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
	      #displaylogo = FALSE, # o displaylogo é o logo do Plotly
	      modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
	  
	  
	}
	  
  
    })
	
	output$titulo_tabela <- renderText({
	
	ano_data_tabela <- lubridate::year(ymd(input$data_jogo))
	mes_data_tabela <- lubridate::month(ymd(input$data_jogo))
	
	paste0("Synthetic Variation Table in ", retorna_mes(mes_data_tabela), " of ", ano_data_tabela)
	
	})
	
	output$tabela_brazuca <- renderFormattable ({
	
    series_contra_ci <- c("s_1005", "s_1006", "s_1016")
	
	casas_decimais <- 1
	
	data_analise <- input$data_jogo
	
	input_ano <- lubridate::year(ymd(data_analise))
	input_mes <- lubridate::month(ymd(data_analise))
	input_grupo <- input$grupo_tabela
	
	if(!input$check_grupo_tabela) {
	tabela_brasileirao <- tabela_brasileirao_pre %>%
							filter(year == input_ano, month == input_mes) %>%
							arrange(Ordem) %>% # arrange(Categoria, Ordem) %>%
							select(NomeVarTabela, Indice, Cresci_Mensal_12M, Cresci_Acum_12M, Cresci_Mensal_1M, Cresci_Acum_3M_JM, Serie, Categoria)}
	
	if(input$check_grupo_tabela) {
	tabela_brasileirao <- tabela_brasileirao_pre %>%
							filter(year == input_ano, month == input_mes, Categoria == input_grupo) %>%
							arrange(Ordem) %>% # arrange(Categoria, Ordem) %>%
							select(NomeVarTabela, Indice, Cresci_Mensal_12M, Cresci_Acum_12M, Cresci_Mensal_1M, Cresci_Acum_3M_JM, Serie, Categoria)}

	fixedWidth = 295

	colnames(tabela_brasileirao) <- c("Series Name", "Raw Value", "Month vs. same month of the previous year*", "Average in 12 months*", "Month vs. immediately preceding month**", "Moving Average (3 Months)**", "Series Code", "Category")
	
	
	formattable(tabela_brasileirao, 
		align = c("l", "c", "r", "r", "r", "r"),
		list(

		"Series Name" = formatter(.tag = "span", style = function(x) style(
																    display = "inline-block",
																	"text-align" = "left",
    	   	                                                        #direction = "rtl", `border-radius` = "4px", 
                                                                  	#`padding-right` = "2px", 
                                                                  	`background-color` = "transparent",
                                                                    width = paste0(fixedWidth, "px", sep = ""))),

		#ifelse(tabela_brasileirao$"Nome da Série" == "Agronegócio (pessoas ocupadas)", 0, 1)

		"Raw Value" = formatter("span",
                         #formatC(tabela_brasileirao$"Raw Value", digits = 1, big.mark = ".", format = "f", decimal.mark = ",")
						 ifelse(tabela_brasileirao$"Series Name" == "Employment - Agribusiness (employed people)", 
						 formatC(tabela_brasileirao$"Raw Value", digits = 0, big.mark = ".", format = "f", decimal.mark = ","), 
						 formatC(tabela_brasileirao$"Raw Value", digits = 1, big.mark = ".", format = "f", decimal.mark = ","))
						 ),

		"Month vs. same month of the previous year*" = formatter("span",
                         style = x ~ style(color = ifelse(tabela_brasileirao$"Category" == "Price Indices", NA, ifelse(tabela_brasileirao$"Month vs. same month of the previous year*" > 0, ifelse(tabela_brasileirao$"Series Code" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Series Code" %in% series_contra_ci, "green", "red"))), width = paste0(200, "px", sep = "")),
                         format(round(tabela_brasileirao$"Month vs. same month of the previous year*", casas_decimais), format = "f", decimal.mark = ","),
                         x ~ icontext(ifelse(tabela_brasileirao$"Month vs. same month of the previous year*" > 0, "arrow-up", "arrow-down"))),
		"Average in 12 months*" = formatter("span",
                                style = x ~ style(color = ifelse(tabela_brasileirao$"Category" == "Price Indices", NA, ifelse(tabela_brasileirao$"Average in 12 months*" > 0, ifelse(tabela_brasileirao$"Series Code" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Series Code" %in% series_contra_ci, "green", "red")))),
                                format(round(tabela_brasileirao$"Average in 12 months*", casas_decimais), format = "f", decimal.mark = ","),
                                x ~ icontext(ifelse(tabela_brasileirao$"Average in 12 months*" > 0, "arrow-up", "arrow-down"))),
		"Month vs. immediately preceding month**" = formatter("span",
                                style = x ~ style(color = ifelse(tabela_brasileirao$"Category" == "Price Indices", NA, ifelse(tabela_brasileirao$"Month vs. immediately preceding month**" > 0, ifelse(tabela_brasileirao$"Series Code" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Series Code" %in% series_contra_ci, "green", "red")))),
                                format(round(tabela_brasileirao$"Month vs. immediately preceding month**", casas_decimais), format = "f", decimal.mark = ","),
                                x ~ icontext(ifelse(tabela_brasileirao$"Month vs. immediately preceding month**" > 0, "arrow-up", "arrow-down"))),
		"Moving Average (3 Months)**" = formatter("span",
                                style = x ~ style(color = ifelse(tabela_brasileirao$"Category" == "Price Indices", NA, ifelse(tabela_brasileirao$"Moving Average (3 Months)**" > 0, ifelse(tabela_brasileirao$"Series Code" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Series Code" %in% series_contra_ci, "green", "red")))),
                                format(round(tabela_brasileirao$"Moving Average (3 Months)**", casas_decimais), format = "f", decimal.mark = ","),
                                x ~ icontext(ifelse(tabela_brasileirao$"Moving Average (3 Months)**" > 0, "arrow-up", "arrow-down"))),
								
		"Series Code" = FALSE,
		"Category" = FALSE
))
	
	})
	
	
	
	
	output$tabela_download_sem_ajuste = renderDataTable({
    datatable(base_transformada_download_eng, options(list(
      #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
      pageLength = 15)))
    })
  
  
    datasetInput_sem <- reactive({
	base_transformada_download_eng
    })
  
  output$downloadData_sem <- downloadHandler(
    filename = function() { paste('base_without_adjustment', '.csv', sep = '') },
    content = function(file) {
      write.csv(datasetInput_sem(), file, fileEncoding = "latin1")
    }
  )
  
  
  
  	
	output$tabela_download_com_ajuste = renderDataTable({
    datatable(base_transformada_SA_download_eng, options(list(
      #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
      pageLength = 15)))
    })
  
  
    datasetInput_com <- reactive({
    base_transformada_SA_download_eng
    })
  
  output$downloadData_com <- downloadHandler(
    filename = function() { paste('base_with_adjustment', '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput_com(), file, fileEncoding = "latin1")
    }
  )

  output$collapsible_bct <- renderCollapsibleTree({

  	tradutor_eng_aux <- tradutor_eng %>%
  					filter(CodVar %in% unique(base_transformada$Serie))
  	
  	collapsibleTree(tradutor_eng_aux,
  		hierarchy = c("Categoria", "NomeVar"),
  		root = "BCT-RS",
  		fontSize = 15)
  	})
	
  output$downloadMetodologia <- downloadHandler(
    filename = "BCT_Methodology.pdf",
    content = function(file) {
      file.copy("www/meg_metodologia.pdf", file)
    }
  )
  











  
})