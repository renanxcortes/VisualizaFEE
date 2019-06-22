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
base <- readRDS("base_ciclo_V3.rds")
base_SA <- readRDS("base_ciclo_SA_V3.rds")
base_HP <- readRDS("base_hp.rds")

#base_pib <- readRDS("base_pib_ciclo_V3.rds")
#base_pib_sa <- readRDS("base_pib_ciclo_sa_V3.rds")

base_pib <- readRDS("base_pib_ciclo_V3_SEM_MES_MEDIANO.rds")
base_pib_sa <- readRDS("base_pib_ciclo_sa_V3_SEM_MES_MEDIANO.rds")

tradutor <- readRDS("tradutor_ciclo_V14.rds")
tradutor$NomeVar <- stri_conv(as.character(tradutor$NomeVar), "latin1", "UTF-8")
tradutor$NomeVarTabela <- stri_conv(as.character(tradutor$NomeVarTabela), "latin1", "UTF-8")
tradutor$DescVar_Tooltips <- stri_conv(as.character(tradutor$DescVar_Tooltips), "latin1", "UTF-8")
tradutor$DescVar_Original <- stri_conv(as.character(tradutor$DescVar_Original), "latin1", "UTF-8")
tradutor$Categoria <- stri_conv(as.character(tradutor$Categoria), "latin1", "UTF-8")
tradutor$UnidadeMedida <- stri_conv(as.character(tradutor$UnidadeMedida), "latin1", "UTF-8")
tradutor$Explicacao <- stri_conv(as.character(tradutor$Explicacao), "latin1", "UTF-8")


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

# base_transformada_HP <- transforma_base(base_HP)


base_transformada_download <- base_transformada_raw %>%
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
		   rename(Mes = month, Ano = year, `Crescimento Mensal em 12 Meses` = Cresci_Mensal_12M, `Crescimento Acumulado em 12 Meses` = Cresci_Acum_12M, `Aceleração Acumulada em 12 Meses` = Acelera_Acum_12M)
		   
		   
base_transformada_SA_download <- base_transformada_SA_raw %>%
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
           rename(Mes = month, Ano = year, `Crescimento Mensal contra mês anterior` = Cresci_Mensal_1M, `Crescimento Acumulado com média móvel de 3 meses` = Cresci_Acum_3M_JM)
           		   



aux1 <- base_transformada_raw %>%
        select(month, year, Cresci_Mensal_12M, Cresci_Acum_12M, Indice, NomeVarTabela, Categoria, Serie) # Pega somente nesse a Categoria, pq, se não, no join ele cria um .x e .y posterior

aux2 <- base_transformada_SA_raw %>%
        select(month, year, Cresci_Mensal_1M, Cresci_Acum_3M_JM, NomeVarTabela)

tabela_brasileirao <- inner_join(aux1, aux2, by = c("month", "year", "NomeVarTabela")) %>% 
                      na.omit()

  
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
    
	series_escolhidas <- input$series_compara_hierarquia
	
	if (!input$check_series_sazonal) {base_analise <- base_transformada; subtitulo_serie <- "Sem ajuste sazonal"} else
	                                 {base_analise <- base_transformada_SA; subtitulo_serie <- "Com ajuste sazonal"}
	
	base_aux <- base_analise %>% 
                filter(NomeVar %in% series_escolhidas) %>%
	            select(Indice, Data, NomeVar, NomeVarTabela, UnidadeMedida, DescVar_Tooltips, date_my) # Pega só o índice pra não dar problema no na.omit posterior das recessões
	
	if (!input$check_recessoes) {

    base_aux %>% 
      plot_ly(x = ~Data, y = ~Indice, type = 'scatter', mode = 'lines', color = ~NomeVar,
	  	      line = list(width = 3),
	          hoverinfo = "text",
              text = paste("", base_aux$DescVar_Tooltips, "<br>",
                             "Valor: ", round(base_aux$Indice, casas_decimais), "<br>",
							 "Data: ", base_aux$date_my)) %>%
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
	  vales<-c("1991-12-01","1995-09-01","1999-02-01","2001-09-01","2003-06-01","2009-01-01",as.character(max(aux_datas$Data)))
	  
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
                             "Valor: ", round(base_aux$Indice, casas_decimais), "<br>",
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
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec_final,
	                  x0 = picos[7], x1 = vales[7], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
				 
				 list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[7], x1 = picos[7], xref = "x",
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

  output$fonte_series <- renderText({
  series_escolhidas <- input$series_compara_hierarquia
  aux <- base_transformada %>% filter(NomeVar %in% series_escolhidas)
  paste0("Fonte: ", unique(aux$Fonte))
  
  })
  
  output$explicacao_series <- renderText({
  series_escolhidas <- input$series_compara_hierarquia
  aux <- base_transformada %>% filter(NomeVar %in% series_escolhidas)
  paste0("Descrição: ", unique(aux$Explicacao))
  
  })
  
  

  output$plot_monitor <- renderPlotly({

    #input_data <- input$data_monitor

    input_ano <- lubridate::year(ymd(input$data_slider_monitor))
    input_mes <- lubridate::month(ymd(input$data_slider_monitor))
	input_categoria <- input$categorias_monitor
	
	aux_eixos_fixos <- base_transformada #%>% filter(Data >= (lubridate::ymd(input$data_slider_monitor))) # Retiro dois meses como prevenção para os eixos pegarem todo mundo  - months(4)

    base_aux_pre <- base_transformada %>%
                  # filter(date == input_data) %>%
                    filter(year == input_ano, month == input_mes) %>%
                    na.omit()
	
	if(!input$check_visualiza_grupo_monitor) {base_aux <- base_aux_pre}
    
	#if(input$check_visualiza_grupo_monitor & input$categoria_monitor == "radio_antecedente") { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Antecedente")}
	#if(input$check_visualiza_grupo_monitor & input$categoria_monitor == "radio_coincidente") { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Coincidente")}
	#if(input$check_visualiza_grupo_monitor & input$categoria_monitor == "radio_defasada")    { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Defasada")}
	
	base_aux <- base_aux_pre %>% filter(CategoriaPIB %in% input_categoria)
	
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
										  if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>PIB</b>"; booleano_flecha <- T}
										  }
										  
	if(!input$check_incluir_PIB_monitor){ base_pib_aux <- base_pib %>% 
													   filter(Ano == input_ano, month == input_mes) %>% 
													   mutate(PIB_X = NA, PIB_Y = NA)
													  #texto_flecha_pib <- ""
													  #booleano_flecha <- F 
													  if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>PIB</b>"; booleano_flecha <- T}
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
	tipo_variacao <- "Variação acum. em 12 meses"
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
              marker = list(size = 13, sizeref = .15, color = "#004B82", symbol = 1:length(base_aux$NomeVar) + 12), # Controla o tamanho do marker
              hoverinfo = "text",
              text = paste("", base_aux$DescVar_Tooltips, "<br>",
                           "Crescimento: ", round(base_aux$Variavel_Y, casas_decimais), "<br>",
                           "Aceleração: ", round(base_aux$Variavel_X, casas_decimais)),
              showlegend = TRUE) %>%

      layout(title = paste0("Dinâmica em ", retorna_mes(input_mes) , " de ", input_ano, " — ", tipo_variacao),
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
               title = "Aceleração (p.p.)",
			   titlefont = f
             ),

             yaxis = list(
               #scaleanchor = "x",
               zeroline = FALSE,
               showline = FALSE,
               showgrid = FALSE,
			   range = c(-limite_cores_y, limite_cores_y), 
               title = "Crescimento (%)",
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
    text = paste0("PIB","<br>",
                  "Crescimento:", round(base_pib_aux$PIB_Y, casas_decimais), "<br>",
                  "Aceleração:", round(base_pib_aux$PIB_X, casas_decimais)),
    xanchor = 'center',
    marker = list(color = "black", symbol = 500),
    showlegend = FALSE) %>%
			 

	
	         config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))

  })
  
  
  
  

  
  output$plot_monitor_com_sa <- renderPlotly({

    #input_data <- input$data_monitor

    input_ano <- lubridate::year(ymd(input$data_slider_monitor_com_sa))
    input_mes <- lubridate::month(ymd(input$data_slider_monitor_com_sa))


    base_aux_pre <- base_transformada_SA %>%
                  # filter(date == input_data) %>%
                    filter(year == input_ano, month == input_mes) %>%
                    na.omit()
	
	if(!input$check_visualiza_grupo_monitor_com_sa) {base_aux <- base_aux_pre}
    
	if(input$check_visualiza_grupo_monitor_com_sa & input$categoria_monitor_com_sa == "radio_antecedente_com_sa") { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Antecedente")}
	if(input$check_visualiza_grupo_monitor_com_sa & input$categoria_monitor_com_sa == "radio_coincidente_com_sa") { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Coincidente")}
	if(input$check_visualiza_grupo_monitor_com_sa & input$categoria_monitor_com_sa == "radio_defasada_com_sa")    { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Defasada")}
	
	if(input$check_incluir_PIB_monitor_com_sa & !input$check_acumulado_6M_monitor_com_sa) { base_pib_aux <- base_pib_sa %>% 
	                                                                                                        filter(Ano == input_ano, month == input_mes) %>% 
	                                                                                                        mutate(PIB_X = Acelera_Trimestral_1T, PIB_Y = Cresc_Trimestral_1T)
																											#texto_flecha_pib <- "<b>PIB</b>"
																											#booleano_flecha <- T 
																											if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>PIB</b>"; booleano_flecha <- T}
																						  }
										  
	if(!input$check_incluir_PIB_monitor_com_sa & !input$check_acumulado_6M_monitor_com_sa){ base_pib_aux <- base_pib_sa %>% 
	                                                                                           filter(Ano == input_ano, month == input_mes) %>% 
	                                                                                           mutate(PIB_X = NA, PIB_Y = NA)
                                                                               #texto_flecha_pib <- ""
                                                                               #booleano_flecha <- F 
																			   if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>PIB</b>"; booleano_flecha <- T}
																			   }
																			   
    if(input$check_incluir_PIB_monitor_com_sa & input$check_acumulado_6M_monitor_com_sa) { base_pib_aux <- base_pib_sa %>% 
	                                                                                           filter(Ano == input_ano, month == input_mes) %>% 
	                                                                                           mutate(PIB_X = Acelera_Acum_1T, PIB_Y = Cresc_Acum_1T) %>% # Observe que estou fazendo a variação na Janela Móvel de 3 meses a mesma variação do t contra (t-1) (neste caso, "Cresc_Trimestral_1T" = "Cresc_Acum_1T")
																							   select(PIB_X, PIB_Y)
	                                                                          if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>PIB</b>"; booleano_flecha <- T}
																			  }
										  
	if(!input$check_incluir_PIB_monitor_com_sa & input$check_acumulado_6M_monitor_com_sa){ base_pib_aux <- base_pib_sa %>% 
	                                                                                           filter(Ano == input_ano, month == input_mes) %>% 
	                                                                                           mutate(PIB_X = NA, PIB_Y = NA)
                                                                              #texto_flecha_pib <- ""
                                                                              #booleano_flecha <- F 
																			  if(is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {texto_flecha_pib <- ""; booleano_flecha <- F; base_pib_aux[1,] <- NA} else {texto_flecha_pib <- "<b>PIB</b>"; booleano_flecha <- T}
																			  }
	

	# Parâmetros Gráficos
	opacidade_cor <- 0.30 #input$opacidade_cor_com_sa
	# cores_estagios <- c("red", "yellow", "green", "orange") # Sentido horário
	cores_estagios <- c("red", "yellow", "green", "orange") # Sentido horário

    # Excelente link de consulta para os gráficos do plotly: https://plot.ly/r/reference/#layout-shapes
	# Excelente link para o modeBar do plotly https://stackoverflow.com/questions/37437808/how-to-custom-or-display-modebar-in-plotly
	
	f <- list(size = 14, color = "black")
	
    if (!input$check_acumulado_6M_monitor_com_sa) {
    base_aux <- rename(base_aux, 
	                   Variavel_X = Acelera_Mensal_1M, 
	                   Variavel_Y = Cresci_Mensal_1M)
	tipo_variacao <- "Mês contra mês anterior"
	}
	
	if (input$check_acumulado_6M_monitor_com_sa) {
    base_aux <- rename(base_aux, 
	                   Variavel_X = Acelera_Acum_3M_JM, 
	                   Variavel_Y = Cresci_Acum_3M_JM)
    tipo_variacao <- "Trimeste contra trimestre imediatamente anterior"
	}
	
	
    if (!input$check_monitor_eixos_fixos_com_sa) {
    limite_cores_x <- 1.1 * max(abs(c(base_aux$Variavel_X, base_pib_aux$PIB_X)), na.rm = T) # Percentual a mais do que o máximo filtrado (e o PIB)
    limite_cores_y <- 1.1 * max(abs(c(base_aux$Variavel_Y, base_pib_aux$PIB_Y)), na.rm = T)
	}
	
	
	if (input$check_monitor_eixos_fixos_com_sa & !input$check_acumulado_6M_monitor_com_sa) {
	limite_cores_x <- max(abs(base_transformada_SA$Acelera_Mensal_1M), na.rm = T) # Percentual a mais do que o máximo filtrado
    limite_cores_y <- max(abs(base_transformada_SA$Cresci_Mensal_1M), na.rm = T)
	}
	
	if (input$check_monitor_eixos_fixos_com_sa & input$check_acumulado_6M_monitor_com_sa) {
	limite_cores_x <- max(abs(base_transformada_SA$Acelera_Acum_3M_JM), na.rm = T) # Percentual a mais do que o máximo filtrado
    limite_cores_y <- max(abs(base_transformada_SA$Cresci_Acum_3M_JM), na.rm = T)
	}

    # Maneira 1
    base_aux %>%
      plot_ly(x = ~Variavel_X, y = ~Variavel_Y, type = 'scatter', mode = 'markers', color = ~NomeVar,
              marker = list(size = 17, sizeref = .15, color = "#004B82", symbol = 1:length(base_aux$NomeVar) + 12), # Controla o tamanho do marker
              hoverinfo = "text",
              text = paste("", base_aux$DescVar_Tooltips, "<br>",
                           "Crescimento: ", round(base_aux$Variavel_Y, casas_decimais), "<br>",
                           "Aceleração: ", round(base_aux$Variavel_X, casas_decimais)),
              showlegend = TRUE) %>%

      layout(title = paste0("Monitor em ", retorna_mes(input_mes) , " de ", input_ano, " — ", tipo_variacao),
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
               title = "Aceleração (p.p.)",
			   titlefont = f
             ),

             yaxis = list(
               #scaleanchor = "x",
               zeroline = FALSE,
               showline = FALSE,
               showgrid = FALSE,
			   range = c(-limite_cores_y, limite_cores_y), 
               title = "Crescimento (%)",
			   titlefont = f
             ),
			 
			 #,
             #legend = list(orientation = 'h')
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
    text = paste0("PIB","<br>",
                  "Crescimento:", round(base_pib_aux$PIB_Y, casas_decimais), "<br>",
                  "Aceleração:", round(base_pib_aux$PIB_X, casas_decimais)),
    xanchor = 'center',
    marker = list(color = "black", symbol = 500),
    showlegend = FALSE) %>%
			 

	
	         config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))

  })
  

  
  output$plot_monitor_cores_no_scatter <- renderPlotly({

    #input_data <- input$data_monitor

    input_ano <- lubridate::year(ymd(input$data_slider_monitor))
    input_mes <- lubridate::month(ymd(input$data_slider_monitor))

    if (!input$check_acumulado_12M_monitor) {
    base_aux_pre <- base_transformada %>%
                  # filter(date == input_data) %>%
                    filter(year == input_ano, month == input_mes) %>%
                    na.omit() %>%
                    mutate(Cor_Monitor = aplica_cor_monitor(Acelera, Cresci)) %>%
                    arrange(NomeVar) # As cores estavam ficando na ordem alfabética dos nomes das variáveis. Ou seja, a lista de atributos dentro do 'marker' não estava conversando bem com o que estava fora dele.
					}
					
	if (input$check_acumulado_12M_monitor) {
    base_aux_pre <- base_transformada %>%
                  # filter(date == input_data) %>%
                    filter(year == input_ano, month == input_mes) %>%
                    na.omit() %>%
                    mutate(Cor_Monitor = aplica_cor_monitor(Acelera_12M, Cresci_12M)) %>%
                    arrange(NomeVar) # As cores estavam ficando na ordem alfabética dos nomes das variáveis. Ou seja, a lista de atributos dentro do 'marker' não estava conversando bem com o que estava fora dele.
					}
  
  
  	if(!input$check_visualiza_grupo_monitor) {base_aux <- base_aux_pre}
    
	if(input$check_visualiza_grupo_monitor & input$categoria_monitor == "radio_antecedente") { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Antecedente")}
	if(input$check_visualiza_grupo_monitor & input$categoria_monitor == "radio_coincidente") { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Coincidente")}
	if(input$check_visualiza_grupo_monitor & input$categoria_monitor == "radio_defasada")    { base_aux <- base_aux_pre %>% filter(CategoriaPIB == "Defasada")} 

# Excelente link de consulta para os gráficos do plotly: https://plot.ly/r/reference/#layout-shapes


    if (!input$check_acumulado_12M_monitor) {
    base_aux <- rename(base_aux, 
	                   Variavel_X = Acelera, 
	                   Variavel_Y = Cresci)
	}
	
	if (input$check_acumulado_12M_monitor) {
    base_aux <- rename(base_aux, 
	                   Variavel_X = Acelera_12M, 
	                   Variavel_Y = Cresci_12M)
	}

	if (!input$check_monitor_eixos_fixos) {
    limite_x <- 1.1 * max(abs(base_aux$Variavel_X)) # Percentual a mais do que o máximo filtrado
    limite_y <- 1.1 * max(abs(base_aux$Variavel_Y))
	}
	
	if (input$check_monitor_eixos_fixos & !input$check_acumulado_12M_monitor) {
	limite_x <- max(abs(base_transformada$Acelera), na.rm = T) # Percentual a mais do que o máximo filtrado
    limite_y <- max(abs(base_transformada$Cresci), na.rm = T)
	}
	
	if (input$check_monitor_eixos_fixos & input$check_acumulado_12M_monitor) {
	limite_x <- max(abs(base_transformada$Acelera_12M), na.rm = T) # Percentual a mais do que o máximo filtrado
    limite_y <- max(abs(base_transformada$Cresci_12M), na.rm = T)
	}


f <- list(size = 14, color = "black")

# Maneira 1
base_aux %>%
  plot_ly(x = ~Variavel_X, y = ~Variavel_Y, type = 'scatter', mode = 'markers', color = ~NomeVar,
          marker = list(size = 17, sizeref = .15, color = ~Cor_Monitor, symbol = 1:length(base_aux$NomeVar) + 12), # Controla o tamanho do marker
          hoverinfo = "text",
          text = paste("", base_aux$DescVar_Tooltips, "<br>",
                       "Crescimento: ", round(base_aux$Variavel_Y, casas_decimais), "<br>",
                       "Aceleração: ", round(base_aux$Variavel_X, casas_decimais)),
          showlegend = TRUE) %>%
  
  layout(title = paste0("Monitor em ", retorna_mes(input_mes) , " de ", input_ano),

         xaxis = list(
           range = c(-limite_x, limite_x),
           zeroline = TRUE,
		   zerolinecolor = "#b3b3b3",
           showline = FALSE,
           showgrid = FALSE,
           title = "Aceleração (p.p.)",
           titlefont = f
         ),
         
         yaxis = list(
           #scaleanchor = "x",
		   range = c(-limite_y, limite_y),
           zeroline = TRUE,
		   zerolinecolor = "#b3b3b3",
           showline = FALSE,
           showgrid = FALSE,
           title = "Crescimento (%)",
           titlefont = f
         ),
		 
		 plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
	     paper_bgcolor = "transparent"
         
         #legend = list(orientation = 'h')
  )%>%
	         config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))

  })
  
  

  output$plot_radar_ok <- renderPlotly({
  
  input_datas <- input$datas_radar

  base_aux_radar_pre <- base_transformada_HP %>%
  #filter(year == input_ano, month %in% input_mes) %>%
  filter(date_my %in% input_datas) %>%
  #select(date_my, Nota, NomeVar) %>%
  select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
  na.omit()

series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))


base_aux_radar <- base_aux_radar_pre %>%
                  filter(NomeVar %in% series_presentes_em_todos_periodos)


df <- base_aux_radar

nodos_verdade <- length(df$NomeVar)/length(input_datas)

taxa_degree <- 360/nodos_verdade

df$degree <- seq(0, 360-taxa_degree, taxa_degree) # Adaptado
df$o <- df$Nota * sin(df$degree * pi / 180) # SOH
df$a <- df$Nota * cos(df$degree * pi / 180) # CAH
df$o100 <- 1 * sin(df$degree * pi / 180) # Outer ring x
df$a100 <- 1 * cos(df$degree * pi / 180) # Outer ring y 

gera_circulo <- function() {
  p = plot_ly()

  var_x <- seq(0, 360 - 360/nodos_verdade, 360/nodos_verdade)
  
  var_x <- c(var_x,0)
  
  db <- data.frame(X = var_x)

  db$o <- 1 * sin(db$X * pi / 180) # SOH
  db$a <- 1 * cos(db$X * pi / 180) # CAH
  
  db$o075 <- 0.75 * sin(db$X * pi / 180) # SOH
  db$a075 <- 0.75 * cos(db$X * pi / 180) # CAH
  
  db$o050 <- 0.50 * sin(db$X * pi / 180) # SOH
  db$a050 <- 0.50 * cos(db$X * pi / 180) # CAH
  
  for(i in 1:nodos_verdade){
    p <- add_trace(
      p,
      x = c(db$o[i], 0),
      y = c(db$a[i], 0),
      evaluate = T,
      mode = "lines",
      line = list(color = "#aaaaaa", dash = "3px", shape = "spline"),
      hoverinfo = "none",
      showlegend = F)
  }
  p %>% 
    add_trace(data = db, x = ~o, y = ~a, 
                  line = list(color = "#aaaaaa", dash = "3px", shape = "spline"),
                  marker = list(color = "#aaaaaa"),
                  hoverinfo = "none",
                  showlegend = FALSE) %>% 
    add_trace(data = db, x = ~o075, y = ~a075, 
              line = list(color = "#aaaaaa", dash = "3px", shape = "spline"),
              marker = list(color = "#aaaaaa"),
              hoverinfo = "none",
              showlegend = FALSE) %>% 
    add_trace(data = db, x = ~o050, y = ~a050, 
              line = list(color = "#aaaaaa", dash = "3px", shape = "spline"),
              marker = list(color = "#aaaaaa"),
              hoverinfo = "none",
              showlegend = FALSE) %>%
	add_trace(data = df, x = ~o100, y = ~a100, 
            text = df$NomeVar,
            hoverinfo = "none",
            textposition = "middle",
            mode = "text",
            textfont = list(color = "#000000"),
            showlegend = FALSE)
}

v1 <- 1:(nodos_verdade*length(input_datas)) # Vetor inicial
v2 <- seq(1, nodos_verdade*length(input_datas), nodos_verdade) # Pega os zeros para fechar o círculo

base_aux_blog <- df[c(v1, v2),]

gera_circulo() %>%
  add_trace(data = base_aux_blog, x = ~o, y = ~a, color = ~date_my, 
            mode = "lines+markers",
            hoverinfo = "text", 
            text = paste0("", base_aux_blog$DescVar_Tooltips, "<br>",
			              "Data: ", base_aux_blog$date_my, "<br>",
			              "Nota: ", round(base_aux_blog$Nota * 10, casas_decimais))) %>%  #base_aux_blog$Year, 
  
  layout(
    #autosize = TRUE,
    hovermode = "closest",     
    #autoscale = TRUE,
    #width = 1000,
    #height = 750,
    xaxis = list(title = "", range = c(-1.25,1.25), showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
    yaxis = list(title = "", range = c(-1.25,1.25), showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
	title = "Radar do Ciclo de Negócios",
	#legend = list(orientation = 'h')
	plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
	paper_bgcolor = "transparent") %>% #papaer_bg serve para descolorir a área da legenda
	         config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
  
  
  
  
  
  })	



  output$plot_radar_unica_ok <- renderPlotly({
    
    input_datas <- input$data_unica_radar # Está no plural porque aproveitei o código das múltiplas datas
    
    input_ano <- lubridate::year(ymd(input_datas))
    input_mes <- lubridate::month(ymd(input_datas))
	
    base_aux_radar_pre <- base_transformada_HP %>%
      #filter(year == input_ano, month %in% input_mes) %>%
      filter(year == input_ano, month == input_mes) %>%
      #select(date_my, Nota, NomeVar) %>%
      select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
      na.omit()
    
    series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))
    
    
    base_aux_radar <- base_aux_radar_pre %>%
      filter(NomeVar %in% series_presentes_em_todos_periodos)
    
    
    df <- base_aux_radar
    
    nodos_verdade <- length(df$NomeVar)/length(input_datas)
    
    taxa_degree <- 360/nodos_verdade
    
    df$degree <- seq(0, 360-taxa_degree, taxa_degree) # Adaptado
    df$o <- df$Nota * sin(df$degree * pi / 180) # SOH
    df$a <- df$Nota * cos(df$degree * pi / 180) # CAH
    df$o100 <- 1 * sin(df$degree * pi / 180) # Outer ring x
    df$a100 <- 1 * cos(df$degree * pi / 180) # Outer ring y 
    
    gera_circulo <- function() {
      p = plot_ly()
      
      var_x <- seq(0, 360 - 360/nodos_verdade, 360/nodos_verdade)
      
      var_x <- c(var_x,0)
      
      db <- data.frame(X = var_x)
      
      db$o <- 1 * sin(db$X * pi / 180) # SOH
      db$a <- 1 * cos(db$X * pi / 180) # CAH
      
      db$o075 <- 0.75 * sin(db$X * pi / 180) # SOH
      db$a075 <- 0.75 * cos(db$X * pi / 180) # CAH
      
      db$o050 <- 0.50 * sin(db$X * pi / 180) # SOH
      db$a050 <- 0.50 * cos(db$X * pi / 180) # CAH
      
      for(i in 1:nodos_verdade){
        p <- add_trace(
          p,
          x = c(db$o[i], 0),
          y = c(db$a[i], 0),
          evaluate = T,
          mode = "lines",
          line = list(color = "#aaaaaa", dash = "3px", shape = "spline"),
          hoverinfo = "none",
          showlegend = F)
      }
      p %>% 
        add_trace(data = db, x = ~o, y = ~a, 
                  line = list(color = "#aaaaaa", dash = "3px", shape = "spline"),
                  marker = list(color = "#aaaaaa"),
                  hoverinfo = "none",
                  showlegend = FALSE) %>% 
        add_trace(data = db, x = ~o075, y = ~a075, 
                  line = list(color = "#aaaaaa", dash = "3px", shape = "spline"),
                  marker = list(color = "#aaaaaa"),
                  hoverinfo = "none",
                  showlegend = FALSE) %>% 
        add_trace(data = db, x = ~o050, y = ~a050, 
                  line = list(color = "#aaaaaa", dash = "3px", shape = "spline"),
                  marker = list(color = "#aaaaaa"),
                  hoverinfo = "none",
                  showlegend = FALSE) %>%
        add_trace(data = df, x = ~o100, y = ~a100, 
                  text = df$NomeVar,
                  hoverinfo = "none",
                  textposition = "middle",
                  mode = "text",
                  textfont = list(color = "#000000"),
                  showlegend = FALSE)
    }
    
    v1 <- 1:(nodos_verdade*length(input_datas)) # Vetor inicial
    v2 <- seq(1, nodos_verdade*length(input_datas), nodos_verdade) # Pega os zeros para fechar o círculo
    
    base_aux_blog <- df[c(v1, v2),]
    
    gera_circulo() %>%
      add_trace(data = base_aux_blog, x = ~o, y = ~a, color = ~date_my, 
                mode = "lines+markers",
                hoverinfo = "text", 
                text = paste0("", base_aux_blog$DescVar_Tooltips, "<br>",
                              "Data: ", base_aux_blog$date_my, "<br>",
                              "Nota: ", round(base_aux_blog$Nota * 10, casas_decimais))) %>%  #base_aux_blog$Year, 
      
      layout(
        #autosize = TRUE,
        hovermode = "closest",     
        #autoscale = TRUE,
        #width = 1000,
        #height = 750,
        xaxis = list(title = "", range = c(-1.25,1.25), showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
        yaxis = list(title = "", range = c(-1.25,1.25), showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
        title = paste0("Radar do Ciclo de Negócios em ", retorna_mes(input_mes), " de ", input_ano),
        legend = list(orientation = 'h'),
		showlegend = FALSE,
		plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
		paper_bgcolor = "transparent") %>% #papaer_bg serve para descolorir a área da legenda
      
      config(
        #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
        #displaylogo = FALSE, # o displaylogo é o logo do Plotly
        modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
    
  })  


##########################
# GAUGES MÚLTIPLAS DATAS #
##########################

output$gauge_1 = renderGauge({

input_datas <- input$datas_radar

base_aux_radar_pre <- base_transformada_SA %>%
  filter(date_my %in% input_datas) %>%
  select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
  na.omit()

series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))

base_aux_gauges <- base_aux_radar_pre %>%
                   filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
				   group_by(Categoria) %>%
                   summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
				   mutate(Categoria = stri_conv(as.character(Categoria), "latin1", "UTF-8"))

# Categorias
nota <- round(base_aux_gauges$Nota_Media[1], casas_decimais)
nome <- base_aux_gauges$Categoria[1]

gauge(nota, min = 0, max = 10, 
      gaugeSectors(success = c(8, 10), 
	               warning = c(4, 8), 
				   danger = c(0, 4)),
      label = nome)

})  


output$gauge_2 = renderGauge({

input_datas <- input$datas_radar

base_aux_radar_pre <- base_transformada_SA %>%
  filter(date_my %in% input_datas) %>%
  select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
  na.omit()

series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))

base_aux_gauges <- base_aux_radar_pre %>%
                   filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
				   group_by(Categoria) %>%
                   summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
				   mutate(Categoria = stri_conv(as.character(Categoria), "latin1", "UTF-8"))

# Categorias
nota <- round(base_aux_gauges$Nota_Media[2], casas_decimais)
nome <- base_aux_gauges$Categoria[2]

gauge(nota, min = 0, max = 10, 
      gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
      label = nome)

})  
  

output$gauge_3 = renderGauge({

input_datas <- input$datas_radar

base_aux_radar_pre <- base_transformada_SA %>%
  filter(date_my %in% input_datas) %>%
  select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
  na.omit()

series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))

base_aux_gauges <- base_aux_radar_pre %>%
                   filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
				   group_by(Categoria) %>%
                   summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
				   mutate(Categoria = stri_conv(as.character(Categoria), "latin1", "UTF-8"))

# Categorias
nota <- round(base_aux_gauges$Nota_Media[3], 2)
nome <- base_aux_gauges$Categoria[3]

gauge(nota, min = 0, max = 10, 
      gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
      label = nome)

})  
  

output$gauge_4 = renderGauge({

input_datas <- input$datas_radar

base_aux_radar_pre <- base_transformada_SA %>%
  filter(date_my %in% input_datas) %>%
  select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
  na.omit()

series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))

base_aux_gauges <- base_aux_radar_pre %>%
                   filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
				   group_by(Categoria) %>%
                   summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
				   mutate(Categoria = stri_conv(as.character(Categoria), "latin1", "UTF-8"))

# Categorias
nota <- round(base_aux_gauges$Nota_Media[4], casas_decimais)
nome <- base_aux_gauges$Categoria[4]

gauge(nota, min = 0, max = 10, 
      gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
      label = nome)

})
  




 
  
  

#####################
# GAUGES DATA ÚNICA #
#####################

output$gauge_unica_1 = renderGauge({
  
  input_datas <- input$data_unica_radar
  
  input_ano <- lubridate::year(ymd(input_datas))
  input_mes <- lubridate::month(ymd(input_datas))
  
  base_aux_radar_pre <- base_transformada_SA %>%
    filter(year == input_ano, month == input_mes) %>%
    select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
    na.omit()
  
  series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))
  
  base_aux_gauges <- base_aux_radar_pre %>%
    filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
    group_by(Categoria) %>%
    summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
    mutate(Categoria = stri_conv(as.character(Categoria), "latin1", "UTF-8"))
  
  
  # Categorias
  #nota <- round(base_aux_gauges$Nota_Media[1], casas_decimais)
  #nome <- "Arrecadação"
  
  nota <- round(base_aux_gauges$Nota_Media[1], casas_decimais)
  nome <- base_aux_gauges$Categoria[1]
  
  gauge(nota, min = 0, max = 10, 
        gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
        label = nome)

  
})  


output$gauge_unica_2 = renderGauge({
  
  input_datas <- input$data_unica_radar
  
  input_ano <- lubridate::year(ymd(input_datas))
  input_mes <- lubridate::month(ymd(input_datas))
  
  base_aux_radar_pre <- base_transformada_SA %>%
    filter(year == input_ano, month == input_mes) %>%
    select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
    na.omit()
  
  series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))
  
  base_aux_gauges <- base_aux_radar_pre %>%
    filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
    group_by(Categoria) %>%
    summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
    mutate(Categoria = stri_conv(as.character(Categoria), "latin1", "UTF-8"))
  
  # Categorias
  #nota <- round(base_aux_gauges$Nota_Media[2], casas_decimais)
  #nome <- "Consumo"
  
  nota <- round(base_aux_gauges$Nota_Media[2], casas_decimais)
  nome <- base_aux_gauges$Categoria[2]
  
  gauge(nota, min = 0, max = 10, 
        gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
        label = nome)
  
})


output$gauge_unica_3 = renderGauge({
  
  input_datas <- input$data_unica_radar
  
  input_ano <- lubridate::year(ymd(input_datas))
  input_mes <- lubridate::month(ymd(input_datas))
  
  base_aux_radar_pre <- base_transformada_SA %>%
    filter(year == input_ano, month == input_mes) %>%
    select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
    na.omit()
  
  series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))
  
  base_aux_gauges <- base_aux_radar_pre %>%
    filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
    group_by(Categoria) %>%
    summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
    mutate(Categoria = stri_conv(as.character(Categoria), "latin1", "UTF-8"))
  
  # Categorias
  #nota <- round(base_aux_gauges$Nota_Media[3], casas_decimais)
  #nome <- "Expectativas"
  
  nota <- round(base_aux_gauges$Nota_Media[3], casas_decimais)
  nome <- base_aux_gauges$Categoria[3]
  
  gauge(nota, min = 0, max = 10, 
        gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
        label = nome)
  
})


output$gauge_unica_4 = renderGauge({
  
  input_datas <- input$data_unica_radar
  
  input_ano <- lubridate::year(ymd(input_datas))
  input_mes <- lubridate::month(ymd(input_datas))
  
  base_aux_radar_pre <- base_transformada_SA %>%
    filter(year == input_ano, month == input_mes) %>%
    select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria) %>%
    na.omit()
  
  series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))
  
  base_aux_gauges <- base_aux_radar_pre %>%
    filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
    group_by(Categoria) %>%
    summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
    mutate(Categoria = stri_conv(as.character(Categoria), "latin1", "UTF-8"))
  
  # Categorias
  #nota <- round(base_aux_gauges$Nota_Media[4], casas_decimais)
  #nome <- "Mercado Externo"
  
  nota <- round(base_aux_gauges$Nota_Media[4], casas_decimais)
  nome <- base_aux_gauges$Categoria[4]
  
  gauge(nota, min = 0, max = 10, 
        gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
        label = nome)
  
})






  
  
##############################################
# GAUGES MÚLTIPLAS DATAS - CLASSIFICAÇÃO PIB #
##############################################

output$gauge_1_PIB = renderGauge({

input_datas <- input$datas_radar

base_aux_radar_pre <- base_transformada %>%
  filter(date_my %in% input_datas) %>%
  select(date_my, Nota, NomeVar, DescVar_Tooltips, CategoriaPIB) %>%
  na.omit()

series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))

base_aux_gauges <- base_aux_radar_pre %>%
                   filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
				   group_by(CategoriaPIB) %>%
                   summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
				   mutate(CategoriaPIB = stri_conv(as.character(CategoriaPIB), "latin1", "UTF-8"))

# Categorias
nota <- round(base_aux_gauges$Nota_Media[1], casas_decimais)
nome <- base_aux_gauges$CategoriaPIB[1]

gauge(nota, min = 0, max = 10, 
      gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
      label = nome)

}) 


output$gauge_2_PIB = renderGauge({

input_datas <- input$datas_radar

base_aux_radar_pre <- base_transformada %>%
  filter(date_my %in% input_datas) %>%
  select(date_my, Nota, NomeVar, DescVar_Tooltips, CategoriaPIB) %>%
  na.omit()

series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))

base_aux_gauges <- base_aux_radar_pre %>%
                   filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
				   group_by(CategoriaPIB) %>%
                   summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
				   mutate(CategoriaPIB = stri_conv(as.character(CategoriaPIB), "latin1", "UTF-8"))

# Categorias
nota <- round(base_aux_gauges$Nota_Media[2], 2)
nome <- base_aux_gauges$CategoriaPIB[2]

gauge(nota, min = 0, max = 10, 
      gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
      label = nome)

})


output$gauge_3_PIB = renderGauge({

input_datas <- input$datas_radar

base_aux_radar_pre <- base_transformada %>%
  filter(date_my %in% input_datas) %>%
  select(date_my, Nota, NomeVar, DescVar_Tooltips, CategoriaPIB) %>%
  na.omit()

series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))

base_aux_gauges <- base_aux_radar_pre %>%
                   filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
				   group_by(CategoriaPIB) %>%
                   summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
				   mutate(CategoriaPIB = stri_conv(as.character(CategoriaPIB), "latin1", "UTF-8"))

# Categorias
nota <- round(base_aux_gauges$Nota_Media[3], casas_decimais)
nome <- base_aux_gauges$CategoriaPIB[3]

gauge(nota, min = 0, max = 10, 
      gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
      label = nome)

})


#########################################
# GAUGES DATA ÚNICA - CLASSIFICAÇÃO PIB #
#########################################

output$gauge_unica_1_PIB = renderGauge({
  
  input_datas <- input$data_unica_radar
  
  input_ano <- lubridate::year(ymd(input_datas))
  input_mes <- lubridate::month(ymd(input_datas))
  
  base_aux_radar_pre <- base_transformada %>%
    filter(year == input_ano, month == input_mes) %>%
    select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria, CategoriaPIB) %>%
    na.omit()
  
  series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))
  
  base_aux_gauges <- base_aux_radar_pre %>%
    filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
    group_by(CategoriaPIB) %>%
    summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
    mutate(CategoriaPIB = stri_conv(as.character(CategoriaPIB), "latin1", "UTF-8"))
  
  
  # Categorias
  #nota <- round(base_aux_gauges$Nota_Media[1], casas_decimais)
  #nome <- "Arrecadação"
  
  nota <- round(base_aux_gauges$Nota_Media[1], casas_decimais)
  nome <- base_aux_gauges$CategoriaPIB[1]
  
  gauge(nota, min = 0, max = 10, 
        gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
        label = nome)
  
})


output$gauge_unica_2_PIB = renderGauge({
  
  input_datas <- input$data_unica_radar
  
  input_ano <- lubridate::year(ymd(input_datas))
  input_mes <- lubridate::month(ymd(input_datas))
  
  base_aux_radar_pre <- base_transformada %>%
    filter(year == input_ano, month == input_mes) %>%
    select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria, CategoriaPIB) %>%
    na.omit()
  
  series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))
  
  base_aux_gauges <- base_aux_radar_pre %>%
    filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
    group_by(CategoriaPIB) %>%
    summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
    mutate(CategoriaPIB = stri_conv(as.character(CategoriaPIB), "latin1", "UTF-8"))
  
  
  # Categorias
  #nota <- round(base_aux_gauges$Nota_Media[1], casas_decimais)
  #nome <- "Arrecadação"
  
  nota <- round(base_aux_gauges$Nota_Media[2], casas_decimais)
  nome <- base_aux_gauges$CategoriaPIB[2]
  
  gauge(nota, min = 0, max = 10, 
        gaugeSectors(success = c(8, 10), warning = c(4, 8), danger = c(0, 4)),
        label = nome)
  
})


output$gauge_unica_3_PIB = renderGauge({
  
  input_datas <- input$data_unica_radar
  
  input_ano <- lubridate::year(ymd(input_datas))
  input_mes <- lubridate::month(ymd(input_datas))
  
  base_aux_radar_pre <- base_transformada %>%
    filter(year == input_ano, month == input_mes) %>%
    select(date_my, Nota, NomeVar, DescVar_Tooltips, Categoria, CategoriaPIB) %>%
    na.omit()
  
  series_presentes_em_todos_periodos <- names(which(table(base_aux_radar_pre$NomeVar) == length(input_datas)))
  
  base_aux_gauges <- base_aux_radar_pre %>%
    filter(NomeVar %in% series_presentes_em_todos_periodos) %>%
    group_by(CategoriaPIB) %>%
    summarize(Nota_Media = mean(Nota, na.rm = T) * 10) %>%
    mutate(CategoriaPIB = stri_conv(as.character(CategoriaPIB), "latin1", "UTF-8"))
  
  
  # Categorias
  #nota <- round(base_aux_gauges$Nota_Media[1], casas_decimais)
  #nome <- "Arrecadação"
  
  nota <- round(base_aux_gauges$Nota_Media[3], casas_decimais)
  nome <- base_aux_gauges$CategoriaPIB[3]
  
  gauge(nota, min = 0, max = 10, 
        gaugeSectors(success = c(8, 10), 
		             warning = c(4, 8), 
		             danger = c(0, 4)),
        label = nome)
  
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
	
	paste0("Tabela Sintética de Variações em ", retorna_mes(mes_data_tabela), " de ", ano_data_tabela)
	
	})
	
	output$tabela_brazuca <- renderFormattable ({
	
    series_contra_ci <- c("s_1005", "s_1006", "s_1016")
	
	casas_decimais <- 1
	
	data_analise <- input$data_jogo
	
	input_ano <- lubridate::year(ymd(data_analise))
	input_mes <- lubridate::month(ymd(data_analise))
	input_grupo <- input$grupo_tabela
	
	if(!input$check_grupo_tabela) {
	tabela_brasileirao <- tabela_brasileirao %>%
							filter(year == input_ano, month == input_mes) %>%
							select(NomeVarTabela, Indice, Cresci_Mensal_12M, Cresci_Acum_12M, Cresci_Mensal_1M, Cresci_Acum_3M_JM, Serie)}
	
	if(input$check_grupo_tabela) {
	tabela_brasileirao <- tabela_brasileirao %>%
							filter(year == input_ano, month == input_mes, Categoria == input_grupo) %>%
							select(NomeVarTabela, Indice, Cresci_Mensal_12M, Cresci_Acum_12M, Cresci_Mensal_1M, Cresci_Acum_3M_JM, Serie)}

	fixedWidth = 225

	colnames(tabela_brasileirao) <- c("Nome da Série", "Valor Bruto", "Mensal 12 Meses*", "Acum. 12 Meses*", "Mês Contra Mês Anterior**", "Média Móvel 3 Meses**", "Cod Série")
	
	formattable(tabela_brasileirao, 
		align = c("l", "c", "r", "r", "r", "r"),
		list(

		"Nome da Série" = formatter(.tag = "span", style = function(x) style(
																    display = "inline-block",
																	"text-align" = "left",
    	   	                                                        #direction = "rtl", `border-radius` = "4px", 
                                                                  	#`padding-right` = "2px", 
                                                                  	`background-color` = "transparent",
                                                                    width = paste0(fixedWidth, "px", sep = ""))),

		#ifelse(tabela_brasileirao$"Nome da Série" == "Agronegócio (pessoas ocupadas)", 0, 1)

		"Valor Bruto" = formatter("span",
                         #formatC(tabela_brasileirao$"Valor Bruto", digits = 1, big.mark = ".", format = "f", decimal.mark = ",")
						 ifelse(tabela_brasileirao$"Nome da Série" == "Agronegócio (pessoas ocupadas)", 
						 formatC(tabela_brasileirao$"Valor Bruto", digits = 0, big.mark = ".", format = "f", decimal.mark = ","), 
						 formatC(tabela_brasileirao$"Valor Bruto", digits = 1, big.mark = ".", format = "f", decimal.mark = ","))
						 ),

		"Mensal 12 Meses*" = formatter("span",
                         style = x ~ style(color = ifelse(tabela_brasileirao$"Mensal 12 Meses*" > 0, ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "green", "red"))),
                         format(round(tabela_brasileirao$"Mensal 12 Meses*", casas_decimais), format = "f", decimal.mark = ","),
                         x ~ icontext(ifelse(tabela_brasileirao$"Mensal 12 Meses*" > 0, "arrow-up", "arrow-down"))),
		"Acum. 12 Meses*" = formatter("span",
                                style = x ~ style(color = ifelse(tabela_brasileirao$"Acum. 12 Meses*" > 0, ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "green", "red"))),
                                format(round(tabela_brasileirao$"Acum. 12 Meses*", casas_decimais), format = "f", decimal.mark = ","),
                                x ~ icontext(ifelse(tabela_brasileirao$"Acum. 12 Meses*" > 0, "arrow-up", "arrow-down"))),
		"Mês Contra Mês Anterior**" = formatter("span",
                                style = x ~ style(color = ifelse(tabela_brasileirao$"Mês Contra Mês Anterior**" > 0, ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "green", "red"))),
                                format(round(tabela_brasileirao$"Mês Contra Mês Anterior**", casas_decimais), format = "f", decimal.mark = ","),
                                x ~ icontext(ifelse(tabela_brasileirao$"Mês Contra Mês Anterior**" > 0, "arrow-up", "arrow-down"))),
		"Média Móvel 3 Meses**" = formatter("span",
                                style = x ~ style(color = ifelse(tabela_brasileirao$"Média Móvel 3 Meses**" > 0, ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "green", "red"))),
                                format(round(tabela_brasileirao$"Média Móvel 3 Meses**", casas_decimais), format = "f", decimal.mark = ","),
                                x ~ icontext(ifelse(tabela_brasileirao$"Média Móvel 3 Meses**" > 0, "arrow-up", "arrow-down"))),
								
		"Cod Série" = FALSE
))
	
	})
	
	
	
	
	output$tabela_download_sem_ajuste = renderDataTable({
    datatable(base_transformada_download, options(list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
      pageLength = 15)))
    })
  
  
    datasetInput_sem <- reactive({
    base_transformada_download
    })
  
  output$downloadData_sem <- downloadHandler(
    filename = function() { paste('base_sem_ajuste', '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput_sem(), file)
    }
  )
  
  
  
  	
	output$tabela_download_com_ajuste = renderDataTable({
    datatable(base_transformada_SA_download, options(list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
      pageLength = 15)))
    })
  
  
    datasetInput_com <- reactive({
    base_transformada_SA_download
    })
  
  output$downloadData_com <- downloadHandler(
    filename = function() { paste('base_sem_ajuste', '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput_com(), file)
    }
  )

  output$collapsible_meg <- renderCollapsibleTree({

  	tradutor_aux <- tradutor %>%
  					filter(CodVar %in% unique(base_transformada$Serie))
  	
  	collapsibleTree(tradutor_aux,
  		hierarchy = c("Categoria", "NomeVar"),
  		root = "MEG",
  		fontSize = 15)
  	})









  
})