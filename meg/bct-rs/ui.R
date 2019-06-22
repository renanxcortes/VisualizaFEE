# Lista de Ícones aqui: http://fontawesome.io/icons/
# Exemplos de Box: https://github.com/rstudio/shinydashboard/issues/219 e https://github.com/rstudio/shinydashboard/issues/164

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
library(shinythemes)
library(flexdashboard)
library(zoo)
library(shinyBS)
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
tradutor_eng$DescVar_Tooltips <- stri_conv(as.character(tradutor_eng$DescVar_Tooltips), "latin1", "UTF-8")
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
transforma_dia_para_mes_ano <- function(x) {paste0(retorna_mes(as.numeric(str_sub(x, 6, 7))), "-", str_sub(x,1,4)) } # Pega uma string "yyyy-mm-dd" e vira "NomeMês-Ano"

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
  
 
 
# Cria uma base auxiliar só para pegar as datas que aparecem NA em todos (para os valores Default dos input ficarem recentes)
aux_datas <- base_transformada %>%
             select(date_my, Data, Acelera_Mensal_12M, Cresci_Mensal_12M, Indice, Nota) %>%
             gather(Tipo_Transformacao, Valor, Acelera_Mensal_12M, Cresci_Mensal_12M, Indice, Nota) %>%
             na.omit() %>%
			 arrange(Data)

aux_datas_pib <- na.omit(base_pib) 


# Definindo o UI
shinyUI(fluidPage(includeCSS("estilociclo.css"), htmlOutput("frame"), theme = shinytheme("cerulean"), tags$head(tags$script(src="tracking.js")), tags$head(tags$link(rel="shortcut icon", href="feeicon.ico")),
        tags$div(
                HTML("<script>
                     (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                     (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                     m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                     })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

                     ga('create', 'UA-1506603-5', 'auto');
                     ga('send', 'pageview');
                     </script>")),
					 
		        tags$head(HTML('<meta property="og:type" content="article">')),
				tags$head(HTML('<meta property="og:title" content="MEG - Monitor da Economia Gaúcha">')),
				tags$head(HTML('<meta property="og:url" content="http://visualiza.fee.tche.br/meg/">')),
				tags$head(HTML('<meta property="og:image" content="http://visualiza.fee.tche.br/_imgs/meg.jpg">')),
				tags$head(HTML('<meta property="og:description" content="O MEG apresenta, de maneira interativa e dinâmica, os principais dados conjunturais da economia do Rio Grande do Sul (RS). " />')),
				tags$head(HTML('<meta property="og:site_name" content="VisualizaFEE" />')),


				 
# Essa parte de baixo dá uma "empurradinha" pra direita o navbarheader pra aparecer o nome do app: https://stackoverflow.com/questions/9792849/how-to-insert-spaces-tabs-in-text-using-html-css
tags$style(type = 'text/css',

                           '.navbar-default .navbar-brand {
							 padding-left: 35px;
                           }'

                           ),


					 
tags$style(type="text/css", # isso é para não mostrar nenhuma mensagem vermelha de erro!!
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
	),  
  # Application title
  #titlePanel(paste0("- ",  "CrimeVis: Visualização da Criminalidade Anual no Rio Grande do Sul")),

 
  navbarPage("BCT-RS", # Navegação
             tabPanel("Presentation", #Apresentação
                      
                      sidebarLayout(
                        sidebarPanel(
                          h2("Economics and Statistics Foundation"),
                          p("The Siegfried Emanuel Heuser Economics and Statistics Foundation (FEE) is a research institution, linked to the Secretariat of Planning, Mobility and Regional Development of the Government of the State of Rio Grande do Sul."),
                          br(),
                          br(),
                          img(src = "fee_logo.png", height = 32, width = 132), 
                          img(src = "logoGoverno.png", height = 62, width = 92),
                          br(),
                          br(),
                          br(),
                          "Shiny is a product from ", 
                          span("RStudio", style = "color:blue"),".", 
                          br(), 
                          br(),
                          img(src = "bigorb.png", height = 82, width = 92),
						  br(),
						  br(),
						  div(h2("Tutorial Video (portuguese)"), align = "center"),
						  div(HTML('<iframe height="315" src="https://www.youtube.com/embed/LKABa4rKo1c" frameborder="0" allowfullscreen></iframe>'), align = "center") # width="450"
                        ),
                        mainPanel(
                          h1("Business Cycle Tracer of Rio Grande do Sul"),
                          p("The ", strong("Business Cycle Tracer of Rio Grande do Sul"),"(BCT-RS) is a product of Economics and Statistics Foundation which presents, 
                            in an interactive and dynamic way, main macroeconomic data of Rio Grande do Sul (RS) state. The indicators have origins in multiple data sources and cover several dimensions, such as Economic Activity, Labor Market, Sentiment Indicators, Credit and Delinquency Rate and Price Indices."),			  
                          br(),
						  #a("Metodologia do BCT-RS", 
                          #  href = "https://www.researchgate.net/profile/Jefferson_Colombo/publication/320560245_Metodologia_de_Construcao_do_Monitor_da_Economia_Gaucha_-_BCT-RS/links/59eddf324585158fe5340ff7/Metodologia-de-Construcao-do-Monitor-da-Economia-Gaucha-BCT-RS.pdf?_iepl%5BhomeFeedViewId%5D=b23P3A1temN6Rq30KjLtRoJc&_iepl%5Bcontexts%5D%5B0%5D=pcfhf&_iepl%5BinteractionType%5D=publicationDownload&origin=publication_detail&ev=pub_int_prw_xdl&msrp=d9-sApPto5sI-C-FC4gwmSY0QT2j4fcLfjPXZdcxZ1WS6Bnqc_eQR1Kn0sPLM6X26QQBBgoQtJVwdMUc82-332qNMGDBnmmKFPsQpIIH_JJRUOZ63cqV8dL5.JcwJ-4KY2kpGLptG-P2Oj3y4WQDi9qOeiZFlhozkQJ4Gnk4Z4TjSYIuNqKQhLeIPuuT2dlrUQyReBB4F9XZ7YicGLwae9L_UKl8YEw.phbME2jATBr0r6jGxoj2azE7R-imJCsl8Iinl5T8h7TDX4vmEb7tw5pQn2I8a2ZoFyXyDupBqDSMTSvMokmuSf24NrgAzGMj5u24jw.5_vffD7dGy30d07gAGQ0By8GsvTUpnwwez5AqgTCROVrxxI9QrD9QjVNssgp7b-_65vu_FQIAVZ31BI6tImucJW_4SoyqLCQu3br4Q",
                          #  target="_blank"),
						  div(downloadButton("downloadMetodologia", "BCT-RS Methodology"), align = "center"),
						  br(),
						  br(),
						  #fluidRow(column(6, h2("Características do aplicativo"), p("* Visualize as séries temporais dos indicadores deste sistema de acordo com a sua dimensão na economia."), p("* Visualize as séries temporais dos indicadores deste sistema de acordo com a sua dimensão na economia."),
                          #p("* Compare diferentes tipos de variação em uma tabela sintética com os indicadores."),
                          #p("* Analise a dinâmica macroeconômica das variáveis do sistema, através do comportamento temporal de indicadores categorizados como antecedentes, coincidentes ou defasados."),
						  #p("* Faça o download de todas as bases de dados utilizadas.")),
						  #         column(6, h2("Vídeo Tutorial"), HTML('<iframe width="400" height="315" src="https://www.youtube.com/embed/LKABa4rKo1c" frameborder="0" allowfullscreen></iframe>'))),
                          h2("App Features"),
                          p("* Visualize the indicators time series of this system according to their dimension in the economy."),
                          p("* Compare different types of variation on a table with the indicators."),
                          p("* Analyze the macroeconomic dynamics of the system variables, through the temporal behavior of indicators categorized as leading, coincident or lagging."),
						  p("* Download all dataset used."),
						  br(),
						  div(h2("BCT-RS Dimensions and Indicators"), align = "center"),
                          div(collapsibleTreeOutput("collapsible_bct"), align = "center"),
						  #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/LKABa4rKo1c" frameborder="0" allowfullscreen></iframe>'),

                          br(),
                          h3("Contact for questions, suggestions or requests:"),
						  p("Jéfferson Augusto Colombo, Renan Xavier Cortes, Fernando Cruz or Luis Henrique Zanandréa Paese ",
                            a("(CONTACT)", 
                              href = "http://www.fee.rs.gov.br/contato/", target="_blank"))
                          #p("Renan Xavier Cortes (renan@fee.tche.br)")
                          )
                      )),
					  
					  
					  
#            tabPanel("Monitor",
#                      
#               sidebarLayout(
#               sidebarPanel(
#			   
#			   sliderInput("data_slider",
#                        "Escolha o mês e ano de Análise:",
#                        #min = as.Date("2006-07-01","%Y-%m-%d"),
#                        #max = as.Date("2016-07-01","%Y-%m-%d"),
#						min = min(aux_datas$Data),
#						max = max(aux_datas$Data),
#                        value = max(aux_datas$Data),timeFormat="%m-%Y",
#						#width = '1750px',
#						step = 31, # Número de dias em cada variação do slider
#						animate = animationOptions(interval = 1250, loop = TRUE))
#                          
#                        ),
#                        
#                        # Show the plots
#                        mainPanel(
#					
#						  
#						tabsetPanel(type = "tabs",
#                       tabPanel("Destaque na Região", br(), div(plotlyOutput("plot_monitor", height = "700px", width = "950px"), align = "center")),
#						tabPanel("Destaque nos Indicadores", br(), div(plotlyOutput("plot_monitor_cores_no_scatter", height = "700px", width = "950px"), align = "center"))
#                        )
#						)
#                      ))
				
# Começa o monitor que funciona				
#		       tabPanel("Monitor",
#                      
#                        # Show the plots
#			
#						tabsetPanel(type = "tabs",
#                        tabPanel("Destaque na Região", br(), div(plotlyOutput("plot_monitor", height = "600px", width = "950px"), align = "center")), # Dentro do plotlyOutput:, height = "700px", width = "950px"
#						tabPanel("Destaque nos Indicadores", br(), div(plotlyOutput("plot_monitor_cores_no_scatter", height = "600px", width = "850px"), align = "center")) # , height = "700px", width = "950px"
#                        
#						
#						),
#						br(), br(),
#						fluidPage(
#						column(12,
#					    div(sliderInput("data_slider_monitor",
#                       "Escolha o mês e ano de Análise:",
#						min = min(aux_datas$Data),
#						max = max(aux_datas$Data),
#                        value = max(aux_datas$Data),timeFormat="%m-%Y",
#						width = "50%",
#						step = 31, # Número de dias em cada variação do slider
#						animate = animationOptions(interval = 1250, loop = TRUE)), align = "center")
#						
#                     ))						
#                      )
# Termina o monitor que funciona	




					  
					  
					  
					  #,
					  
					  
					  

					  
			
			
#			tabPanel("Radares",
#			tabsetPanel(type = 'tabs',
#			
##            tabPanel("Data Única",
##            br(),
##            fluidRow(
##						column(3, gaugeOutput("gauge_unica_1"), gaugeOutput("gauge_unica_2"), gaugeOutput("gauge_unica_3")),
##						column(6, plotlyOutput("plot_radar_unica_ok", height = "650px")),
##						column(3, gaugeOutput("gauge_unica_4"), gaugeOutput("gauge_unica_5"), gaugeOutput("gauge_unica_6"))
##						  
##					), hr(),
##			fluidPage(
##            column(12,			
##			div(sliderInput("data_unica_radar",
##                          "Escolha o mês e ano de Análise:",
##                          #min = as.Date("2006-07-01","%Y-%m-%d"),
##                          #max = as.Date("2016-07-01","%Y-%m-%d"),
##                          min = min(aux_datas$Data),
##                          max = max(aux_datas$Data),
##                          value = as.Date("2017-04-01"), #max(aux_datas$Data),
##						  timeFormat = "%m-%Y",
##                          width = "50%",
##                          step = 31, # Número de dias em cada variação do slider
##                          animate = animationOptions(interval = 1250, loop = TRUE)), align = "center")
##						  ))
##					 ),
#
#
#            tabPanel("Data Única",
#		    
#                      
#            sidebarLayout(
#            sidebarPanel(width = 3, # Numero de colunas
#
#				radioButtons("tipo_gauge_data_unica", "Classificação das Notas:",
#                                       c("Por Setor na Economia" = "radio_setor_data_unica",
#                                         "Por Defasagem na Economia" = "radio_classificacaoPIB_data_unica"))
#                          
#                        ),
#                        
#                        # Maneira 1
#                        #mainPanel(
#                        #div(plotlyOutput("plot_radar_ok", height = "auto", width = "1000px"), align = "center"),
#						#hr(),
#						#fluidRow(column(4,gaugeOutput("gauge_1")), column(4,gaugeOutput("gauge_2")), column(4,gaugeOutput("gauge_3"))),
#						#fluidRow(column(4,gaugeOutput("gauge_4")), column(4,gaugeOutput("gauge_5")), column(4,gaugeOutput("gauge_6")))
#                        #)
#						
#						# Maneira 2
#						mainPanel(width = 9, # Complementar do width do sidebar
#						conditionalPanel(condition = ("input.tipo_gauge_data_unica == 'radio_setor_data_unica'"), column(3, gaugeOutput("gauge_unica_1"), gaugeOutput("gauge_unica_2"))), #, gaugeOutput("gauge_unica_3") 
#						br(),
#						column(6, plotlyOutput("plot_radar_unica_ok", width = "500px", height = "500px")), 
#						conditionalPanel(condition = ("input.tipo_gauge_data_unica == 'radio_setor_data_unica'"), column(3, gaugeOutput("gauge_unica_3"), gaugeOutput("gauge_unica_4"))), #, gaugeOutput("gauge_unica_6")
#						conditionalPanel(condition = ("input.tipo_gauge_data_unica != 'radio_setor_data_unica'"), column(6, gaugeOutput("gauge_unica_1_PIB"), gaugeOutput("gauge_unica_2_PIB"), gaugeOutput("gauge_unica_3_PIB"))),
#						br(),
#						column(12,
#						div(sliderInput("data_unica_radar",
#                          "Escolha o mês e ano de Análise:",
#                          #min = as.Date("2006-07-01","%Y-%m-%d"),
#                          #max = as.Date("2016-07-01","%Y-%m-%d"),
#                          min = min(aux_datas$Data),
#                          max = max(aux_datas$Data),
#                          value = as.Date("2017-03-01"), #max(aux_datas$Data),
#						  timeFormat = "%m-%Y",
#                          width = "75%",
#                          step = 31, # Número de dias em cada variação do slider
#                          animate = animationOptions(interval = 1250, loop = TRUE)), align = "center"))
#                        )
#						
#						
#                      )),
#
#
#
#            tabPanel("Múltiplas Datas",
#		    
#                      
#            sidebarLayout(
#            sidebarPanel(width = 3, # Numero de colunas
#                          
#			   selectizeInput('datas_radar', 'Escolha abaixo as datas para comparação no radar:', 
#                            as.character(unique(arrange(aux_datas, desc(Data))$date_my)), # Ordena do mais recente para o menos recente
#							selected = "Março-2017", #transforma_dia_para_mes_ano(as.character(max(aux_datas$Data))),
#                            options = list(maxItems = length(as.character(unique(aux_datas$date_my))),
#                                           placeholder = 'Selecione uma lista de datas...')),
#										   
#				radioButtons("tipo_gauge_multiplas_datas", "Classificação das Notas:",
#                                       c("Por Setor na Economia" = "radio_setor_multiplas_datas",
#                                         "Por Defasagem na Economia" = "radio_classificacaoPIB_multiplas_datas"))
#                          
#                        ),
#                        
#                        # Maneira 1
#                        #mainPanel(
#                        #div(plotlyOutput("plot_radar_ok", height = "auto", width = "1000px"), align = "center"),
#						#hr(),
#						#fluidRow(column(4,gaugeOutput("gauge_1")), column(4,gaugeOutput("gauge_2")), column(4,gaugeOutput("gauge_3"))),
#						#fluidRow(column(4,gaugeOutput("gauge_4")), column(4,gaugeOutput("gauge_5")), column(4,gaugeOutput("gauge_6")))
#                        #)
#						
#						# Maneira 2
#						mainPanel(width = 9, # Complementar do width do sidebar
#						conditionalPanel(condition = ("input.tipo_gauge_multiplas_datas == 'radio_setor_multiplas_datas'"), column(2, gaugeOutput("gauge_1"), gaugeOutput("gauge_2"))), #, gaugeOutput("gauge_3")
#						br(),
#						column(8, plotlyOutput("plot_radar_ok", height = "500px", width = "600px")),
#						conditionalPanel(condition = ("input.tipo_gauge_multiplas_datas == 'radio_setor_multiplas_datas'"), column(2, gaugeOutput("gauge_3"), gaugeOutput("gauge_4"))), #, gaugeOutput("gauge_6")
#						conditionalPanel(condition = ("input.tipo_gauge_multiplas_datas != 'radio_setor_multiplas_datas'"), column(4, gaugeOutput("gauge_1_PIB"), gaugeOutput("gauge_2_PIB"), gaugeOutput("gauge_3_PIB")))
#                        )
#						
#						
#                      ))					 
#					  
#					  
#					  
#					  
#					  
#					  ))
					  
					  #,
					  
	        tabPanel("Time Series",
                      
            sidebarLayout(
            sidebarPanel(width = 3, # Numero de colunas
			
			              h3("Time Series"),
                          p("In the graph to the side it is possible to analyze the time evolution of all the indicators of the tracer."),
						  br(),
						  p("The indicators are grouped by five dimensions: ", strong("Activity Level, Labor Market, Sentiment Indicators, Credit and Default and Price Indices."), "."), 
						  br(),
                          
            #selectizeInput('series_compara', 'Escolha as séries a serem plotadas:', 
            #                as.character(unique(base_transformada$NomeVar)),
			#				selected = "IBCR",
            #                options = list(maxItems = length(as.character(unique(base_transformada$NomeVar))),
            #                               placeholder = 'Selecione uma lista de séries...')),
			
			# Árvore Hierárquica: unique(select(base_transformada, NomeVar, Categoria)) %>% arrange(desc(Categoria), desc(NomeVar))							   
			selectInput('series_compara_hierarquia', 'Choose indicator:', choices = list(
						  `Economic Activity` = c(`IBCR-RS` = 'IBCR-RS',
						                            `Retail Sales` = 'Retail Sales',
						                            `Services` = 'Services', 
													`Industry - Production` = 'Industry - Production', 
													`Industry - CU` = 'Industry - CU',
													`Industry - Performance` = 'Industry - Performance', 
													`Industry - Purchases` = 'Industry - Purchases'
													),
						  `Labor Market` = c(`Duration of Unemployment` = 'Duration of Unemployment', 
													`Real Income` = 'Real Income',
													`Unemployment Rate` = 'Unemployment Rate',
													`Employment - Agribusiness` = 'Employment - Agribusiness',
													`Employment - Total` = 'Employment - Total'),
						  `Sentiment Indicators` =  c(`Industry - Confidence` = 'Industry - Confidence', 
														  `Real Estate Market - Expectations` = 'Real Estate Market - Expectations',
														  `Intention of Household Consumption` = 'Intention of Household Consumption'), 
						  `Credit and Delinquency Rate` =  c(`Delinquency Rate` = 'Delinquency Rate', 
														  `Credit` = 'Credit'),
						  `Price Indices` = c(`CPI - General Index` = 'CPI - General Index',
						                                     `CPI - Food & Beverage` = 'CPI - Food & Beverage',
															 `CPI - Housing` = 'CPI - Housing',
															 `CPI - Household items` = 'CPI - Household items',
															 `CPI - Clothing` = 'CPI - Clothing',
															 `CPI - Transportation` = 'CPI - Transportation',
															 `CPI - Health and personal care` = 'CPI - Health and personal care',
															 `CPI - Personal expenses` = 'CPI - Personal expenses',
															 `CPI - Education` = 'CPI - Education',
															 `CPI- Comunication` = 'CPI- Comunication',
															 `Residential Real Estate Price - Purchase` = 'Residential Real Estate Price - Purchase',
															 `Residential Real Estate Price - Rent` = 'Residential Real Estate Price - Rent',
															 `Rental yield - Residences` = 'Rental yield (%) - Residences',
															 `Corporate Real Estate Price - Purchase` = 'Corporate Real Estate Price - Purchase',
															 `Corporate Real Estate Price - Rent` = 'Corporate Real Estate Price - Rent',
															 `Rental yield - Corporate Real Estate` = 'Rental yield (%) - Corporate Real Estate')
														  
						), selected = "IBCR-RS", selectize = FALSE),
										   
			checkboxInput('check_series_sazonal', "Time Series with Seasonal Adjustment", value=F),
            
            checkboxInput('check_recessoes', "Visualize periods of national recession?", value=F),
			
			conditionalPanel(condition = 'input.check_recessoes',
			div(bsButton("info_recessao", label = "Informations", icon = icon("info"), style = "info", size = "medium"), align = "center")),
						bsPopover(id = "info_recessao", title = "Recession Information",
                                                 content = paste0("The national recessive periods refer to the dates of the " ,a("CODACE - FGV/IBRE", 
                                                       href = "http://portalibre.fgv.br/main.jsp?lumChannelId=4028808126B9BC4C0126BEA1755C6C93",
                                                       target="_blank"), ". The periods represented by the dark gray color are already finalized in and out of a recession. The lighter gray represents a period of entry from a recession that has not yet had its final date defined by CODACE."),
                                                 placement = "right", 
                                                 trigger = "focus", # O 'click' só fechava quando clicava denovo 
                                                 options = list(container = "body")
                                       )
                          
                        ),
                        
                        # Show the plots
                        mainPanel(width = 9, # Complementar ao Sidebar
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Real Estate Market - Expectations"', p("Due to the reduced size of the serie, there is no seasonally adjusted decomposition.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Residential Real Estate Price - Rent"', p("Due to the reduced size of the serie, there is no seasonally adjusted decomposition.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Rental yield (%) - Residences"', p("Due to the reduced size of the serie, there is no seasonally adjusted decomposition.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Corporate Real Estate Price - Purchase"', p("Due to the reduced size of the serie, there is no seasonally adjusted decomposition.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Corporate Real Estate Price - Rent"', p("Due to the reduced size of the serie, there is no seasonally adjusted decomposition.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Rental yield (%) - Corporate Real Estate"', p("Due to the reduced size of the serie, there is no seasonally adjusted decomposition.")),
                        div(plotlyOutput("plot_series", height = "500px", width = "85%"), align = "center"), # height = "600px", width = "1000px"
						div(textOutput('fonte_series'), align = "center"),
						br(),
						div(textOutput('explicacao_series'), align = "center")
                        )
                      )),
					  
			tabPanel("Variations Table",
                      
            sidebarLayout(
			
			sidebarPanel(width = 3,
                          h3("Variations Table"),
                          p("The table beside presents the BCT-RS indicators in a synthetic way, highlighting the variations."),
						  br(),
						div(bsButton("info_tabela", label = "Informations", icon = icon("info"), style = "info", size = "medium"), align = "center"),
						bsPopover(id = "info_tabela", title = "Table Information",
                                                 content = paste0("It presents the raw value of the indicator without seasonal adjustment and its monthly variations in 12 months, as well as accumulated in 12 months. The table also presents variations of the series with seasonal adjustment being month against previous month and variation of the window of 3 months. The arrows indicate whether the indicator is increasing or decreasing, while the colors indicate whether the performance is good (green) or poor (red). The variances of the series that are measured in percentage are in absolute percentage points."),
                                                 placement = "right", 
                                                 trigger = "focus", # O 'click' só fechava quando clicava denovo 
                                                 options = list(container = "body")
                                       ),
						  br(),		   
									   
						  h4("Filter Table"),
						  checkboxInput('check_grupo_tabela', "Display only one group of indicators?", value=F),
						  
						  conditionalPanel(condition = 'input.check_grupo_tabela',
						  selectInput('grupo_tabela', 'Choose group:', 
                                      as.character(unique(base_transformada$Categoria)),
                                      selected = "Nível de Atividade"))
						  
                        ),
			
            mainPanel(width = 9, # Complementar do width do sidebar
                        div(sliderInput("data_jogo",
                        "Choose the month and year of the table:",
						min = min(aux_datas$Data) + years(2), # Adiciona dois anos, pois não tem como fazer o monitor nos primeiros dois anos
						max = max(aux_datas$Data),
                        value = max(aux_datas$Data),
						timeFormat = "%m-%Y",
						width = "90%",
						step = 31, # Número de dias em cada variação do slider
						animate = animationOptions(interval = 2500, loop = FALSE)), align = "center"),

                          
                        #),
                        
                        # Maneira 1
                        #mainPanel(
                        #div(plotlyOutput("plot_radar_ok", height = "auto", width = "1000px"), align = "center"),
						#hr(),
						#fluidRow(column(4,gaugeOutput("gauge_1")), column(4,gaugeOutput("gauge_2")), column(4,gaugeOutput("gauge_3"))),
						#fluidRow(column(4,gaugeOutput("gauge_4")), column(4,gaugeOutput("gauge_5")), column(4,gaugeOutput("gauge_6")))
                        #)
						
						# Maneira 2
						
						#conditionalPanel(condition = ("input.tipo_gauge_multiplas_datas == 'radio_setor_multiplas_datas'"), column(2, gaugeOutput("gauge_1"), gaugeOutput("gauge_2"))), #, gaugeOutput("gauge_3")
						br(),
						div(h3(textOutput("titulo_tabela")), align = "center"),
						br(),
						column(12, formattableOutput("tabela_brazuca")),
						p("* Not Seasonally Adjusted"),
						p("** Seasonally Adjusted"),
						p("NA - Not Available")
						#conditionalPanel(condition = ("input.tipo_gauge_multiplas_datas == 'radio_setor_multiplas_datas'"), column(2, gaugeOutput("gauge_3"), gaugeOutput("gauge_4"))), #, gaugeOutput("gauge_6")
						#conditionalPanel(condition = ("input.tipo_gauge_multiplas_datas != 'radio_setor_multiplas_datas'"), column(4, gaugeOutput("gauge_1_PIB"), gaugeOutput("gauge_2_PIB"), gaugeOutput("gauge_3_PIB")))
                        )
						

						
						
                      )),
					  
					  
					               #navbarMenu(title = "Dinâmica Macroeconômica",
               tabPanel("Macroeconomic Dynamics",
                      
                        # Show the plots
			
						
						sidebarLayout(
                        sidebarPanel(width = 3, # Numero de colunas
						

                         #selectizeInput('cat_monito', 'Escolha abaixo as datas para comparação no radar:', 
                         #   as.character(unique(base_transformada$CategoriaPIB)), # Ordena do mais recente para o menos recente
						 #	selected = as.character(unique(base_transformada$CategoriaPIB)), #transforma_dia_para_mes_ano(as.character(max(aux_datas$Data))),
                         #   options = list(maxItems = length(as.character(unique(base_transformada$CategoriaPIB))),
                         #                  placeholder = 'Selecione uma lista de datas...')),
						h4("Chart Properties"),
						
										   
					    checkboxInput('check_monitor_eixos_fixos', "Fixed axes over time", value=F),
						
						#sliderInput("opacidade_cor","Escolha a opacidade:", min = 0, max = 1, value = 0.85),
						
					    #checkboxInput('check_destaque_indicadores', "Destaque das cores nos indicadores", value=F),
						
						checkboxInput('check_incluir_PIB_monitor', "Include GDP in the chart", value=F),
						
						#checkboxInput('check_acumulado_12M_monitor', "Variações acumuladas em 12 meses", value=T),
						
						#selectizeInput('categorias_monitor', "Categorias dos indicadores do monitor",
						#            choices = unique(base_transformada$CategoriaPIB),
						#			selected = unique(base_transformada$CategoriaPIB),
						#			multiple = TRUE),
						
                        br(),						
						p(h5("Categories of indicators:")),
						checkboxInput('check_antecedente', "Antecedent", value=T),
						checkboxInput('check_coincidente', "Coincident", value=T),
						checkboxInput('check_defasada', "Lagging", value=T),
						
                        #checkboxInput('check_visualiza_grupo_monitor', "Visualizar somente uma categoria de indicadores", value=F),
						
						#conditionalPanel(condition = 'input.check_visualiza_grupo_monitor',
	                    #radioButtons("categoria_monitor", "Escolha a classificação:",
                        #               c("Antecedente" = "radio_antecedente",
                        #                 "Coincidente" = "radio_coincidente",
						#				 "Defasada" = "radio_defasada"))),
						
						p("NOTE: If the chosen period has no data, the graph will not appear.", style = "color:gray", align = "left"),
										 
						div(bsButton("info_monitor", label = "Informations", icon = icon("info"), style = "info", size = "medium"), align = "center"),
						bsPopover(id = "info_monitor", title = "Informations",
                                                 content = paste0("The graph colors represent the indicator stage at any given time in the economy. The green color in the upper right quadrant means that the indicator is performing ", 
												 span("well", style = "color:green"), " while the red color in the lower left quadrant means that the indicator is performing ", span("poorly", style = "color:red"), 
												 ". In addition, the series that move in the opposite direction of the GDP (countercyclical) such as Unemployment Rate, Duration of Unemployment and Default were inverted to maintain consistency of interpretation of the graph colors."),
                                                 placement = "right", 
                                                 trigger = "focus", # O 'click' só fechava quando clicava denovo 
                                                 options = list(container = "body")
                                       ),
						
						br(),
						br()
										 

                        ),
										 
				 
						mainPanel(width = 9,
						conditionalPanel(condition = '!input.check_destaque_indicadores',
						div(plotlyOutput("plot_monitor", height = "473px", width = "750px"), align = "center")), # 
						
						conditionalPanel(condition = 'input.check_destaque_indicadores',
						div(plotlyOutput("plot_monitor_cores_no_scatter", height = "600px", width = "950px"), align = "center")),
						
						br(),
						br(),
						
						#conditionalPanel(condition = '!input.check_incluir_PIB_monitor',
						div(sliderInput("data_slider_monitor",
                        "Choose the month and year of the chart:",
						min = min(aux_datas$Data) + years(2), # Adiciona dois anos, pois não tem como fazer o monitor nos primeiros dois anos
						max = max(aux_datas$Data),
                        value = max(aux_datas$Data),
						timeFormat="%m-%Y",
						width = "90%",
						step = 30.5, # Número de dias em cada variação do slider
						animate = animationOptions(interval = 2500, loop = FALSE)), align = "center"),#)#, # Dentro do plotlyOutput:, height = "700px", width = "950px"
						#h4("Crescimento: representa a taxa de crescimento do indicador no acumulado de 12 meses."),
						#h4("Aceleração: representa a variação absoluta mensal do crescimento.")
						span("Growth: represents the 12 months cumulative growth rate of the indicator.", style = "color:gray"),
						br(),
						span("Acceleration: represents the absolute variation of the monthly growth.", style = "color:gray")
						
						#conditionalPanel(condition = 'input.check_incluir_PIB_monitor',
						#div(sliderInput("data_slider_monitor",
                        #"Escolha o mês e ano de Análise:",
						#min = min(aux_datas_pib$Data),
						#max = max(aux_datas_pib$Data),
                        #value = max(aux_datas_pib$Data),
						#timeFormat="%m-%Y",
						#width = "75%",
						#step = 31, # Número de dias em cada variação do slider
						#animate = animationOptions(interval = 1250, loop = TRUE)), align = "center")) # Dentro do plotlyOutput:, height = "700px", width = "950px"
	
						) 


						#br(), br(),

						#column(12,
					    #div(sliderInput("data_slider_monitor",
                        #"Escolha o mês e ano de Análise:",
						#min = min(aux_datas$Data),
						#max = max(aux_datas$Data),
                        #value = max(aux_datas$Data),timeFormat="%m-%Y",
						#width = "50%",
						#step = 31, # Número de dias em cada variação do slider
						#animate = animationOptions(interval = 1250, loop = TRUE)), align = "center")
                      #)
				      #)
                      ))#,
					  
					  
					  
#					    tabPanel("Com Ajuste Sazonal",
#                      
#                        # Show the plots
#			
#						
#						sidebarLayout(
#                        sidebarPanel(width = 3, # Numero de colunas
#						
#
#                         #selectizeInput('cat_monito', 'Escolha abaixo as datas para comparação no radar:', 
#                         #   as.character(unique(base_transformada$CategoriaPIB)), # Ordena do mais recente para o menos recente
#						 #	selected = as.character(unique(base_transformada$CategoriaPIB)), #transforma_dia_para_mes_ano(as.character(max(aux_datas$Data))),
#                         #   options = list(maxItems = length(as.character(unique(base_transformada$CategoriaPIB))),
#                         #                  placeholder = 'Selecione uma lista de datas...')),
#						h4("Propriedades do Monitor"),
#						
#										   
#					    checkboxInput('check_monitor_eixos_fixos_com_sa', "Eixos fixos ao longo do tempo", value=F),
#						
#						#sliderInput("opacidade_cor_com_sa","Escolha a opacidade:", min = 0, max = 1, value = 0.85),
#						
#					    #checkboxInput('check_destaque_indicadores_com_sa', "Destaque das cores nos indicadores", value=F),
#						
#						checkboxInput('check_incluir_PIB_monitor_com_sa', "Incluir PIB no monitor", value=F),
#						
#						checkboxInput('check_acumulado_6M_monitor_com_sa', "Janela Móvel em 3 meses", value=F),
#						
#                        checkboxInput('check_visualiza_grupo_monitor_com_sa', "Visualizar somente uma categoria de indicadores", value=F),
#						
#						conditionalPanel(condition = 'input.check_visualiza_grupo_monitor_com_sa',
#	                    radioButtons("categoria_monitor_com_sa", "Escolha a classificação:",
#                                       c("Antecedente" = "radio_antecedente_com_sa",
#                                         "Coincidente" = "radio_coincidente_com_sa",
#										 "Defasada" = "radio_defasada_com_sa"))),
#										 
#										 
#						br(),
#										 
#						bsButton("info_monitor2", label = "Informações", icon = icon("info"), style = "info", size = "medium"),
#						bsPopover(id = "info_monitor2", title = "Informações do Monitor",
#                                                 content = paste0("As cores do monitor representam o estágio do indicador em um dado momento da economia. A cor verde, do quadrante superior direito, significa que o indicador está tendo um desempenho ", 
#												 span("bom", style = "color:green"), " enquanto que a cor vermelha, do quadrante inferior esquerdo, signfica que o indicador está tendo um desempenho ", span("ruim", style = "color:red"), 
#												 ". Além disso, as séries que se movimentam no sentido oposto ao PIB (contracíclicas) como Taxa de Desemprego, Tempo de Desemprego e Inadimplência foram invertidas para manter a consistência de interpretação das cores do monitor. Para maiores informações veja este ", 
#                                                                  a("link metodológico", 
#                                                                    href = "https://visualiza.fee.tche.br/BCT-RS",
#                                                                    target="_blank"),"."),
#                                                 placement = "right", 
#                                                 trigger = "focus", # O 'click' só fechava quando clicava denovo 
#                                                 options = list(container = "body")
#                                       ),
#						
#						br(),
#						br(),
#										 
#						p("OBS.: Se o período escolhido não tiver dados, o monitor não aparecerá", style = "color:gray", align = "left")
#                        ),
#										 
#				 
#						mainPanel(
#						conditionalPanel(condition = '!input.check_destaque_indicadores_com_sa',
#						div(plotlyOutput("plot_monitor_com_sa", height = "600px", width = "950px"), align = "center")),
#						
#						conditionalPanel(condition = 'input.check_destaque_indicadores_com_sa',
#						div(plotlyOutput("plot_monitor_cores_no_scatter_com_sa", height = "600px", width = "950px"), align = "center")),
#						
#						br(),
#						br(),
#						
#						#conditionalPanel(condition = '!input.check_incluir_PIB_monitor',
#						div(sliderInput("data_slider_monitor_com_sa",
#                        "Escolha o mês e ano de Análise:",
#						min = min(aux_datas$Data) + years(2), # Adiciona dois anos, pois não tem como fazer o monitor nos primeiros dois anos
#						max = max(aux_datas$Data),
#                        value = max(aux_datas$Data),
#						timeFormat="%m-%Y",
#						width = "75%",
#						step = 31, # Número de dias em cada variação do slider
#						animate = animationOptions(interval = 1250, loop = TRUE)), align = "center")#)#, # Dentro do plotlyOutput:, height = "700px", width = "950px"
#						
#						#conditionalPanel(condition = 'input.check_incluir_PIB_monitor',
#						#div(sliderInput("data_slider_monitor",
#                        #"Escolha o mês e ano de Análise:",
#						#min = min(aux_datas_pib$Data),
#						#max = max(aux_datas_pib$Data),
#						#timeFormat="%m-%Y",
#						#width = "75%",
#						#step = 31, # Número de dias em cada variação do slider
#						#animate = animationOptions(interval = 1250, loop = TRUE)), align = "center")) # Dentro do plotlyOutput:, height = "700px", width = "950px"
#	
#						) 
#
#
#						#br(), br(),
#
#						#column(12,
#					    #div(sliderInput("data_slider_monitor",
#                        #"Escolha o mês e ano de Análise:",
#						#min = min(aux_datas$Data),
#						#max = max(aux_datas$Data),
#                        #value = max(aux_datas$Data),timeFormat="%m-%Y",
#						#width = "50%",
#						#step = 31, # Número de dias em cada variação do slider
#						#animate = animationOptions(interval = 1250, loop = TRUE)), align = "center")
#                      #)
#				      #)
#                      ))
					  
					  #)
					  
					  ,

#	        tabPanel("Teste Multiple Plots",
#
#	        sidebarLayout(
 #           sidebarPanel(width = 3, # Numero de colunas
  #                        
   #         selectizeInput('series_compara_teste', 'Escolha as séries a serem plotadas:', 
    #                        as.character("Panorama"),
	#						selected = "Panorama",
     #                       options = list(maxItems = length(as.character(unique(base_transformada$NomeVar))),
      #                                     placeholder = 'Selecione uma lista de séries...')),
		#								   
		#	checkboxInput('check_series_sazonal_teste', "Series Temporais com Ajuste Sazonal", value=F),
         #   
          #  checkboxInput('check_recessoes_teste', "Visualizar períodos recessivos nacionais?", value=F)
#
 #           			),
#
 #           			mainPanel(width = 9,
  #          				div(plotlyOutput("plot_series_teste", height = "125px", width = "90%"), align = "center"))
   #                     )),
					  
			
			navbarMenu(title = "Download Data",
			    tabPanel("Not Seasonally Adjusted",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h3("Database Table"),
                          p("Search the information of your interest in the table."),
						  br(),
                          p("if you want to ", strong("download"), "of the full database, click the icon below."),
                          downloadButton('downloadData_sem', 'Download .csv')
                        ),
                        
                        # Show the plots
                        mainPanel(
                          dataTableOutput("tabela_download_sem_ajuste")
                        )
                      )),
					  
				tabPanel("Seasonally Adjusted",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h3("Database Table"),
                          p("Search the information of your interest in the table."),
						  br(),
                          p("if you want to ", strong("download"), "of the full database, click the icon below."),
                          downloadButton('downloadData_com', 'Download .csv')
                        ),
                        
                        # Show the plots
                        mainPanel(
                          dataTableOutput("tabela_download_com_ajuste")
                        )
                      ))
					  
					  )#,
			
			#tabPanel("Sobre",
            #          
            #          sidebarLayout(
            #            sidebarPanel(
            #              h2("Economics and Statistics Foundation"),
            #              p("A Economics and Statistics Foundation Siegfried Emanuel Heuser (FEE) é uma instituição de pesquisa, vinculada à Secretaria do Planejamento, Mobilidade e Desenvolvimento Regional do Governo do Estado do Rio Grande do Sul."),
            #              br(),
            #              br(),
            #              img(src = "fee_logo.png", height = 32, width = 132), 
            #              img(src = "logoGoverno.png", height = 62, width = 92),
            #              br(),
            #              br(),
            #              br(),
            #              "Shiny é um produto do ", 
            #              span("RStudio", style = "color:blue"),".", 
            #              br(), 
            #              br(),
            #              img(src = "bigorb.png", height = 82, width = 92)
            #            ),
            #            mainPanel(
            #             h1("Informações do Aplicativo"),
            #              p("Explicações do Ciclo de Negócios. Das estatísticas dos monitores e dos radares, etc. Quais séries são contra cíclicas e porque? Qual o critério de escolha das categorias de defasagem. Qual método de dessazonalização? Quais séries são em opntos percentuais?"),
            #              br(),
            #              p("O CicloVis-RS foi desenvolvido com o uso da ferramenta gratuita Shiny. Para uma introdução e outros exemplos, acesse ",
            #                a("Shiny homepage.", 
            #                  href = "http://www.rstudio.com/shiny")),
			#			  br(),				  
            #              br(),
            #              h2("Características do aplicativo"),
            #              p("* Visualize o monitor."),
            #              p("* Relacione as séries temporais."),
            #              p("* Visualize um radar dos indicadores e setores da economia."),
            #              br(),
            #              h3("Contato para dúvidas, sugestões ou solicitações:"),
			#			  p("Renan Xavier Cortes ",
            #                a("(CONTATO)", 
            #                  href = "http://www.fee.rs.gov.br/contato/", target="_blank"))
            #              #p("Renan Xavier Cortes (renan@fee.tche.br)")
            #              )
            #          )
			#		  )
					  
					  
					  
			)
			
))

