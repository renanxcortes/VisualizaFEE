# Pacotes
library(shiny)
#library(dygraphs)
library(plotly)
library(leaflet)
#library(leafletR)
#library(rgdal)
library(d3plus) # devtools::install_github("jpmarindiaz/d3plus")
#library(MTS)
#library(xts)
library(DT) # Data tables
library(C3) # devtools::install_github("FrissAnalytics/shinyJsTutorials/widgets/C3")
#library(highcharter)
library(spdep)
library(flexdashboard) # install.packages("flexdashboard", type = "source")
#library(flexclust)
#library(rgeos) # Função gSimplify

require(tidyverse)
#require(dplyr)
#require(tidyr)
require(stringi)
require(shinyBS) # Pelos botoes de popover e tooltip
require(shinythemes)

# Para definir a imagem
#setwd("C:/Users/renan/Desktop/Shiny Apps/ProjetoAppCrime")
#setwd("C:/Users/Windows 8.1/Desktop/Shiny Apps/ProjetoAppCrime")
#base_crime_pre <- read.csv("Base_Crimes_FEE.csv", header=T, sep=";")
#base_crime <- subset(base_crime_pre, !(Mun == "Pinto Bandeira" & Ano < 2013)) # Retira os dados que daram problemas depois
#mapa_rs <-  readOGR("C:/Users/renan/Desktop/Shiny Apps/Shapes", "43mu2500gsd", encoding='UTF-8', verbose = FALSE)
##mapa_rs <-  readOGR("C:/Users/Windows 8.1/Desktop/Shiny Apps", "43mu2500gsd", encoding='UTF-8', verbose = FALSE)

#load("C:/Users/renan/Desktop/Shiny Apps/ProjetoAppCrime/Imagem_AppCrime.Rdata")
#load("C:/Users/Windows 8.1/Desktop/Shiny Apps/ProjetoAppCrime/Imagem_AppCrime.Rdata")
#load("srv/shiny-server/crime/Imagem_AppCrime.Rdata")

#base_crime <- readRDS("BaseCrime_2016.rds") # Está em tibble
base_crime <- readRDS("base_crimevis_2016_pop_ok.rds") # Está em tibble

base_crime$Mun <- stri_conv(as.character(base_crime$Mun), "latin1", "UTF-8")
base_crime$Crime <- stri_conv(as.character(base_crime$Crime), "latin1", "UTF-8")

mapa_rs <- readRDS("MapaRS.rds")
mapa_rs@data$Nome_Munic <- stri_conv(as.character(mapa_rs@data$Nome_Munic), "latin1", "UTF-8")

cods_rmpa <- c(4300604,4300877,4301107,4303103,4303905,4304606,4304689,4305355,4306403,4306767,4307609,4307708,4309050,4309209,4309308,4310108,4310801,4312401,4313060,4313375,4313409,4314050,4314803,4314902,4316006,4317608,4318408,4318705,4319505,4319901,4320008,4321204,4322004,4323002)

options(shiny.sanitize.errors = FALSE)

# Definindo o UI
shinyUI(fluidPage(includeCSS("estilocrime.css"), htmlOutput("frame"), theme = shinytheme("cerulean"), tags$head(tags$script(src="tracking.js")), tags$head(tags$link(rel="shortcut icon", href="feeicon.ico")),
        tags$div(
  HTML("<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-1506603-5', 'auto');
  ga('send', 'pageview');

</script>
")
),
tags$style(type="text/css", # isso é para não mostrar nenhuma mensagem vermelha de erro!!
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
),

# Essa parte de baixo dá uma "empurradinha" pra direita o navbarheader pra aparecer o nome do app: https://stackoverflow.com/questions/9792849/how-to-insert-spaces-tabs-in-text-using-html-css
tags$style(type = 'text/css',

                           '.navbar-default .navbar-brand {
							 padding-left: 35px;
                           }'

                           ),  
  # Application title
  #titlePanel(paste0("- ",  "CrimeVis: Visualização da Criminalidade Anual no Rio Grande do Sul")),
  
  navbarPage("CrimeVis", # Navegação
             tabPanel("Apresentação", #Apresentação
                      
                      sidebarLayout(
                        sidebarPanel(
                          h2("Fundação de Economia e Estatística"),
                          p("A Fundação de Economia e Estatística Siegfried Emanuel Heuser (FEE) é uma instituição de pesquisa, vinculada à Secretaria do Planejamento, Mobilidade e Desenvolvimento Regional do Governo do Estado do Rio Grande do Sul."),
                          br(),
                          br(),
                          img(src = "fee_logo.png", height = 32, width = 132), 
                          img(src = "logoGoverno.png", height = 62, width = 92),
                          br(),
                          br(),
                          br(),
                          "Shiny é um produto do ", 
                          span("RStudio", style = "color:blue"),".", 
                          br(), 
                          br(),
                          img(src = "bigorb.png", height = 82, width = 92)
                        ),
                        mainPanel(
                          h1("Visualização da criminalidade anual no Rio Grande do Sul"),
                          p("O aplicativo CrimeVis é um produto da Fundação de Economia e Estatística que apresenta, 
                            de maneira interativa e dinâmica, os crimes dos municípios do Rio Grande do Sul (RS) 
                            com dados anuais disponibilizados pela Secretaria de Segurança Pública do RS (SSP-RS). Seus 
                            dados brutos podem ser acessados tanto no ",em("site")," da SSP-RS neste", 
                            a("link", href = "http://www.ssp.rs.gov.br/"), "na parte de indicadores 
                            criminais, quanto no FEEDADOS neste", a("link", href = "http://feedados.fee.tche.br/feedados/",".")),
                          br(),
                          p("O CrimeVis foi desenvolvido com o uso da ferramenta gratuita Shiny. Para uma introdução e outros exemplos, acesse ",
                            a("Shiny homepage.", 
                              href = "http://www.rstudio.com/shiny")),
						  br(),
                          #p(em("Nota: Como os dados de criminalidade de 2016 já estão disponíveis, os dados populacionais de 2016 são as estimativas de 2015 neste aplicativo para o cálculo das taxas. Assim que as estimativas populacionais de 2016 forem calculadas, elas serão atualizadas no CrimeVis.")),						  
                          br(),
                          h2("Características do aplicativo"),
                          p("* Visualize séries temporais dos municípios e do Estado por número de ocorrências e taxas."),
                          p("* Relacione crimes em gráficos de dispersão e crie grupos de cidades."),
                          p("* Visualize os dados em mapas, de maneira interativa, e calcule autocorrelações espaciais dos crimes. Adicionalmente, verifique a evolução espaço temporal de presença/ausência de crimes através de Cadeias de Markov."),
                          p("* Obtenha a representação municipal no Estado de maneira rápida e intuitiva."),
                          p("* Realize pesquisas rápidas na base de dados e faça o ",em("download")," dos dados."),
                          br(),
                          h3("Contato para dúvidas, sugestões ou solicitações:"),
						  p("Renan Xavier Cortes ",
                            a("(CONTATO)", 
                              href = "http://www.fee.rs.gov.br/contato/", target="_blank"))
                          #p("Renan Xavier Cortes (renan@fee.tche.br)")
                          )
                      )),
             navbarMenu(title = "Séries Temporais",
               tabPanel("Compara Crimes",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput('cidade_compara_crime', 'Cidade a ser escolhida', 
                                      as.character(unique(base_crime$Mun)),
                                      selected = "Porto Alegre"),
                          
                          selectizeInput('crimes_compara_crimes', 'Escolha até 5 crimes', 
                                      as.character(unique(base_crime$Crime)),
                                      selected = c("Homicídio Doloso"),
                                      options = list(maxItems = 5,
                                                     placeholder = 'Selecione uma lista de crimes...')),
                          
                          radioButtons("tipo_dado_compara_crime", "Tipo de Informação:",
                                       c("Número de Ocorrências" = "ocorre_radio_compara_crime",
                                         "Taxa por 100.000" = "taxa_radio_compara_crime"))
                          
                        ),
                        
                        # Show the plots
                        mainPanel(
                          tabsetPanel(type = "tabs", 
                                      tabPanel("Municípios", plotlyOutput("ts_compara_crime_cidades")), 
                                      tabPanel("Estado", plotlyOutput("ts_compara_crime_rs"))
                          )
                        )
                      )),
               tabPanel("Compara Municípios",
                        
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectizeInput('cidades_compara', 'Escolha até 5 cidades', 
                                        as.character(unique(base_crime$Mun)),
                                        selected = c("Porto Alegre"),
                                        options = list(maxItems = 5,
                                                       placeholder = 'Selecione uma lista de municípios...')),
                            
                            selectInput('crime_compara', 'Crime a ser escolhido', 
                                        as.character(unique(base_crime$Crime)),
										selected = "Roubo"),
                            
                            radioButtons("tipo_dado_compara_municipio", "Tipo de Informação:",
                                         c("Número de Ocorrências" = "ocorre_radio_compara_municipio",
                                           "Taxa por 100.000" = "taxa_radio_compara_municipio")),
                            
                            checkboxInput("checkbox_inclui_rs_rmpa_ts", label = "Incluir outra série?", value = FALSE),
							
							conditionalPanel(condition = 'input.checkbox_inclui_rs_rmpa_ts',
                            radioButtons("radio_estado_rmpa", "Qual região?",
                                         c("Rio Grande do Sul" = "radio_rs",
                                           "Região Metropolitana de Porto Alegre (RMPA)" = "radio_rmpa"
                                           ))
                            )
							
                            
                          ),
                          
                          # Show the plots
                          mainPanel(
                            plotlyOutput("ts_compara_cidades")
                          )
                        ))), 
             tabPanel("Relação entre crimes",
                                   sidebarLayout(
                                     sidebarPanel(
                                       
                                       sliderInput('ano_disp', 'Ano a ser escolhido', 
                                                   min = min(base_crime$Ano),
                                                   max = max(base_crime$Ano),
                                                   value = max(base_crime$Ano),
                                                   step=1,
                                                   animate = animationOptions(interval = 1750, loop = TRUE), sep=""), # 1500
                                       
								       sliderInput('dispersao_mun_pop_control', "População:",
						                           min = 0,
						                           max = 1500000,
						                           value = range(c(0, 1500000)),
						                           step = 2500),
									   
                                       selectInput('crimex', 'Selecione o crime do eixo horizontal', 
                                                   as.character(unique(base_crime$Crime)),
                                                   selected = "Homicídio Doloso"),
                                       
                                       selectInput('crimey', 'Selecione o crime do eixo vertical', 
                                                   as.character(unique(base_crime$Crime)),
                                                   selected = "Roubo"),
                                       
                                       radioButtons("tipo_dado_disp", "Tipo de Informação:",
                                                    c("Número de Ocorrências" = "ocorre_radio_disp",
                                                      "Taxa por 100.000" = "taxa_radio_disp")),

                                       fluidRow(
                                         column(8, checkboxInput("checkbox_kmeans", label = "Criar grupos de municípios?", 
                                                                 value = FALSE)),
                                        column(4, bsButton("q_kmeans", label = "Ajuda", icon = icon("info"),
                                                           style = "info", size = "medium")),
                                        bsPopover(id = "q_kmeans", title = "K-Means",
                                                 content = paste0("Os grupos de municípios a serem criados seguem o algoritmo de clusterização denominado ", strong("K-Means"), ". Este algoritmo agrupa cidades similares entre si de acordo com as suas distâncias entre os crimes escolhidos. Isto é, municípios que tem ocorrências ou taxas próximas, estarão no mesmo grupo. Para maiores informações veja este ", 
                                                                  a("link metodológico", 
                                                                    href = "https://www.coursera.org/learn/machine-learning/lecture/93VPG/k-means-algorithm",
                                                                    target="_blank"),"."),
                                                 placement = "right", 
                                                 trigger = "focus", # O 'click' só fechava quando clicava denovo 
                                                 options = list(container = "body")
                                       )),
                                       
                                       conditionalPanel(condition = 'input.checkbox_kmeans',
                                                        numericInput("n_grupos_kmeans", "Número de Grupos:", 3, min = 2, max = 496)
                                                        )
                                       
                                     ),
                                     
                                     # Show the plots
                                     mainPanel(
                                       plotlyOutput("dispersao")
                                     )
                                   )),
             tabPanel("Mapas",
			 
			 mainPanel(width = 12,
                                tabsetPanel(type = 'tabs',
                      tabPanel("Mapa Dinâmicos",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput('ano_mapa', 'Ano a ser escolhido', 
                                      min = min(base_crime$Ano),
                                      max = max(base_crime$Ano),
                                      value = max(base_crime$Ano),
                                      step=1,
                                      animate = animationOptions(interval = 1750, loop = TRUE), sep=""),
                          
                          conditionalPanel(condition = ("input.tipo_dado_mapa != 'pop_radio_mapa'"),
                                           selectInput('crime_mapa', 'Crime a ser escolhido', 
                                                       as.character(unique(base_crime$Crime)),
                                                       selected = "Homicídio Doloso")),
                          
                          radioButtons("tipo_dado_mapa", "Tipo de Informação:",
                                       c("Número de Ocorrências" = "ocorre_radio_mapa",
                                         "Taxa por 100.000" = "taxa_radio_mapa",
                                         "População" = "pop_radio_mapa")),
                          
                          conditionalPanel(condition = ("input.tipo_dado_mapa == 'pop_radio_mapa'"), 
                                           sliderInput("sens_mapa", "Sensibilidade do Mapa:", 
                                                       min=1.5, max=2.5, value=1.9)),
                          
                          fluidRow(
                            column(8, checkboxInput("checkbox_moran", label = "Calcular autocorrelação espacial?", 
                                                    value = FALSE)),
                            column(4, bsButton("q_moran", label = "Ajuda", icon = icon("info"),
                                   style = "info", size = "medium"),
                          bsPopover(id = "q_moran", title = "Autocorrelação Espacial",
                                    content = paste0("A autocorrelação espacial é uma medida estatística que mede o quanto um município é afetado pelos seus vizinhos e, para isso, é necessário determinar uma estrutura de vizinhança. A medida utilizada é o índice ", strong("I de Moran"), " que varia de -1 até 1. Índices próximos de 1 significam que municípios vizinhos possuem valores similares, já o valor de -1 significa que a relação criminal é inversa, ou seja, altos valores implicam em valor baixos de vizinhos e vice-versa. O valor zero indica ausência de autocorrelação espacial. Para maiores informações veja este ", 
                                                     a("link metodológico", 
                                                       href = "http://www.leg.ufpr.br/lib/exe/fetch.php/disciplinas:cieg:intro-areas.pdf",
                                                       target="_blank"),"."),
                                    placement = "right", 
                                    trigger = "focus", # O 'click' só fechava quando clicava denovo 
                                    options = list(container = "body")
                          ))),
                          
                          conditionalPanel(condition = 'input.checkbox_moran',
                                           selectInput('tipo_estrutura_espacial', "Tipo de estrutura de vizinhança:", 
                                                       choices = c("Municípios que fazem fronteira", "Municipios mais próximos"))),
                          
                          conditionalPanel(condition = 'input.tipo_estrutura_espacial == "Municipios mais próximos"',
                                           conditionalPanel(condition = 'input.checkbox_moran',
                                           numericInput("n_vizinhos_moran", "Número de vizinhos:", 2, min = 1, max = 496))
                          )
                          
                        ),
                        
                        # Show the plots
                        mainPanel(
                          leafletOutput("mapa_final"),
                          hr(),
                          conditionalPanel(condition = 'input.checkbox_moran',
                                           fluidRow(column(4,gaugeOutput("gauge_moran"),
										                     bsTooltip("gauge_moran", title = "Autocorrelação espacial calculada", placement = "top", 
                                                                     trigger = "hover",
                                                                     options = NULL)),
                                                    column(8,plotOutput("mapinha_grafo"))))
                        )
                      ))
					  
					  ,tabPanel("Cadeias de Markov",
                                
                                sidebarLayout(
                                  sidebarPanel(
                                    
                                    selectInput('crime_markov', 'Crime a ser analisado', 
                                                                 as.character(unique(base_crime$Crime)),
                                                                 selected = "Roubo de Veículos"),
                                    
                                    sliderInput('anos_markov', "Período de Análise:",
                                                min = min(base_crime$Ano),
                                                max = max(base_crime$Ano),
                                                value = range(c(min(base_crime$Ano), max(base_crime$Ano))),
                                                sep = ""), # Para remover o separador de milhar
                                    
                                    radioButtons("tipo_analise_markov", "Tipo de Análise Markoviana:",
                                                 c("Temporal" = "radio_matriz_markov",
                                                   "Espaço-Temporal" = "radio_matriz_markov_estratificada")),
									
									br(),
												   
									p('Inspirado em Rey, Sergio J., Elizabeth A. Mack, and Julia Koschinsky. ', em('Exploratory space–time analysis of burglary patterns.'), 'Journal of Quantitative Criminology 28.3 (2012): 509-531.')
                                    
                                    
                                  ),
                                  
                                  # Show the plots
                                  mainPanel(plotOutput("mapas_markov"), 
                                  conditionalPanel(condition = ("input.tipo_analise_markov == 'radio_matriz_markov'"),
                                            div(h3("Matriz de Markov de Probabilidades de Transição Temporal"), align = "center"),
                                            div(tableOutput("tabela_markov_simples"), align = "center"),
                                            div(h4("Tabela de Razões de Chance"), align = "center"),
                                            div(tableOutput("odds_ratio_simples"), align = "center"),
                                            br(),
                                            div(h4("Matriz de Frequências e Teste de Independência"), align = "center"),
                                            fluidRow(column(8, tableOutput("tabela_bruta")),
                                                     div(column(4, h4("Teste Qui-Quadrado"), br(), verbatimTextOutput("estatistica_chi")), align = "center"))),
                                  
                                  conditionalPanel(condition = ("input.tipo_analise_markov == 'radio_matriz_markov_estratificada'"),
                                                   div(h3("Matriz de Markov de Probabilidades de Transição Espaço-Temporal"), align = "center"),
                                                   div(tableOutput("tabela_markov_estratificada"), align = "center"),
                                                   div(h4("Tabela de Razões de Chance"), align = "center"),
                                                   div(tableOutput("odds_ratio_estratificado"), align = "center"))
                                  
                                  
                                )
                                )
                                
                                
                                )
                      
                      ))) # !
					  
					  ,
             
             tabPanel("Representação no estado",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput('ano_tree', 'Ano a ser escolhido', 
                                      min = min(base_crime$Ano),
                                      max = max(base_crime$Ano),
                                      value = max(base_crime$Ano),
                                      step = 1,
                                      animate = animationOptions(interval = 1750, loop = TRUE), sep=""),
                          
                          conditionalPanel(condition = ("input.tipo_dado_tree != 'pop_radio_tree'"),
                          selectInput('crime_tree', 'Crime a ser escolhido', 
                                      as.character(unique(base_crime$Crime)),
                                      selected = "Homicídio Doloso")),
                          
                          radioButtons("tipo_dado_tree", "Tipo de Informação:",
                                       c("Número de Ocorrências" = "ocorre_radio_tree",
                                         "Taxa por 100.000" = "taxa_radio_tree",
                                         "População" = "pop_radio_tree"))
                          
                        ),
                        
                        # Show the plots
                        mainPanel(
                          d3plusOutput("tree_map")
                        )
                      )),
             tabPanel("Download dos Dados",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h2("Tabela de Dados"),
                          p("Pesquise as informações de seu interesse ao lado."),
						  br(),
                          p("Se você deseja fazer o ", strong("download"), "da base completa, clique no ícone abaixo."),
                          downloadButton('downloadData', 'Baixar .csv')
                        ),
                        
                        # Show the plots
                        mainPanel(
                          dataTableOutput("tabela")
                        )
                      )))
  
  # Sidebar
  
  # Fim do User Interface
))