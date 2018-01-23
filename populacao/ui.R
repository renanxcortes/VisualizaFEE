# Packages
library(dplyr)
library(plotly)
library(stringi)
library(tidyverse)
library(leaflet)
library(shinythemes)
library(D3plusR)
library(DT) 


df_pre_pre <- readRDS("popvisBase_2016.rds")

corresp <- readRDS("Corresp_Mun_PopRS.rds")
df_pre <- inner_join(df_pre_pre, corresp, by = "CodIBGE")
df_proj <- readRDS("proj_rs.rds")
df_pre$Mun <- stri_conv(as.character(df_pre$Mun), "latin1", "UTF-8")
df_pre$Classe <- stri_conv(as.character(df_pre$Classe), "latin1", "UTF-8")
df_pre$Corede <- stri_conv(as.character(df_pre$Corede), "latin1", "UTF-8")


options(shiny.sanitize.errors = FALSE)

# Definindo o UI
shinyUI(function(request){fluidPage(includeCSS("estilodemo.css"), 

				 tags$iframe(src="https://datavis2.fee.tche.br/frame/frame.html", 
                           height="31px", 
                           width = "100%", 
                           borderbottom = "1px",
                           solid = "#6CB3D4",     
                           bordertop = "0px",
                           borderleft = "0px",
                           borderright = "0px",
                           margin = "0px",
                           padding= "0px"),
						   
				  theme = shinytheme("cerulean"), tags$head(tags$script(src="tracking.js")), tags$head(tags$link(rel="shortcut icon", href="feeicon.ico")),
                  tags$div(
                    HTML("<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-1506603-6', 'auto');
  ga('send', 'pageview');

</script>")
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
                  
                  navbarPage(id = "navegation_bar", "PopVis",
                             tabPanel(id = "home_page", "Apresentação",
                                      
                                      sidebarLayout(
                                        sidebarPanel(
                                          h2("Fundação de Economia e Estatística"),
                                          p("A Fundação de Economia e Estatística Siegfried Emanuel Heuser (FEE) é uma instituição de pesquisa, vinculada à Secretaria do Planejamento, Mobilidade e Desenvolvimento Regional do Governo do Estado do Rio Grande do Sul."),
                                          br(),
                                          br(),
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
                                          h1("PopVis: Portal Demográfico da FEE"),
                                          p("Este aplicativo permite a visualização, de maneira interativa, dos dados de demografia do Rio Grande do Sul. Ele faz uso das estimativas populacionais calculadas pelo Núcleo de Demografia e Previdência, vinculado ao Centro de Indicadores Econômicos e Sociais, da Fundação de Economia e Estatística. Além disso, utiliza dados de projeções do Instituto Brasileiro de Geografia e Estatística (IBGE)."),
                                          br(),
                                          p("O PopVis foi feito utilizando a ferramenta gratuita Shiny. Para uma introdução e outros exemplos, acesse ",
                                            a("Shiny homepage.", 
                                              href = "http://www.rstudio.com/shiny")),
                                          br(),
                                          h2("Características do aplicativo"),
                                          p("* Construa pirâmides etárias de todos os municípios, Conselhos Regionais de Desenvolvimento (Coredes), Regiões Funcionais e do Estado para todos os anos disponíveis."),
                                          p("* Construa pirâmides etárias das projeções estaduais."),
                                          p("* Visualize mapas dinâmicos de municípios, Coredes e Regiões Funcionais."),
										  p("* Alterne a variável do mapa entre população bruta, percentual de pessoas na região e variação populacional em todo o período."),
                                          p("* Analise a representação de cada região no Estado."),
                                          p("* Faça qualquer análise para qualquer gênero ou grupo etário."),
										  p("* Realize o ",em("download")," dos dados."),
                                          br(),
                                          h3("Contato para dúvidas, sugestões ou solicitações de código:"),
                                          p("Renan Xavier Cortes ou Pedro Tonon Zuanazzi ", a("(CONTATO)", 
                                                                                              href = "http://www.fee.rs.gov.br/contato/", target="_blank")),
						  
						  br(),
						  br(),
						  div(img(href = "http://creativecommons.org/licenses/by/4.0/", src="https://i.creativecommons.org/l/by/4.0/88x31.png"), align = "center"),
						  div(p("Este obra está licenciada com uma Licença"), align = "center"),
						  div(a("Creative Commons Atribuição 4.0 Internacional",
                              href = "http://creativecommons.org/licenses/by/4.0/", target="_blank"), align = "center")
                                        )
                                      )),
                             navbarMenu(title = "Pirâmides Etárias",
                                        
                                        tabPanel(id = "piramides_municipios", "Municípios",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     selectInput('cidade_piramide', 'Cidade a ser escolhida', 
                                                                 as.character(unique(df_pre$Mun)),
                                                                 selected = "Porto Alegre"),
                                                     
                                                     sliderInput('ano_piramide', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     br(),
                                                     br(),
                                                     
                                                     
                                                     leafletOutput("mapinha_municipios"),
													 
                                                     br(),
                          div(bookmarkButton(id = "book_piramides_municipios",
                                               label = "Compartilhe...",
                                               icon = icon("share-alt"),
                                               title = "Compartilhe essa informação!"), align = "center"),
											   
											   br(),
											   
                                                     p("Inspirado numa aplicação do ", a("Center for Urban Studies", href = "http://urbanstudies.tcu.edu/", target="_blank")," do Texas Christian University.")
                                                     
                                                   ),
                                                   mainPanel(
                                                     plotlyOutput("piramide_municipio"),
													 br(),
													 #div(h4("População"), align = "center"),
                                            		 div(tableOutput("pop_municipio_piramide"), align = "center"),
										p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                            href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   ))),
                                        
                                        tabPanel(id = "piramides_coredes", "Coredes",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     selectInput('corede_piramide', 'Corede a ser escolhido', 
                                                                 as.character(unique(df_pre$Corede)),
                                                                 selected = "Metropolitano Delta do Jacuí"),
                                                     
                                                     sliderInput('ano_piramide_corede', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1000, loop = FALSE), sep=''),
                                                     
                                                     br(),
                                                     br(),
                                                     
                                                     leafletOutput("mapinha_corede"),
                                                     br(),
                          div(bookmarkButton(id = "book_piramides_coredes",
                                               label = "Compartilhe...",
                                               icon = icon("share-alt"),
                                               title = "Compartilhe essa informação!"), align = "center"),
											   
											   br(),
											   
                                                     p("Inspirado numa aplicação do ", a("Center for Urban Studies", href = "http://urbanstudies.tcu.edu/", target="_blank")," do Texas Christian University.")
                                                     
                                                   ),
                                                   mainPanel(
                                                     plotlyOutput("piramide_corede"),
													 br(),
													 #div(h4("População"), align = "center"),
                                            		 div(tableOutput("pop_corede_piramide"), align = "center"),
										p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                            href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   ))),
                                        
                                        tabPanel(id = "piramides_rf", "Regiões Funcionais",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     selectInput('rf_piramide', 'Região funcional a ser escolhida', 
                                                                 as.character(sort(unique(df_pre$CodRF))), # Ordenando os números
                                                                 selected = "1"),
                                                     
                                                     sliderInput('ano_piramide_rf', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1000, loop = FALSE), sep=''),
                                                     
                                                     br(),
                                                     br(),
                                                     
                                                     leafletOutput("mapinha_rf"),
                                                     br(),
                          div(bookmarkButton(id = "book_piramides_rf",
                                               label = "Compartilhe...",
                                               icon = icon("share-alt"),
                                               title = "Compartilhe essa informação!"), align = "center"),
											   
											   br(),
											   
                                                     p("Inspirado numa aplicação do ", a("Center for Urban Studies", href = "http://urbanstudies.tcu.edu/", target="_blank")," do Texas Christian University.")
                                                     
                                                   ),
                                                   mainPanel(
                                                     plotlyOutput("piramide_rf"),
													 br(),
                                            		 div(tableOutput("pop_rf_piramide"), align = "center"),
										p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                            href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   ))),	  
                                        
                                        tabPanel(id = "piramides_estado", "Estado",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_piramide_rs', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1000, loop = FALSE), sep=''),
                                                     
                                                     br(),
                          div(bookmarkButton(id = "book_piramides_estado",
                                               label = "Compartilhe...",
                                               icon = icon("share-alt"),
                                               title = "Compartilhe essa informação!"), align = "center"),
											   
											   br(),
											   
                                                     p("Inspirado numa aplicação do ", a("Center for Urban Studies", href = "http://urbanstudies.tcu.edu/", target="_blank")," do Texas Christian University.")
                                                     
                                                     
                                                   ),
                                                   mainPanel(
                                                     plotlyOutput("piramide_rs"),
													 br(),
                                            		 div(tableOutput("pop_rs_piramide"), align = "center"),
										p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                            href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   ))),
                                        
                                        tabPanel(id = "piramides_projecao", "Projeção Estadual",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_piramide_rs_proj', 'Ano a ser escolhido', 
                                                                 min = min(df_proj$Ano),
                                                                 max = max(df_proj$Ano),
                                                                 value = max(df_proj$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1000, loop = FALSE), sep=''),
                                                     
                                                     br(),
                          div(bookmarkButton(id = "book_piramides_projecoes",
                                               label = "Compartilhe...",
                                               icon = icon("share-alt"),
                                               title = "Compartilhe essa informação!"), align = "center"),
											   
											   br(),
											   
                                                     p("Inspirado numa aplicação do ", a("Center for Urban Studies", href = "http://urbanstudies.tcu.edu/", target="_blank")," do Texas Christian University.")
                                                     
                                                     
                                                   ),
                                                   mainPanel(
                                                     plotlyOutput("piramide_rs_proj"),
													 br(),
													 div(h4("População"), align = "center"),
                                            		 div(tableOutput("pop_proj_piramide"), align = "center"),
										p('Fonte dos Dados: ', a('Projeções populacionais IBGE – Revisão 2013', 
                            href = 'http://www.ibge.gov.br/home/estatistica/populacao/projecao_da_populacao/2013/default.shtm', target = '_blank'))
                                                   )))
                                        
                             ),
                             
                             
                             navbarMenu(title = "Mapas",
                                        
                                        tabPanel(id = "mapas_municipios", "Municípios",
                                        	tabsetPanel(type = 'tabs',
                                        		tabPanel(id = "mapa_bruto_municipios", "População Bruta",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_mapa_t1', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     radioButtons("genero_mapa_t1", "Tipo de Informação:",
                                                                  c("Total" = "total_mapa_radio",
                                                                    "Homens" = "homens_mapa_radio",
                                                                    "Mulheres" = "mulheres_mapa_radio")),
                                                     
                                                     selectizeInput('classe_mapa_t1', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),
                                                     
                                                     sliderInput("sens_mapa_t1", "Sensibilidade dos Círculos do Mapa:", 
                                                                 min=1.5, max=2.5, value=2),
                                                     
													  br(),
							                          div(bookmarkButton(id = "book_mapas_municipios_t1",
							                                               label = "Compartilhe...",
							                                               icon = icon("share-alt"),
							                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                   ),
                                                   mainPanel(
                                                   		leafletOutput("mapa_genero"),
                                                   		br(),
                                                   		p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                          								href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   	)
                                                   )),
                                        		tabPanel(id = "mapa_perc_municipios", "Percentual (%) no Município",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_mapa_t2', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     radioButtons("genero_mapa_t2", "Tipo de Informação:",
                                                                  c("Total" = "total_mapa_radio",
                                                                    "Homens" = "homens_mapa_radio",
                                                                    "Mulheres" = "mulheres_mapa_radio")),
                                                     
                                                     selectizeInput('classe_mapa_t2', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),                                       
													  br(),
							                          div(bookmarkButton(id = "book_mapas_municipios_t2",
							                                               label = "Compartilhe...",
							                                               icon = icon("share-alt"),
							                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                   ),
                                                   mainPanel(
                                                   		leafletOutput("mapa_genero_percent"),
                                                   		br(),
                                                   		p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                          								href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   	)
                                                   )),
                                        		tabPanel(id = "mapa_var_municipios", "Variação (%) no Período Escolhido",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_mapa_t3', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = c(min(df_pre$Ano),max(df_pre$Ano)),
                                                                 step = 1,
                                                                 sep=''),
                                                     
                                                     radioButtons("genero_mapa_t3", "Tipo de Informação:",
                                                                  c("Total" = "total_mapa_radio",
                                                                    "Homens" = "homens_mapa_radio",
                                                                    "Mulheres" = "mulheres_mapa_radio")),
                                                     
                                                     selectizeInput('classe_mapa_t3', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),                                       
													  br(),
							                          div(bookmarkButton(id = "book_mapas_municipios_t3",
							                                               label = "Compartilhe...",
							                                               icon = icon("share-alt"),
							                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                   ),
                                                   mainPanel(
                                                   		h4(textOutput("titulo_mapa_genero_variacao"), align = "center"),
                                                   		leafletOutput("mapa_genero_variacao"),
                                                   		br(),
                                                   		p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                          								href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   	)
                                                   ))
                                        		)),                                       
                                        
                                        tabPanel(id = "mapas_coredes", "Coredes",
                                        	tabsetPanel(type = 'tabs',
                                        		tabPanel(id = "mapa_bruto_corede", "População Bruta",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_mapa_cr_t1', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     radioButtons("genero_mapa_cr_t1", "Tipo de Informação:",
                                                                  c("Total" = "total_mapa_radio",
                                                                    "Homens" = "homens_mapa_radio",
                                                                    "Mulheres" = "mulheres_mapa_radio")),
                                                     
                                                     selectizeInput('classe_mapa_cr_t1', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),
                                                     
                                                     sliderInput("sens_mapa_cr_t1", "Sensibilidade dos Círculos do Mapa:", 
                                                                 min=1.5, max=2.5, value=2),
                                                     
													  br(),
							                          div(bookmarkButton(id = "book_mapas_coredes_t1",
							                                               label = "Compartilhe...",
							                                               icon = icon("share-alt"),
							                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                   ),
                                                   mainPanel(
                                                   		leafletOutput("mapa_genero_cr"),
                                                   		br(),
                                                   		p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                          								href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   	)
                                                   )),
                                        		tabPanel(id = "mapa_perc_coredes", "Percentual (%) nos Coredes",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_mapa_cr_t2', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     radioButtons("genero_mapa_cr_t2", "Tipo de Informação:",
                                                                  c("Total" = "total_mapa_radio",
                                                                    "Homens" = "homens_mapa_radio",
                                                                    "Mulheres" = "mulheres_mapa_radio")),
                                                     
                                                     selectizeInput('classe_mapa_cr_t2', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),                                       
													  br(),
							                          div(bookmarkButton(id = "book_mapas_coredes_t2",
							                                               label = "Compartilhe...",
							                                               icon = icon("share-alt"),
							                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                   ),
                                                   mainPanel(
                                                   		leafletOutput("mapa_genero_cr_percent"),
                                                   		br(),
                                                   		p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                          								href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   	)
                                                   )),
                                        		tabPanel(id = "mapa_var_municipios", "Variação (%) no Período Escolhido",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_mapa_cr_t3', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = c(min(df_pre$Ano),max(df_pre$Ano)),
                                                                 step = 1,
                                                                 sep=''),
                                                     
                                                     radioButtons("genero_mapa_cr_t3", "Tipo de Informação:",
                                                                  c("Total" = "total_mapa_radio",
                                                                    "Homens" = "homens_mapa_radio",
                                                                    "Mulheres" = "mulheres_mapa_radio")),
                                                     
                                                     selectizeInput('classe_mapa_cr_t3', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),                                       
													  br(),
							                          div(bookmarkButton(id = "book_mapas_coredes_t3",
							                                               label = "Compartilhe...",
							                                               icon = icon("share-alt"),
							                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                   ),
                                                   mainPanel(
                                                   		h4(textOutput("titulo_mapa_genero_cr_variacao"), align = "center"),
                                                   		leafletOutput("mapa_genero_cr_variacao"),
                                                   		br(),
                                                   		p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                          								href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   	)
                                                   ))
                                        		)),
                                        
                                        
                                        tabPanel(id = "mapas_rf", "Regiões Funcionais",
                                        	tabsetPanel(type = 'tabs',
                                        		tabPanel(id = "mapa_bruto_rf", "População Bruta",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_mapa_rf_t1', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     radioButtons("genero_mapa_rf_t1", "Tipo de Informação:",
                                                                  c("Total" = "total_mapa_radio",
                                                                    "Homens" = "homens_mapa_radio",
                                                                    "Mulheres" = "mulheres_mapa_radio")),
                                                     
                                                     selectizeInput('classe_mapa_rf_t1', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),
                                                     
                                                     sliderInput("sens_mapa_rf_t1", "Sensibilidade dos Círculos do Mapa:", 
                                                                 min=1.5, max=2.5, value=2),
                                                     
													  br(),
							                          div(bookmarkButton(id = "book_mapas_rf_t1",
							                                               label = "Compartilhe...",
							                                               icon = icon("share-alt"),
							                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                   ),
                                                   mainPanel(
                                                   		leafletOutput("mapa_genero_rf"),
                                                   		br(),
                                                   		p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                          								href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   	)
                                                   )),
                                        		tabPanel(id = "mapa_perc_rf", "Percentual (%) nas Regiões Funcionais",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_mapa_rf_t2', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     radioButtons("genero_mapa_rf_t2", "Tipo de Informação:",
                                                                  c("Total" = "total_mapa_radio",
                                                                    "Homens" = "homens_mapa_radio",
                                                                    "Mulheres" = "mulheres_mapa_radio")),
                                                     
                                                     selectizeInput('classe_mapa_rf_t2', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),                                       
													  br(),
							                          div(bookmarkButton(id = "book_mapas_rf_t2",
							                                               label = "Compartilhe...",
							                                               icon = icon("share-alt"),
							                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                   ),
                                                   mainPanel(
                                                   		leafletOutput("mapa_genero_rf_percent"),
                                                   		br(),
                                                   		p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                          								href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   	)
                                                   )),
                                        		tabPanel(id = "mapa_var_rf", "Variação (%) no Período Escolhido",
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_mapa_rf_t3', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = c(min(df_pre$Ano),max(df_pre$Ano)),
                                                                 step = 1,
                                                                 sep=''),
                                                     
                                                     radioButtons("genero_mapa_rf_t3", "Tipo de Informação:",
                                                                  c("Total" = "total_mapa_radio",
                                                                    "Homens" = "homens_mapa_radio",
                                                                    "Mulheres" = "mulheres_mapa_radio")),
                                                     
                                                     selectizeInput('classe_mapa_rf_t3', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),                                       
													  br(),
							                          div(bookmarkButton(id = "book_mapas_rf_t3",
							                                               label = "Compartilhe...",
							                                               icon = icon("share-alt"),
							                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                   ),
                                                   mainPanel(
                                                   		h4(textOutput("titulo_mapa_genero_rf_variacao"), align = "center"),
                                                   		leafletOutput("mapa_genero_rf_variacao"),
                                                   		br(),
                                                   		p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                          								href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   	)
                                                   ))
                                        		))
                                        
                                        
                                        
                                        
                             ),
                             
                             
                             
                             
                             navbarMenu(title = "Representação no estado",
                                        
                                        tabPanel(id = "representacao_municipios", "Municípios",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_tree_mun', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     radioButtons("genero_tree_mun", "Tipo de Informação:",
                                                                  c("Total" = "total_tree_radio_mun",
                                                                    "Homens" = "homens_tree_radio_mun",
                                                                    "Mulheres" = "mulheres_tree_radio_mun")),
                                                     
                                                     selectizeInput('classe_tree_mun', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),
																				   
						  br(),
                          div(bookmarkButton(id = "book_representacao_municipios",
                                               label = "Compartilhe...",
                                               icon = icon("share-alt"),
                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                     
                                                   ),
                                                   mainPanel(
                                                     d3plusOutput("tree_mun"),
													 br(),
										p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                            href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   )
                                                 )),
                                        
                                        
                                        tabPanel(id = "representacao_coredes", "Coredes",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_tree_cr', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     radioButtons("genero_tree_cr", "Tipo de Informação:",
                                                                  c("Total" = "total_tree_radio_cr",
                                                                    "Homens" = "homens_tree_radio_cr",
                                                                    "Mulheres" = "mulheres_tree_radio_cr")),
                                                     
                                                     selectizeInput('classe_tree_cr', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),
																				   
						  br(),
                          div(bookmarkButton(id = "book_representacao_coredes",
                                               label = "Compartilhe...",
                                               icon = icon("share-alt"),
                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                     
                                                   ),
                                                   mainPanel(
                                                     d3plusOutput("tree_cr"),
													 br(),
										p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                            href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   )
                                                 )),
                                        
                                        tabPanel(id = "representacao_rf", "Regiões Funcionais",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     
                                                     sliderInput('ano_tree_rf', 'Ano a ser escolhido', 
                                                                 min = min(df_pre$Ano),
                                                                 max = max(df_pre$Ano),
                                                                 value = max(df_pre$Ano),
                                                                 step = 1,
                                                                 animate = animationOptions(interval = 1500, loop = FALSE), sep=''),
                                                     
                                                     radioButtons("genero_tree_rf", "Tipo de Informação:",
                                                                  c("Total" = "total_tree_radio_rf",
                                                                    "Homens" = "homens_tree_radio_rf",
                                                                    "Mulheres" = "mulheres_tree_radio_rf")),
                                                     
                                                     selectizeInput('classe_tree_rf', 'Selecione uma ou mais Faixas Etárias (em anos)', 
                                                                    sort(as.character(unique(df_pre$Classe))),
                                                                    selected = "Total",
                                                                    
                                                                    options = list(maxItems = 50,
                                                                                   placeholder = 'Selecione uma lista de faixas...')),
																				   
																				   
						  br(),
                          div(bookmarkButton(id = "book_representacao_rf",
                                               label = "Compartilhe...",
                                               icon = icon("share-alt"),
                                               title = "Compartilhe essa informação!"), align = "center")
                                                     
                                                     
                                                   ),
                                                   mainPanel(
                                                     d3plusOutput("tree_rf"),
													 br(),
										p('Fonte dos Dados: ', a(paste('Estimativas Populacionais FEE – Revisão', max(df_pre$Ano)), 
                            href = 'http://www.fee.rs.gov.br/indicadores/populacao/estimativas-populacionais/', target = '_blank'))
                                                   )
                                                 ))),
                             
                             
                             navbarMenu(title = "Download dos Dados",
                                        
                                        tabPanel(id = "download_estimativas", "Estimativas",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     h2("Tabela de Dados"),
                                                     p("Pesquise as informações de seu interesse ao lado."),
                                                     br(),
                                                     p("Se você deseja fazer o ", strong("download"), "da base completa, clique no ícone abaixo."),
                                                     downloadButton('downloadData_est', 'Baixar .csv')
                                                   ),
                                                   
                                                   # Show the plots
                                                   mainPanel(
                                                     dataTableOutput("tabela")
                                                   )
                                                 )),
                                        
                                        
                                        tabPanel(id = "download_projecoes", "Projeções",
                                                 
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     h2("Tabela de Dados"),
                                                     p("Pesquise as informações de seu interesse ao lado."),
                                                     br(),
                                                     p("Se você deseja fazer o ", strong("download"), "da base completa, clique no ícone abaixo."),
                                                     downloadButton('downloadData_proj', 'Baixar .csv')
                                                   ),
                                                   
                                                   mainPanel(
                                                     dataTableOutput("tabela_proj")
                                                   )
                                                 ))
                                        
												)
                             
                             
                             
                             
                             
                             
                             
                  ))})