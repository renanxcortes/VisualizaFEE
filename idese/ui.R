library(shiny)
library(plotly)
library(dplyr)
library(tibble)
library(shinythemes)
library(leaflet)
library(formattable)
library(DT)
require(stringi)
library(collapsibleTree)

options(shiny.sanitize.errors = FALSE)

# setwd("C:/Users/Luis Paese/Desktop/IDESE_app/IDESEVis")
# setwd("~/Downloads/IDESE_app/IDESEVis")
# setwd("/srv/shiny-server/IdeseVis")
#

main_data <- readRDS("IDESE_TOTAL_V0506_1724.rds")
main_data$VALOR[main_data$VALOR == 0] <- NA
main_data <- na.omit(main_data)

mapa_RS_old <- readRDS("mapaRS_old.rds")
mapa_RS_old@data$Nome_Munic <- stri_conv(as.character(mapa_RS_old@data$Nome_Munic), "latin1", "UTF-8")

mapa_RS <- readRDS("MapaRSMunicipios.RDS")
mapa_Cor <- readRDS("MapaRSCoredes.RDS")
mapa_Meso <- readRDS("MapaRSMeso.RDS")
mapa_Micro <- readRDS("MapaRSMicro.RDS")
mapa_RF <- readRDS("MapaRSRF.RDS")

choices_scatter <- c('Idese', 'Bloco Educação', 'Bloco Renda', 'Bloco Saúde')

choice_ts_first <- c('Idese', 'Bloco Educação', 'Bloco Renda', 'Bloco Saúde')

choice_ts_second_idese <- c('Idese', 'Bloco Educação', 'Bloco Renda', 'Bloco Saúde')

choice_ts_second_saude <- c('Bloco Saúde\\Longevidade',
                            'Bloco Saúde\\Condições Gerais de Saúde\\Óbitos por Causas Mal Definidas',
                            'Bloco Saúde\\Condições Gerais de Saúde\\Óbitos por Causas Evitáveis',
                            'Bloco Saúde\\Saúde Materno Infantil\\Consultas Pré Natal',
                            'Bloco Saúde\\Saúde Materno Infantil\\Mortalidade de Menores de 5 anos',
                            'Bloco Saúde\\Condições Gerais de Saúde',
                            'Bloco Saúde\\Saúde Materno Infantil',
                            'Bloco Saúde')

choice_ts_second_educ <- c('Bloco Educação\\Ensino Fundamental\\Anos Iniciais',
                           'Bloco Educação\\Ensino Fundamental\\Anos Finais',
                           'Bloco Educação\\Ensino Fundamental',
                           'Bloco Educação\\Pré Escola',
                           'Bloco Educação\\Ensino Médio',
                           'Bloco Educação\\Escolaridade Adulta',
                           'Bloco Educação')

choice_ts_second_renda <- c('Bloco Renda\\Apropriação da Renda',
                            'Bloco Renda\\Geração da Renda',
                            'Bloco Renda')

municipios_compara <- unique(main_data$NOME[main_data$TIPO_UNID == "Municípios"])

corede_compara <- unique(main_data$NOME[main_data$TIPO_UNID == "Coredes. Def: 2011-atual"])

rf_compara <- unique(main_data$NOME[main_data$TIPO_UNID == "Regiões Funcionais. Def: 2008-atual"])

micro_compara <- unique(main_data$NOME[main_data$TIPO_UNID == "Microrregiões"])

meso_compara <- unique(main_data$NOME[main_data$TIPO_UNID == "Mesorregiões"])

choice_all <- c(choice_ts_second_idese, choice_ts_second_saude, choice_ts_second_educ, choice_ts_second_renda)


name <- ("IdeseVis")
shinyUI(fluidPage(includeCSS("estiloidese.css"), 
	              #includeCSS("collapsibleTree.css"),
				  #tags$head(tags$script(src="d3.min.js")),
				  #tags$head(tags$script(src="collapsibleTree.js")),
				  #tags$head(tags$script(src="htmlwidgets.js")), 

				  htmlOutput("frame"), 


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

theme = shinytheme("cerulean"),
tags$head(tags$script(src="tracking.js")),

tags$head(tags$link(rel="shortcut icon", href="feeicon.ico")),
  tags$div(
    HTML("<script>
 			 (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  			 (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
 			  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
 			  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-1506603-9', 'auto');
  ga('send', 'pageview');

</script>
      ")
),

  navbarPage(name,
             tabPanel("Apresentação",
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
                          h1 (name),
                          p("O aplicativo ",name," é um produto da Fundação de Economia
                            e Estatística (FEE) que apresenta de maneira interativa e dinâmica
                            os dados sobre o Índice de Desenvolvimento Socioeconômico Estado do Rio Grande do 
                            Sul (RS). O Idese avalia a situação socioeconômica dos municípios gaúchos quanto à 
                            Educação, à Renda e à Saúde, considerando aspectos quantitativos e qualitativos do processo de 
                            desenvolvimento, para mais informações sobre a metodologia utilzada para o cálculo do Idese,",
                            a(" clique aqui.", href = "http://www.fee.rs.gov.br/indicadores/indice-de-desenvolvimento-socioeconomico/metodologia/"),
                             "Seus dados brutos podem ser acessados no FEEDADOS neste",
                            a("link", href = "http://feedados.fee.tche.br/feedados/",".")),
                          br(),
                          p("O ", name," foi desenvolvido utilizando a ferramenta gratuíta Shiny, e para
                            uma introdução e outros exemplos, acesse:", a("Shiny homepage", 
                                                                          href = ("http://www.rstudio.com/shiny"))),
                          br(),
                          h2("Características do Aplicativo"),
                          p("* Visualize séries temporais dos indicadores socioeconômicos do Estado com comparações entre
                            os indicadores ou entre as unidades geográficas escolhidas."),
                          p("* Visualize as informações em mapas, de maneira interativa"),
                          p("* Relacione indicadores em gráficos de dispersão em diversas unidades geográficas."),
                          p("* Obtenha Rankings dos indicadores com diversas unidades geográficas"),
                          p("* Realize pesquisas rápidas na base de dados e faça o ",em("download")," dos dados."),
                          br(),
                          h2("Estrutura do Idese"),
                          div(collapsibleTreeOutput("idese_tree_struc"), align = "center"),
                          #collapsibleTreeOutput("idese_tree_struc"),
                          br(),
                          h3("Contato para dúvidas, sugestões ou solicitações:"),
                          p("Renan Xavier Cortes ",
                            a("(CONTATO)", 
                              href = "http://www.fee.rs.gov.br/contato/", target="_blank"),
                           "e Luis Henrique Zanandréa Paese",
                            a("(CONTATO)", 
                                href = "mailto:lhzpaese@gmail.com")
                          ))
                          )
                        ),
  navbarMenu("Séries Temporais",
                        tabPanel("Compara Unidades Geográficas",
                                 h3("Séries Temporais para Comparar Unidades Geográficas", align = "center"),
                                 mainPanel(width = 12,
                                           tabsetPanel(type = 'tabs',
                                                       tabPanel("Estado",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('categoria_idese_est', "Selecione um grupo: ",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = 'Idese'),
                                                                    conditionalPanel(condition = 'input.categoria_idese_est == "Bloco Educação"',
                                                                                     selectInput('subcat_idese_est_educ', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_educ)),
                                                                                                 selected = "Bloco Educação")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_est == "Bloco Renda"',
                                                                                     selectInput('subcat_idese_est_renda', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_renda)),
                                                                                                 selected = "Bloco Renda")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_est == "Bloco Saúde"',
                                                                                     selectInput('subcat_idese_est_saude', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_saude)),
                                                                                                 selected = "Bloco Saúde")),
                                                                    checkboxInput('fixed_scale_est', "Escala fixa entre 0 e 1", value = F)
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_est", height = "100%")
                                                                  )
                                                                )),
                                                       tabPanel("Municípios",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('categoria_idese_mun', "Selecione um grupo: ",
                                                                                as.character(unique(choice_ts_first)),
                                                                                             selected = 'Idese'),
                                                                    conditionalPanel(condition = 'input.categoria_idese_mun == "Bloco Educação"',
                                                                                     selectInput('subcat_idese_mun_educ', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_educ)),
                                                                                                 selected = "Bloco Educação")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_mun == "Bloco Renda"',
                                                                                     selectInput('subcat_idese_mun_renda', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_renda)),
                                                                                                 selected = "Bloco Renda")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_mun == "Bloco Saúde"',
                                                                                     selectInput('subcat_idese_mun_saude', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_saude)),
                                                                                                 selected = "Bloco Saúde")),
                                                                    
                                                                    
                                                                    selectizeInput('idese_municipios','Município:',
                                                                                as.character(unique(municipios_compara)),
                                                                                selected = 'Porto Alegre',
                                                                                options = list(maxItems = 5,
                                                                                               placeholder = 'Selecione uma lista de municípios...')),
                                                                    
                                                                    checkboxInput("checkbox_inclui_rs_ts_idese", label = "Incluir série estadual", value = FALSE),
                                                                    checkboxInput('fixed_scale_mun', "Escala fixa entre 0 e 1", value = F)
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_mun")
                                                                  )
                                                                )),
                                                       tabPanel("Coredes",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('categoria_idese_cor', "Selecione um grupo: ",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = 'Idese'),
                                                                    conditionalPanel(condition = 'input.categoria_idese_cor == "Bloco Educação"',
                                                                                     selectInput('subcat_idese_cor_educ', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_educ)),
                                                                                                 selected = "Bloco Educação")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_cor == "Bloco Renda"',
                                                                                     selectInput('subcat_idese_cor_renda', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_renda)),
                                                                                                 selected = "Bloco Renda")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_cor == "Bloco Saúde"',
                                                                                     selectInput('subcat_idese_cor_saude', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_saude)),
                                                                                                 selected = "Bloco Saúde")),
                                                                    
                                                                    
                                                                    selectizeInput('idese_corede_idese','Corede:',
                                                                                as.character(unique(corede_compara)),
                                                                                selected = 'Metropolitano do Delta do Jacuí',
                                                                                options = list(maxItems = 5,
                                                                                               placeholder = 'Selecione uma lista de Coredes...')),
                                                                    
                                                                    checkboxInput("checkbox_inclui_rs_ts_idese_corede", label = "Incluir série estadual", value = FALSE),
                                                                    checkboxInput('fixed_scale_cor', "Escala fixa entre 0 e 1", value = F)
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_cor")
                                                                  )
                                                                )),
                                                       tabPanel("Regiões Funcionais",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('categoria_idese_rf', "Selecione um grupo: ",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = 'Idese'),
                                                                    conditionalPanel(condition = 'input.categoria_idese_rf == "Bloco Educação"',
                                                                                     selectInput('subcat_idese_rf_educ', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_educ)),
                                                                                                 selected = "Bloco Educação")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_rf == "Bloco Renda"',
                                                                                     selectInput('subcat_idese_rf_renda', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_renda)),
                                                                                                 selected = "Bloco Renda")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_rf == "Bloco Saúde"',
                                                                                     selectInput('subcat_idese_rf_saude', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_saude)),
                                                                                                 selected = "Bloco Saúde")),
                                                                    
                                                                    
                                                                    selectizeInput('idese_rf_idese','Região Funcional:',
                                                                                   as.character(unique(rf_compara)),
                                                                                   selected = 'REGIÃO FUNCIONAL 1',
                                                                                   options = list(maxItems = 5,
                                                                                                  placeholder = 'Selecione uma lista de Regiões Funcionais...')),
                                                                    
                                                                    checkboxInput("checkbox_inclui_rs_ts_idese_rf", label = "Incluir série estadual", value = FALSE),
                                                                    checkboxInput('fixed_scale_rf', "Escala fixa entre 0 e 1", value = F)
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_rf")
                                                                  )
                                                                )),
                                                       tabPanel("Microrregiões",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('categoria_idese_micro', "Selecione um grupo: ",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = 'Idese'),
                                                                    conditionalPanel(condition = 'input.categoria_idese_micro == "Bloco Educação"',
                                                                                     selectInput('subcat_idese_micro_educ', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_educ)),
                                                                                                 selected = "Bloco Educação")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_micro == "Bloco Renda"',
                                                                                     selectInput('subcat_idese_micro_renda', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_renda)),
                                                                                                 selected = "Bloco Renda")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_micro == "Bloco Saúde"',
                                                                                     selectInput('subcat_idese_micro_saude', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_saude)),
                                                                                                 selected = "Bloco Saúde")),
                                                                    
                                                                    selectizeInput('idese_micro','Microrregião:',
                                                                                as.character(unique(micro_compara)),
                                                                                selected = 'Porto Alegre',
                                                                                options = list(maxItems = 5,
                                                                                               placeholder = 'Selecione uma lista de Microrregiões...')),
                                                                    
                                                                    checkboxInput("checkbox_inclui_rs_ts_idese_micro", label = "Incluir série estadual", value = FALSE),
                                                                    checkboxInput('fixed_scale_micro', "Escala fixa entre 0 e 1", value = F)
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_micro")
                                                                  )
                                                                )), 
                                                       tabPanel("Mesorregiões",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('categoria_idese_meso', "Selecione um grupo: ",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = 'Idese'),
                                                                    conditionalPanel(condition = 'input.categoria_idese_meso == "Bloco Educação"',
                                                                                     selectInput('subcat_idese_meso_idese', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_educ)),
                                                                                                 selected = "Bloco Educação")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_meso == "Bloco Renda"',
                                                                                     selectInput('subcat_idese_meso_idese', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_renda)),
                                                                                                 selected = "Bloco Renda")),
                                                                    conditionalPanel(condition = 'input.categoria_idese_meso == "Bloco Saúde"',
                                                                                     selectInput('subcat_idese_meso_idese', "Selecione um subgrupo:",
                                                                                                 choices = as.character(unique(choice_ts_second_saude)),
                                                                                                 selected = "Bloco Saúde")),
                                                                    
                                                                    selectizeInput('idese_meso','Mesorregião:',
                                                                                as.character(unique(meso_compara)),
                                                                                selected = 'Metropolitana de Porto Alegre',
                                                                                options = list(maxItems = 5,
                                                                                               placeholder = 'Selecione uma lista de Mesorregiões...')),
                                                                    
                                                                    checkboxInput("checkbox_inclui_rs_ts_idese_meso", label = "Incluir série estadual", value = FALSE),
                                                                    checkboxInput('fixed_scale_meso', "Escala fixa entre 0 e 1", value = F)
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_meso")
                                                                  )
                                                                ))
                                           )
                                 )),
                        
                        tabPanel("Compara Indicadores",
                                 h3("Séries Temporais para Comparar Indicadores", align = "center"),
                                 mainPanel(width = 12,
                                           tabsetPanel(type = 'tabs',
                                                       tabPanel("Estado",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectizeInput('categoria_idese_est_indi', "Selecione um indicador: ",
                                                                                as.character(unique(choice_all)),
                                                                                selected = 'Idese',
                                                                                options = list(maxItems = 5,
                                                                                               placeholder = 'Selecione uma lista de indicadores...')),
                                                                      checkboxInput('fixed_scale_est_indicadores', "Escala fixa entre 0 e 1", value = F)
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_est_indi")
                                                                  )
                                                                )),
                                                       tabPanel("Municípios",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('idese_mun_compara', "Municipio: ",
                                                                                as.character(unique(municipios_compara)),
                                                                                selected = 'Porto Alegre'),
                                                                    selectizeInput('mun_indicador_compara','Indicador:',
                                                                                   as.character(unique(choice_all)),
                                                                                   selected = 'Idese',
                                                                                   options = list(maxItems = 5,
                                                                                                  placeholder = 'Selecione uma lista de municípios...')),
                                                                    checkboxInput('fixed_scale_mun_indicadores', "Escala fixa entre 0 e 1", value = F),
                                                                    br(),
                                                                    leafletOutput("mapinha_municipios")
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_mun_indi")
                                                                  )
                                                                )),
                                                       tabPanel("Coredes",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('idese_cor_compara', "Corede: ",
                                                                                as.character(unique(corede_compara)),
                                                                                selected = 'Metropolitano do Delta do Jacuí'),
                                                                    selectizeInput('cor_indicador_compara','Indicador:',
                                                                                   as.character(unique(choice_all)),
                                                                                   selected = 'Idese',
                                                                                   options = list(maxItems = 5,
                                                                                                  placeholder = 'Selecione uma lista de municípios...')),
                                                                    checkboxInput('fixed_scale_cor_indicadores', "Escala fixa entre 0 e 1", value = F),
                                                                    br(),
                                                                    leafletOutput("mapinha_coredes")
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_cor_indi")
                                                                  )
                                                                )),
                                                       tabPanel("Regiões Funcionais",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('idese_rf_compara', "Região Funcional: ",
                                                                                as.character(unique(rf_compara)),
                                                                                selected = 'Metropolitano do Delta do Jacuí'),
                                                                    selectizeInput('rf_indicador_compara','Indicador:',
                                                                                   as.character(unique(choice_all)),
                                                                                   selected = 'Idese',
                                                                                   options = list(maxItems = 5,
                                                                                                  placeholder = 'Selecione uma lista de municípios...')),
                                                                    checkboxInput('fixed_scale_rf_indicadores', "Escala fixa entre 0 e 1", value = F),
                                                                    br(),
                                                                    leafletOutput("mapinha_rf")
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_rf_indi")
                                                                  )
                                                                )),
                                                       tabPanel("Microrregiões",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('idese_micro_compara', "Microrregião: ",
                                                                                as.character(unique(micro_compara)),
                                                                                selected = 'Metropolitano do Delta do Jacuí'),
                                                                    selectizeInput('micro_indicador_compara','Indicador:',
                                                                                   as.character(unique(choice_all)),
                                                                                   selected = 'Idese',
                                                                                   options = list(maxItems = 5,
                                                                                                  placeholder = 'Selecione uma lista de municípios...')),
                                                                    checkboxInput('fixed_scale_micro_indicadores', "Escala fixa entre 0 e 1", value = F),
                                                                    br(),
                                                                    leafletOutput("mapinha_micro")
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_micro_indi")
                                                                  )
                                                                )),
                                                       tabPanel("Mesorregiões",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('idese_meso_compara', "Mesorregião: ",
                                                                                as.character(unique(meso_compara)),
                                                                                selected = 'Metropolitano do Delta do Jacuí'),
                                                                    selectizeInput('meso_indicador_compara','Indicador:',
                                                                                   as.character(unique(choice_all)),
                                                                                   selected = 'Idese',
                                                                                   options = list(maxItems = 5,
                                                                                                  placeholder = 'Selecione uma lista de municípios...')),
                                                                    checkboxInput('fixed_scale_meso_indicadores', "Escala fixa entre 0 e 1", value = F),
                                                                    br(),
                                                                    leafletOutput("mapinha_meso")
                                                                    
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("ts_vis_idese_meso_indi")
                                                                  )
                                                                ))
                                           )
                                 ))
                        
                        
                        ),
          tabPanel("Mapas",
                                 h3("Mapas do Idese", align = "center"),
                                 mainPanel(width = 12,
                                           tabsetPanel(type = 'tabs', 
                                                       tabPanel("Municípios",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_mun', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                sep = ''),
                                                                    selectInput('mapa_idese_mun', "Indicador:",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = "Idese")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    leafletOutput("mapa_idese_mun"))
                                                                  )
                                                            ),
                                                       tabPanel("Coredes",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_corede', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                sep = ''),
                                                                    selectInput('mapa_idese_cor', "Indicador:",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = "Idese")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    leafletOutput("mapa_idese_cor"))
                                                                )
                                                       ),
                                                       tabPanel("Regiões Funcionais",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_rf', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                sep = ''),
                                                                    selectInput('mapa_idese_rf', "Indicador:",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = "Idese")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    leafletOutput("mapa_idese_rf"))
                                                                )
                                                       ),
                                                       tabPanel("Microrregiões",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_micro', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                sep = ''),
                                                                    selectInput('mapa_idese_micro', "Indicador:",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = "Idese")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    leafletOutput("mapa_idese_micro"))
                                                                )
                                                       ),
                                                       tabPanel("Mesorregiões",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_meso', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                sep = ''),
                                                                    selectInput('mapa_idese_meso', "Indicador:",
                                                                                as.character(unique(choice_ts_first)),
                                                                                selected = "Idese")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    leafletOutput("mapa_idese_meso"))
                                                                )
                                                       )
                                           
                                 ))          
                ),
        


        tabPanel("Relação entre Indicadores",
                                 h3("Relação entre Indicadores do Idese", align = "center"),
                                 mainPanel(width = 12,
                                           tabsetPanel(type = 'tabs', 
                                                       tabPanel("Municípios",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_mun_scat', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                animate = animationOptions(interval = 1000, loop = FALSE), sep=''),
                                                                    
                                                                    selectInput('mun_var_x', 'Variavel para o eixo horizontal: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Saúde'),
                                                                    selectInput('mun_var_y', 'Variavel para o eixo vertical: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Renda'),
                                                                    selectInput('mun_var_color', 'Variavel para a cor: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Educação'),
                                                                    checkboxInput('fixed_scale_scat_mun', "Escala fixa entre 0 e 1", value = F),
                                                                    p("OBS.: Não é possível realizar essa análise com a repetição de variáveis", style = "color:gray", align = "left")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("scat_idese_mun"))
                                                                )
                                                       ),
                                                       tabPanel("Coredes",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_corede_scat', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                animate = animationOptions(interval = 1000, loop = FALSE), sep=''),
                                                                    
                                                                    selectInput('corede_var_x', 'Variavel para o eixo horizontal: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Saúde'),
                                                                    selectInput('corede_var_y', 'Variavel para o eixo vertical: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Renda'),
                                                                    selectInput('corede_var_color', 'Variavel para a cor: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Educação'),
                                                                    checkboxInput('fixed_scale_scat_cor', "Escala fixa entre 0 e 1", value = F),
                                                                    p("OBS.: Não é possível realizar essa análise com a repetição de variáveis", style = "color:gray", align = "left")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("scat_idese_cor"))
                                                                )
                                                       ),
                                                       tabPanel("Regiões Funcionais",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_rf_scat', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                animate = animationOptions(interval = 1000, loop = FALSE), sep=''),
                                                                    
                                                                    selectInput('rf_var_x', 'Variavel para o eixo horizontal: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Saúde'),
                                                                    selectInput('rf_var_y', 'Variavel para o eixo vertical: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Renda'),
                                                                    selectInput('rf_var_color', 'Variavel para a cor: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Educação'),
                                                                    checkboxInput('fixed_scale_scat_rf', "Escala fixa entre 0 e 1", value = F),
                                                                    p("OBS.: Não é possível realizar essa análise com a repetição de variáveis", style = "color:gray", align = "left")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("scat_idese_rf"))
                                                                )
                                                       ),
                                                       tabPanel("Microrregiões",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_micro_scat', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                animate = animationOptions(interval = 1000, loop = FALSE), sep=''),
                                                                    
                                                                    selectInput('micro_var_x', 'Variavel para o eixo horizontal: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Saúde'),
                                                                    selectInput('micro_var_y', 'Variavel para o eixo vertical: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Renda'),
                                                                    selectInput('micro_var_color', 'Variavel para a cor: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Educação'),
                                                                    checkboxInput('fixed_scale_scat_micro', "Escala fixa entre 0 e 1", value = F),
                                                                    p("OBS.: Não é possível realizar essa análise com a repetição de variáveis", style = "color:gray", align = "left")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("scat_idese_micro"))
                                                                )
                                                       ),
                                                       tabPanel("Mesorregiões",
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    sliderInput('ano_idese_meso_scat', 'Ano:', 
                                                                                min = min(main_data$ANO),
                                                                                max = max(main_data$ANO),
                                                                                value=2014,
                                                                                step=1,
                                                                                animate = animationOptions(interval = 1000, loop = FALSE), sep=''),
                                                                    
                                                                    selectInput('meso_var_x', 'Variavel para o eixo horizontal: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Saúde'),
                                                                    selectInput('meso_var_y', 'Variavel para o eixo vertical: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Renda'),
                                                                    selectInput('meso_var_color', 'Variavel para a cor: ',
                                                                                as.character(unique(choices_scatter)),
                                                                                selected = 'Bloco Educação'),
                                                                    checkboxInput('fixed_scale_scat_meso', "Escala fixa entre 0 e 1", value = F),
                                                                    p("OBS.: Não é possível realizar essa análise com a repetição de variáveis", style = "color:gray", align = "left")
                                                                  ),
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("scat_idese_meso"))
                                                                )
                                                       )
                                           )
                                 )
                        
                        
                        
             ),



      tabPanel("Tabela",
                      h3("Tabela do Idese", align = "center"),
                      mainPanel(width = 12,
                                tabsetPanel(type = 'tabs', 
                                            tabPanel("Municípios",
                                                     sidebarLayout(
                                                       sidebarPanel(
                                                         sliderInput('ano_idese_mun_table', 'Ano:', 
                                                                     min = min(main_data$ANO),
                                                                     max = max(main_data$ANO),
                                                                     value=2014,
                                                                     step=1, 
                                                                     sep=''),
                                                         sliderInput('table_mun_pop_control', "População:",
                                                                     min = 0,
                                                                     max = 1500000,
                                                                     value = range(c(0, 1500000)),
                                                                     step = 12500),
                                                         selectInput("ranking_escolha_mun", "",
                                                                     c("Ranking: Idese" = "escolhe_idese_mun",
                                                                              "Ranking: Educação" = "escolhe_educ_mun",
                                                                              "Ranking: Renda" = "escolhe_renda_mun",
                                                                              "Ranking: Saúde" = "escolhe_saude_mun"
                                                                              
                                                                              ),
                                                                     selected = "escolhe_idese_mun"),
                                                         radioButtons("ranking_table_mun", "Ordenamento dos dados: ",
                                                                      c("Nome dos Municípios" = "ranking_nome_mun",
                                                                        "Idese" = "ranking_idese_mun",
                                                                        "Bloco Educação" = "ranking_educ_mun",
                                                                        "Bloco Renda" = "ranking_renda_mun",
                                                                        "Bloco Saúde" = "ranking_saude_mun"
                                                                        ),
                                                                      selected = "ranking_idese_mun"),
                                                         checkboxInput('inverte_ordem_mun', "Inverter ordenamento da tabela", value = F)
                                                       ),
                                                       mainPanel(
                                                         br(),
                                                         formattableOutput("table_idese_mun", height = "400px"))
                                                     )
                                            ),
                                            tabPanel("Coredes",
                                                     sidebarLayout(
                                                       sidebarPanel(
                                                         sliderInput('ano_idese_corede_table', 'Ano:', 
                                                                     min = min(main_data$ANO),
                                                                     max = max(main_data$ANO),
                                                                     value=2014,
                                                                     step=1,
                                                                     sep=''),
                                                         sliderInput('table_cor_pop_control', "População:",
                                                                     min = 75000,
                                                                     max = 2600000,
                                                                     value = range(c(75000, 2600000)),
                                                                     step = 12500),
                                                         selectInput("ranking_escolha_cor", "",
                                                                     c("Ranking: Idese" = "escolhe_idese_cor",
                                                                       "Ranking: Educação" = "escolhe_educ_cor",
                                                                       "Ranking: Renda" = "escolhe_renda_cor",
                                                                       "Ranking: Saúde" = "escolhe_saude_cor"
                                                                       ),
                                                                     selected = "escolhe_idese_cor"),
                                                         radioButtons("ranking_table_cor", "Ordenamento dos dados: ",
                                                                      c("Nome dos Coredes" = "ranking_nome_cor",
                                                                        "Idese" = "ranking_idese_cor",
                                                                        "Bloco Educação" = "ranking_educ_cor",
                                                                        "Bloco Renda" = "ranking_renda_cor",
                                                                        "Bloco Saúde" = "ranking_saude_cor"),
                                                                      selected = "ranking_idese_cor"),
                                                         checkboxInput('inverte_ordem_corede', "Inverter ordenamento da tabela", value = F)
                                                       ),
                                                       mainPanel(
                                                         br(),
                                                         formattableOutput("table_idese_corede"))
                                                     )
                                            ),
                                            tabPanel("Regiões Funcionais",
                                                     sidebarLayout(
                                                       sidebarPanel(
                                                         sliderInput('ano_idese_rf_table', 'Ano: ', 
                                                                     min = min(main_data$ANO),
                                                                     max = max(main_data$ANO),
                                                                     value=2014,
                                                                     step=1,
                                                                     sep=''),
                                                         sliderInput('table_rf_pop_control', "População:",
                                                                     min = 250000,
                                                                     max = 4750000,
                                                                     value = range(c(250000, 4750000)),
                                                                     step = 12500),
                                                         selectInput("ranking_escolha_rf", "",
                                                                     c("Ranking: Idese" = "escolhe_idese_rf",
                                                                       "Ranking: Educação" = "escolhe_educ_rf",
                                                                       "Ranking: Renda" = "escolhe_renda_rf",
                                                                       "Ranking: Saúde" = "escolhe_saude_rf"),
                                                                     selected = "escolhe_idese_rf"),
                                                         radioButtons("ranking_table_rf", "Ordenamento dos dados: ",
                                                                      c("Nome das Regiões Funcionais" = "ranking_nome_rf",
                                                                        "Idese" = "ranking_idese_rf",
                                                                        "Bloco Educação" = "ranking_educ_rf",
                                                                        "Bloco Renda" = "ranking_renda_rf",
                                                                        "Bloco Saúde" = "ranking_saude_rf"),
                                                                      selected = "ranking_idese_rf"),
                                                         checkboxInput('inverte_ordem_rf', "Inverter ordenamento da tabela", value = F)
                                                       ),
                                                       mainPanel(
                                                         br(),
                                                         formattableOutput("table_idese_rf"))
                                                     )
                                            ),
                                            tabPanel("Microrregiões",
                                                     sidebarLayout(
                                                       sidebarPanel(
                                                         sliderInput('ano_idese_micro_table', 'Ano:', 
                                                                     min = min(main_data$ANO),
                                                                     max = max(main_data$ANO),
                                                                     value=2014,
                                                                     step=1,
                                                                     sep=''),
                                                         sliderInput('table_micro_pop_control', "População:",
                                                                     min = 25000,
                                                                     max = 4000000,
                                                                     value = range(c(25000, 4000000)),
                                                                     step = 12500),
                                                         selectInput("ranking_escolha_micro", "",
                                                                     c("Ranking: Idese" = "escolhe_idese_micro",
                                                                       "Ranking: Educação" = "escolhe_educ_micro",
                                                                       "Ranking: Renda" = "escolhe_renda_micro",
                                                                       "Ranking: Saúde" = "escolhe_saude_micro"),
                                                                     selected = "escolhe_idese_micro"),
                                                         radioButtons("ranking_table_micro", "Ordenamento dos dados: ",
                                                                      c("Nome das Microrregiões" = "ranking_nome_micro",
                                                                        "Idese" = "ranking_idese_micro",
                                                                        "Bloco Educação" = "ranking_educ_micro",
                                                                        "Bloco Renda" = "ranking_renda_micro",
                                                                        "Bloco Saúde" = "ranking_saude_micro"),
                                                                      selected = "ranking_idese_micro"),
                                                         checkboxInput('inverte_ordem_micro', "Inverter ordenamento da tabela", value = F)
                                                       ),
                                                       mainPanel(
                                                         br(),
                                                         formattableOutput("table_idese_micro"))
                                                     )
                                            ),
                                            tabPanel("Mesorregiões",
                                                     sidebarLayout(
                                                       sidebarPanel(
                                                         sliderInput('ano_idese_meso_table', 'Ano:', 
                                                                     min = min(main_data$ANO),
                                                                     max = max(main_data$ANO),
                                                                     value=2014,
                                                                     step=1,
                                                                     sep=''),
                                                         sliderInput('table_meso_pop_control', "População:",
                                                                     min = 500000,
                                                                     max = 5200000,
                                                                     value = range(c(500000, 5200000)),
                                                                     step = 12500),
                                                         selectInput("ranking_escolha_meso", "",
                                                                     c("Ranking: Idese" = "escolhe_idese_meso",
                                                                       "Ranking: Educação" = "escolhe_educ_meso",
                                                                       "Ranking: Renda" = "escolhe_renda_meso",
                                                                       "Ranking: Saúde" = "escolhe_saude_meso"),
                                                                     selected = "escolhe_idese_meso"),
                                                         radioButtons("ranking_table_meso", "Ordenamento dos dados: ",
                                                                      c("Nome das Mesorregiões" = "ranking_nome_meso",
                                                                        "Idese" = "ranking_idese_meso",
                                                                        "Bloco Educação" = "ranking_educ_meso",
                                                                        "Bloco Renda" = "ranking_renda_meso",
                                                                        "Bloco Saúde" = "ranking_saude_meso"),
                                                                      selected = "ranking_idese_meso"),
                                                         checkboxInput('inverte_ordem_meso', "Inverter ordenamento da tabela", value = F)
                                                         
                                                       ),
                                                       mainPanel(
                                                         br(),
                                                         formattableOutput("table_idese_meso", height = "400px"))
                                                     )
                                            )
                                )
                      )
                      
                      
                      
             ),




              



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
                          br(),
                          dataTableOutput("tabela_download")
                        )
                      ))






  )
))
