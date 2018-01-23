library(shiny)
library(tidyverse)
library(leaflet.minicharts) # install.packages("leaflet.minicharts")
library(leaflet)
library(collapsibleTree)
library(D3plusR) # devtools::install_github("paulofelipe/D3plusR") # Para atualizar o pacote
library(plotly)
library(shinythemes)
library(stringi)
library(formattable)

options(shiny.sanitize.errors = FALSE)

name <- "PIBVis"


base_pib_mun <- readRDS("base_pib_mun.rds") %>%
  mutate(VAB_OutSer = VAB_Serv - VAB_Apub,
         Pop_PIB = PIB / PIB_pp)

base_pib_mun$Municipio <- stri_conv(as.character(base_pib_mun$Municipio), "latin1", "UTF-8")		 

corresp <- select(readRDS("Corresp_Mun_PopRS.rds"), -Estado, -CodUG, -CodMeso, -Meso, -CodMicro, -Micro, -CodMunicipio)
mapa_mun <- readRDS("MapaRSMunicipios.rds")
mapa_cor <- readRDS("MapaRSCoredes.rds")
mapa_rf <- readRDS("mapaRF.rds")

base_trimestral <- readRDS("base_final_trimestral_TRIM_III_2017_V1.rds") #%>%
                   #filter(AnoTrim != "2017.II")
base_trimestral$TipoDado <- stri_conv(as.character(base_trimestral$TipoDado), "latin1", "UTF-8")
base_trimestral$SeRefere <- stri_conv(as.character(base_trimestral$SeRefere), "latin1", "UTF-8")
base_trimestral$DescVar <- stri_conv(as.character(base_trimestral$DescVar), "latin1", "UTF-8")
base_trimestral$DescVarShort <- stri_conv(as.character(base_trimestral$DescVarShort), "latin1", "UTF-8")
base_trimestral$DescVarTabela <- stri_conv(as.character(base_trimestral$DescVarTabela), "latin1", "UTF-8")


# Funções
retorna_setor <- Vectorize(function(cod_setor) {
  switch(as.character(cod_setor),
         "VAB_Agro" = "VAB da Agropecuária",
         "VAB_Ind" = "VAB da Indústria",
         "VAB_Serv" = "VAB dos Serviços",
         "VAB_Apub" = "VAB da Administração Pública",
         "VAB" = "Valor Adicionado Bruto Total (VAB)",
         "Imp" = "Impostos",
         "PIB" = "Produto Interno Bruto (PIB)",
         "PIB_pp" = "PIB per capita",
         "VAB_OutSer" = "VAB dos Outros Serviços",
         "Pop_PIB" = "População do PIB")
})

setores <- c("VAB da Agropecuária", "VAB da Indústria", "VAB dos Serviços", "VAB da Administração Pública", "Valor Adicionado Bruto Total (VAB)", "Impostos", "Produto Interno Bruto (PIB)", "PIB per capita", "VAB dos Outros Serviços")

base_pib_aux <- inner_join(base_pib_mun, corresp, by = c("CodIBGE", "CodIBGE"))

base_pib_cor <- base_pib_aux %>%
  select(-CodIBGE, -Municipio, -CodIBGE6, -Cidade, -CodRF, -PIB_pp) %>%
  group_by(Ano, CodCorede, Corede) %>%
  summarize(VAB_Agro = sum(VAB_Agro, na.rm = T),
            VAB_Ind = sum(VAB_Ind, na.rm = T),
            VAB_Serv = sum(VAB_Serv, na.rm = T),
            VAB_Apub = sum(VAB_Apub, na.rm = T),
            VAB_OutSer = sum(VAB_OutSer, na.rm = T),
            VAB = sum(VAB, na.rm = T),
            Imp = sum(Imp, na.rm = T),
            PIB = sum(PIB, na.rm = T),
            Pop_PIB = sum(Pop_PIB, na.rm = T)) %>%
  mutate(PIB_pp = ifelse(PIB / Pop_PIB == Inf, NA, PIB / Pop_PIB)) %>%
  ungroup()

base_pib_cor$Corede <- stri_conv(as.character(base_pib_cor$Corede), "latin1", "UTF-8")
  
base_pib_rf <- base_pib_aux %>%
  select(-CodIBGE, -Municipio, -CodIBGE6, -Cidade, -CodCorede, -Corede, -PIB_pp) %>%
  group_by(Ano, CodRF) %>%
  summarize(VAB_Agro = sum(VAB_Agro, na.rm = T),
            VAB_Ind = sum(VAB_Ind, na.rm = T),
            VAB_Serv = sum(VAB_Serv, na.rm = T),
            VAB_Apub = sum(VAB_Apub, na.rm = T),
            VAB_OutSer = sum(VAB_OutSer, na.rm = T),
            VAB = sum(VAB, na.rm = T),
            Imp = sum(Imp, na.rm = T),
            PIB = sum(PIB, na.rm = T),
            Pop_PIB = sum(Pop_PIB, na.rm = T)) %>%
  mutate(PIB_pp = ifelse(PIB / Pop_PIB == Inf, NA, PIB / Pop_PIB)) %>%
  ungroup()


shinyUI(fluidPage(htmlOutput("frame"), theme = shinytheme("cerulean"), includeCSS("estilopib.css"), tags$head(tags$link(rel="shortcut icon", href="feeicon.ico")),

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
  
  navbarPage("PIBVis",
             
             tabPanel("Apresentação",
                      sidebarLayout(
                        sidebarPanel(
                          h2("Fundação de Economia e Estatística"),
                          p("A Fundação de Economia e Estatística(FEE) é uma instituição
                            de pesquisa vinculada à Secretaria de Planejamento do Estado
                            do Rio Grande do Sul"),
                          br(),
                          img(src = "fee_logo.png", height = 32, width = 132), 
                          img(src = "logoGoverno.png", height = 62, width = 92),
                          br(),
                          "Shiny é um produto do",
                          span("Rstudio", style = "color:blue"), ".",
                          br(),
                          br(),
                          img(src = "RStudio_ball.png", height = 82, width = 92)
                          ),
                        mainPanel(
                          h1 (name),
                          p("O aplicativo ",name," é um produto da Fundação de Economia
                            e Estatística (FEE) que apresenta de maneira interativa e dinâmica
                            os dados do Produto Interno Bruto (PIB) do Rio Grande do 
                            Sul (RS), com dados disponibilizados pela FEE.
                            Seus dados brutos podem ser
                            encontrados neste", a("link", href = "http://www.fee.rs.gov.br/indicadores/pib-rs/")),
						  p("Os dados do PIB da FEE são realizados em parceria metodológica com o Instituto Brasileiro de Geografia e Estatística (IBGE). Esta parceria resulta no cálculo e divulgação das Contas Regionais do Rio Grande do Sul e o PIB de seus 497 municípios. Ela também é responsável pelo PIB Trimestral e as  Matrizes de Insumo Produto do Estado."),
                          br(),
						  h2("Características do aplicativo"),
                          p("* Compare todos os setores do PIB Trimestral para o RS e Brasil para todos os trimestres da série histórica."),
						  p("* Compare as variações trimestrais, acumuladas em 4 trimestres e acumuladas no ano."),
                          p("* Visualize a evolução temporal variando a região do RS e Brasil, setores, ajustes sazonais, etc."),
                          p("* Construa uma tabela comparando diferentes tipos de variações."),
						  p("* Faça tabelas comparando as variações regionais com as estaduais."),
                          br(),
						  h2("Estrutura dos Setores do PIB Trimestral"),
                          div(collapsibleTreeOutput("pib_trim_tree_struc"), align = "center"),
						  br(),
						  br(),
						  h3("Contato para dúvidas, sugestões ou solicitações de código:"),
						  p("Roberto Pereira da Rocha ou Renan Xavier Cortes ",
                            a("(CONTATO)", 
                              href = "http://www.fee.rs.gov.br/contato/", target="_blank")),
						  
						  br(),
						  br(),
						  div(img(href = "http://creativecommons.org/licenses/by/4.0/", src="https://i.creativecommons.org/l/by/4.0/88x31.png"), align = "center"),
						  div(p("Este obra está licenciada com uma Licença"), align = "center"),
						  div(a("Creative Commons Atribuição 4.0 Internacional",
                              href = "http://creativecommons.org/licenses/by/4.0/", target="_blank"), align = "center")

                        )
                        )
                        ),
             
             

             
             
             navbarMenu(title = "PIB Trimestral - Gráficos",
                        
                        tabPanel("Resultados Trimestrais",
                                 mainPanel(width = 12,
                                           tabsetPanel(type = 'tabs',
                                                       
                                                       
                                                       tabPanel("Taxas Setoriais", 
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    radioButtons("ajuste_var_compara", "Série com Ajuste Sazonal?",
                                                                                 c("Sem Ajuste" = "radio_ajuste_compara_sem",
                                                                                   "Com Ajuste" = "radio_ajuste_compara_com")),
                                                                    
                                                                    
                                                                    selectInput('ano_trim_var_compara', 'Ano e Trimestre de Análise:', 
                                                                                choices = rev(unique(base_trimestral$AnoTrim)), # Reverso da ordem default, pois ele não ordenava bem characters
                                                                                selected = max(base_trimestral$AnoTrim)),
                                                                    
                                                                    selectInput('setores_var_compara', 'Nível de Desagregação:', 
                                                                                choices = unique(base_trimestral$SeRefere),
                                                                                selected = "PIB"),
                                                                    
                                                                    
                                                                    
                                                                    conditionalPanel(condition = 'input.ajuste_var_compara == "radio_ajuste_compara_sem"',
                                                                                     selectInput('tipo_dado_var_compara1', 'Tipo de Dado:', 
                                                                                                 choices = unique(filter(base_trimestral, Ajuste == "Sem")$TipoDado),
                                                                                                 selected = "Taxa contra mesmo trimestre do ano anterior (%)")),
                                                                    
                                                                    conditionalPanel(condition = 'input.ajuste_var_compara == "radio_ajuste_compara_com"',
                                                                                     selectInput('tipo_dado_var_compara2', 'Tipo de Dado:', 
                                                                                                 choices = unique(filter(base_trimestral, Ajuste == "Com")$TipoDado),
                                                                                                 selected = "Taxa contra trimestre imediatamente anterior (%)")),
                                                                    
                                                                    checkboxInput("checkbox_retira_rotulo_compara", label = "Retirar rótulo de dados", value = FALSE)
                                                                    
                                                                    
                                                                    
                                                                  ),
                                                                  
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("comparacao_trimestral"),
																	textOutput("fonte_ambos"),
                                                                    br(),
                                                                    div(p(strong("Lista de Descrição das Variáveis do Gráfico:")), align = "center"),
                                                                    br(),
                                                                    div(tableOutput('tabela_descricao'), align = "center")
                                                                  )
                                                                )
                                                                
                                                       )
                                                       
                                           )
                                           
                                           
                                 )
                                 
                                 
                        ),
                        
                        
                        
                        tabPanel("Séries de Tempo",
                                 mainPanel(width = 12,
                                           tabsetPanel(type = 'tabs',
                                                       
                                                       
                                                       tabPanel("Comparação RS e Brasil", 
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    radioButtons("ajuste_evolu", "Série com Ajuste Sazonal?",
                                                                                 c("Sem Ajuste" = "radio_ajuste_evolu_sem",
                                                                                   "Com Ajuste" = "radio_ajuste_evolu_com")),
                                                                    
                                                                    
                                                                    selectInput('setor_evolu_rs_br', 'Setor', choices = list(
                                                                      `PIB` = c(`PIB` = 'PIB', 
                                                                                `Agropecuária` = 'Agropecuária', 
                                                                                `Indústria` = 'Indústria', 
                                                                                `Serviços` = 'Serviços', 
                                                                                `Impostos` = 'Impostos', 
                                                                                `Valor Adicionado Bruto` = 'Valor Adicionado Bruto'),
                                                                      Indústria = c(`Construção` = 'Construção', 
                                                                                    `Eletricidade e gás, água, esgoto e limpeza urbana` = 'Eletricidade e gás, água, esgoto e limpeza urbana',
                                                                                    `Indústria Extrativa Mineral` = 'Indústria Extrativa Mineral',
                                                                                    `Indústria de Transformação` = 'Indústria de Transformação'),
                                                                      Serviços =  c(`APU, educação pública e saúde pública` = 'APU, educação pública e saúde pública', 
                                                                                    `Atividades Imobiliárias` = 'Atividades Imobiliárias',
                                                                                    `Comércio` = 'Comércio',
                                                                                    `Intermediação financeira e seguros` = 'Intermediação financeira e seguros',
                                                                                    `Outros Serviços` = 'Outros Serviços',
                                                                                    `Serviços de informação` = 'Serviços de informação',
                                                                                    `Transporte, armazenagem e correio` = 'Transporte, armazenagem e correio')
                                                                    ), selectize = FALSE),
                                                                    
                                                                    conditionalPanel(condition = 'input.ajuste_evolu == "radio_ajuste_evolu_sem"',
                                                                                     selectInput('tipo_dado_evolu1', 'Tipo de Dado:', 
                                                                                                 choices = unique(filter(base_trimestral, Ajuste == "Sem")$TipoDado),
                                                                                                 selected = "Taxa contra mesmo trimestre do ano anterior (%)")),
                                                                    
                                                                    conditionalPanel(condition = 'input.ajuste_evolu == "radio_ajuste_evolu_com"',
                                                                                     selectInput('tipo_dado_evolu2', 'Tipo de Dado:', 
                                                                                                 choices = unique(filter(base_trimestral, Ajuste == "Com")$TipoDado),
                                                                                                 selected = "Taxa contra trimestre imediatamente anterior (%)"))
                                                                    
                                                                  ),
                                                                  
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("evolucao_trimestral"),
																	br(),
																	textOutput("fonte_ambos2")
                                                                  )
                                                                )
                                                                
                                                       ),
                                                       
                                                       tabPanel("Comparação de Setores", 
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    radioButtons("ajuste_evolu_setores", "Série com Ajuste Sazonal?",
                                                                                 c("Sem Ajuste" = "radio_ajuste_evolu_sem_setores",
                                                                                   "Com Ajuste" = "radio_ajuste_evolu_com_setores")),
                                                                    
                                                                    selectInput('regiao_evolu_setores', 'Nacional ou Regional:', 
                                                                                choices = unique(base_trimestral$Local),
                                                                                selected = "Rio Grande do Sul"),
                                                                    
                                                                    
                                                                    selectizeInput('setor_evolu_setores', 'Setor:', choices = list(
                                                                      `PIB` = c(`PIB` = 'PIB', 
                                                                                `Agropecuária` = 'Agropecuária', 
                                                                                `Indústria` = 'Indústria', 
                                                                                `Serviços` = 'Serviços', 
                                                                                `Impostos` = 'Impostos', 
                                                                                `Valor Adicionado Bruto` = 'Valor Adicionado Bruto'),
                                                                      Indústria = c(`Construção` = 'Construção', 
                                                                                    `Eletricidade e gás, água, esgoto e limpeza urbana` = 'Eletricidade e gás, água, esgoto e limpeza urbana',
                                                                                    `Indústria Extrativa Mineral` = 'Indústria Extrativa Mineral',
                                                                                    `Indústria de Transformação` = 'Indústria de Transformação'),
                                                                      Serviços =  c(`APU, educação pública e saúde pública` = 'APU, educação pública e saúde pública', 
                                                                                    `Atividades Imobiliárias` = 'Atividades Imobiliárias',
                                                                                    `Comércio` = 'Comércio',
                                                                                    `Intermediação financeira e seguros` = 'Intermediação financeira e seguros',
                                                                                    `Outros Serviços` = 'Outros Serviços',
                                                                                    `Serviços de informação` = 'Serviços de informação',
                                                                                    `Transporte, armazenagem e correio` = 'Transporte, armazenagem e correio')
                                                                    ), selected = c("PIB","Agropecuária", "Indústria", "Serviços"), multiple = TRUE),
                                                                    
                                                                    conditionalPanel(condition = 'input.ajuste_evolu_setores == "radio_ajuste_evolu_sem_setores"',
                                                                                     selectInput('tipo_dado_evolu1_setores', 'Tipo de Dado:', 
                                                                                                 choices = unique(filter(base_trimestral, Ajuste == "Sem")$TipoDado),
                                                                                                 selected = "Taxa contra mesmo trimestre do ano anterior (%)")),
                                                                    
                                                                    conditionalPanel(condition = 'input.ajuste_evolu_setores == "radio_ajuste_evolu_com_setores"',
                                                                                     selectInput('tipo_dado_evolu2_setores', 'Tipo de Dado:', 
                                                                                                 choices = unique(filter(base_trimestral, Ajuste == "Com")$TipoDado),
                                                                                                 selected = "Taxa contra trimestre imediatamente anterior (%)"))
                                                                    
                                                                  ),
                                                                  
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("evolucao_trimestral_setores"),
																	br(),
																	textOutput("fonte_comparacao_setores")
                                                                  )
                                                                )
                                                                
                                                       ),
                                                       
                                                       tabPanel("Comparação do Ajuste Sazonal", 
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    selectInput('regiao_evolu_ajuste', 'Nacional ou Regional:', 
                                                                                choices = unique(base_trimestral$Local),
                                                                                selected = "Rio Grande do Sul"),

                                                                    
                                                                    selectInput('setor_evolu_rs_br_ajuste', 'Setor', choices = list(
                                                                      `PIB` = c(`PIB` = 'PIB', 
                                                                                `Agropecuária` = 'Agropecuária', 
                                                                                `Indústria` = 'Indústria', 
                                                                                `Serviços` = 'Serviços', 
                                                                                `Impostos` = 'Impostos', 
                                                                                `Valor Adicionado Bruto` = 'Valor Adicionado Bruto'),
                                                                      Indústria = c(`Construção` = 'Construção', 
                                                                                    `Eletricidade e gás, água, esgoto e limpeza urbana` = 'Eletricidade e gás, água, esgoto e limpeza urbana',
                                                                                    `Indústria Extrativa Mineral` = 'Indústria Extrativa Mineral',
                                                                                    `Indústria de Transformação` = 'Indústria de Transformação'),
                                                                      Serviços =  c(`APU, educação pública e saúde pública` = 'APU, educação pública e saúde pública', 
                                                                                    `Atividades Imobiliárias` = 'Atividades Imobiliárias',
                                                                                    `Comércio` = 'Comércio',
                                                                                    `Intermediação financeira e seguros` = 'Intermediação financeira e seguros',
                                                                                    `Outros Serviços` = 'Outros Serviços',
                                                                                    `Serviços de informação` = 'Serviços de informação',
                                                                                    `Transporte, armazenagem e correio` = 'Transporte, armazenagem e correio')
                                                                    ), selectize = FALSE)
                                                                    
                                                                  ),
                                                                  
                                                                  mainPanel(
                                                                    br(),
                                                                    plotlyOutput("evolucao_trimestral_ajuste"),
																	br(),
																	textOutput("fonte_comparacao_ajuste")
                                                                  )
                                                                )
                                                                
                                                       )
                                                       
                                           )
                                           
                                           
                                 )
                                 
                                 
                        )
                                         
            ),

              navbarMenu(title = "PIB Trimestral - Tabelas",
                      tabPanel("Comparações Variações",
                                 mainPanel(width = 12,
                                           tabsetPanel(type = 'tabs',
                                                       
                                                       tabPanel("Comparações Variações", 
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                    
                                                                    radioButtons("ajuste_tabela", "Série com Ajuste Sazonal?",
                                                                                 c("Sem Ajuste" = "radio_tabela_sem_ajuste",
                                                                                   "Com Ajuste" = "radio_tabela_com_ajuste")),

                                                                    selectInput('tabela_local_seletor', 'Seletor de Local:', 
                                                                                choices = (unique(base_trimestral$Local)),
                                                                                selected = max("Rio Grande do Sul")),                                                                    
                                                                    
                                                                    selectInput('ano_tabela_var', 'Ano e Trimestre de Análise:', 
                                                                                choices = rev(unique(base_trimestral$AnoTrim)), # Reverso da ordem default, pois ele não ordenava bem characters
                                                                                selected = max(base_trimestral$AnoTrim))
                                                                    
                                                                    
                                                                    
                                                                  ),
                                                                  
                                                                  mainPanel(
                                                                    br(),
                                                                    div(h3(textOutput("titulo_tabela_pib")), align = "center"),
                                                                    br(),
                                                                    formattableOutput("tabela_brazuca_pib"),
																	br(),
																	br(),
																	textOutput("fonte_tabela_compara_variacoes")
                                                                    )
                                                                )
                                                                
                                                       )
                                                       
                                           )
                                           
                                           
                                 )
                                 
                                 
                        ),

                      tabPanel("Comparações Regionais",
                                 mainPanel(width = 12,
                                           tabsetPanel(type = 'tabs',
                                                       
                                                       tabPanel("Comparações Indicadores", 
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                  
                                                                    selectInput('tabela_indi_seletor_brrs', 'Seletor de Indicador:', 
                                                                                choices = (c("Taxa acumulada em 4 trimestres (%)",
                                                                                             "Taxa acumulada no ano (%)",
                                                                                             "Taxa contra mesmo trimestre do ano anterior (%)",
                                                                                             "Taxa contra trimestre imediatamente anterior (%)")),
                                                                                selected = "Taxa acumulada em 4 trimestres (%)"),                                                                    
                                                                    
                                                                    selectInput('ano_tabela_var_brrs', 'Ano e Trimestre de Análise:', 
                                                                                choices = rev(unique(base_trimestral$AnoTrim)), # Reverso da ordem default, pois ele não ordenava bem characters
                                                                                selected = max(base_trimestral$AnoTrim))
                                                                    
                                                                    
                                                                    
                                                                  ),
                                                                  
                                                                  mainPanel(
                                                                    br(),
                                                                    div(h3(textOutput("titulo_tabela_pib_brrs")), align = "center"),
                                                                    br(),
                                                                    formattableOutput("tabela_brazuca_pib_brrs"),
																	br(),
																	br(),
																	textOutput("fonte_ambos3")
                                                                    )
                                                                )
                                                                
                                                       )
                                                       
                                           )
                                           
                                           
                                 )
                                 
                                 
                        ),
						
						
						tabPanel("Comparações Temporais",
                                 mainPanel(width = 12,
                                           tabsetPanel(type = 'tabs',
                                                       
                                                       tabPanel("Comparações Temporais", 
                                                                sidebarLayout(
                                                                  sidebarPanel(
                                                                  
                                                                    selectInput('tabela_temporal_local', 'Seletor de Local:', 
                                                                                choices = (unique(base_trimestral$Local)),
                                                                                selected = max("Rio Grande do Sul")),                                                                    
                                                                    
                                                                    selectInput('setor_tabela_temporal', 'Setor', choices = list(
                                                                      `PIB` = c(`PIB` = 'PIB', 
                                                                                `Agropecuária` = 'Agropecuária', 
                                                                                `Indústria` = 'Indústria', 
                                                                                `Serviços` = 'Serviços', 
                                                                                `Impostos` = 'Impostos', 
                                                                                `Valor Adicionado Bruto` = 'Valor Adicionado Bruto'),
                                                                      Indústria = c(`Construção` = 'Construção', 
                                                                                    `Eletricidade e gás, água, esgoto e limpeza urbana` = 'Eletricidade e gás, água, esgoto e limpeza urbana',
                                                                                    `Indústria Extrativa Mineral` = 'Indústria Extrativa Mineral',
                                                                                    `Indústria de Transformação` = 'Indústria de Transformação'),
                                                                      Serviços =  c(`APU, educação pública e saúde pública` = 'APU, educação pública e saúde pública', 
                                                                                    `Atividades Imobiliárias` = 'Atividades Imobiliárias',
                                                                                    `Comércio` = 'Comércio',
                                                                                    `Intermediação financeira e seguros` = 'Intermediação financeira e seguros',
                                                                                    `Outros Serviços` = 'Outros Serviços',
                                                                                    `Serviços de informação` = 'Serviços de informação',
                                                                                    `Transporte, armazenagem e correio` = 'Transporte, armazenagem e correio')
                                                                    ), selectize = FALSE)
                                                                    
                                                                    
                                                                    
                                                                  ),
                                                                  
                                                                  mainPanel(
                                                                    br(),
                                                                    div(h3(textOutput("titulo_tabela_temporal")), align = "center"),
                                                                    br(),
                                                                    formattableOutput("tabela_temporal_pib_trim"),
																	br(),
																	br(),
																	textOutput("fonte_tabela_temporal")
                                                                    )
                                                                )
                                                                
                                                       )
                                                       
                                           )
                                           
                                           
                                 )
                                 
                                 
                        )



               )
             
             
  )
  
)
)
