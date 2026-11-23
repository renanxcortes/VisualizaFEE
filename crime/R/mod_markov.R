# Módulo "Cadeias de Markov" (aba Mapas): transições de presença/ausência de crime
# entre anos, temporal ou espaço-temporal (condicionada aos vizinhos), com
# matrizes, razões de chance e testes de homogeneidade.
# Outputs: mapas_markov, tabela_markov_simples, odds_ratio_simples, tabela_bruta,
#          estatistica_chi, anos_janelas, teste_chi_homog_temporal, evolucao_odds_temporal,
#          tabela_markov_estratificada, odds_ratio_estratificado, teste_chi_homog_spat,
#          evolucao_odds_espaco_temporal

markovUI <- function(id) {
  ns <- NS(id)
  tabPanel("Cadeias de Markov",
                                
                                sidebarLayout(
                                  sidebarPanel(
                                    
                                    selectInput(ns('crime_markov'), 'Crime a ser analisado', 
                                                                 as.character(unique(base_crime$Crime)),
                                                                 selected = "Roubo de Veículos"),
                                    
                                    sliderInput(ns('anos_markov'), "Período de Análise:",
                                                min = min(base_crime$Ano),
                                                max = max(base_crime$Ano),
                                                value = range(c(min(base_crime$Ano), max(base_crime$Ano))),
                                                sep = ""), # Para remover o separador de milhar
                                    
                                    radioButtons(ns("tipo_analise_markov"), "Tipo de Análise Markoviana:",
                                                 c("Temporal" = "radio_matriz_markov",
                                                   "Espaço-Temporal" = "radio_matriz_markov_estratificada")),
												   
									conditionalPanel(condition = "input.tipo_analise_markov == 'radio_matriz_markov'", ns = ns,
                                    checkboxInput(ns("checkbox_inclui_teste_temporal"), label = "Fazer teste de homogeneidade temporal?", value = FALSE)),
                                    
                                    conditionalPanel(condition = "input.tipo_analise_markov == 'radio_matriz_markov'", ns = ns,
									conditionalPanel(condition = "input.checkbox_inclui_teste_temporal", ns = ns,
                                                     sliderInput(ns('anos_markov_janela_1'), "Primeira janela de tempo do teste:",
                                                                 min = min(base_crime$Ano),
                                                                 max = max(base_crime$Ano),
                                                                 value = range(c(min(base_crime$Ano), floor(mean(c(min(base_crime$Ano), max(base_crime$Ano)))))),
                                                                 sep = ""),
                                                     sliderInput(ns('anos_markov_janela_2'), "Segunda janela de tempo do teste:",
                                                                 min = min(base_crime$Ano),
                                                                 max = max(base_crime$Ano),
                                                                 value = range(c(floor(mean(c(min(base_crime$Ano), max(base_crime$Ano)))), max(base_crime$Ano))),
                                                                 sep = ""))),
												   
									checkboxInput(ns("checkbox_inclui_evolucao_instantaneo_markoviana"), label = "Incluir evolução instantânea anual de Odds", value = FALSE),
									
									br(),
												   
									p('Inspirado em Rey, Sergio J., Elizabeth A. Mack, and Julia Koschinsky. ', em('Exploratory space–time analysis of burglary patterns.'), 'Journal of Quantitative Criminology 28.3 (2012): 509-531.')
                                    
                                    
                                  ),
                                  
                                  # Show the plots
                                  mainPanel(plotOutput(ns("mapas_markov")), 
                                  conditionalPanel(condition = ("input.tipo_analise_markov == 'radio_matriz_markov'"), ns = ns,
                                            div(h3("Matriz de Markov de Probabilidades de Transição Temporal"), align = "center"),
                                            div(tableOutput(ns("tabela_markov_simples")), align = "center"),
                                            div(h4("Tabela de Razões de Chance"), align = "center"),
                                            div(tableOutput(ns("odds_ratio_simples")), align = "center"),
                                            br(),
                                            div(h4("Matriz de Frequências e Teste de Independência"), align = "center"),
                                            fluidRow(column(8, tableOutput(ns("tabela_bruta"))),
                                                     div(column(4, h4("Teste Qui-Quadrado"), br(), verbatimTextOutput(ns("estatistica_chi"))), align = "center")),
													 
											conditionalPanel(condition = "input.checkbox_inclui_teste_temporal", ns = ns,
                                            div(h4(textOutput(ns('anos_janelas'))), align = "center"),
                                            div(verbatimTextOutput(ns("teste_chi_homog_temporal")), align = "center")),
													 
										    conditionalPanel(condition = 'input.checkbox_inclui_evolucao_instantaneo_markoviana', ns = ns, br(), br(), plotlyOutput(ns("evolucao_odds_temporal")))),
													 
													 
								  
								  
                                  
                                  conditionalPanel(condition = ("input.tipo_analise_markov == 'radio_matriz_markov_estratificada'"), ns = ns,
                                                   div(h3("Matriz de Markov de Probabilidades de Transição Espaço-Temporal"), align = "center"),
                                                   div(tableOutput(ns("tabela_markov_estratificada")), align = "center"),
                                                   fluidRow(column(8, div(h4("Tabela de Razões de Chance"), align = "center"),
                                                   div(tableOutput(ns("odds_ratio_estratificado")), align = "center")),
                                                   column(4, div(h4("Teste de Homogeneidade Espacial"), align = "center"),
                                                   div(verbatimTextOutput(ns("teste_chi_homog_spat")), align = "center"))),
												   
												   conditionalPanel(condition = 'input.checkbox_inclui_evolucao_instantaneo_markoviana', ns = ns, br(), br(), plotlyOutput(ns("evolucao_odds_espaco_temporal")))
												   
            )
            )
            )
            )
}

markovServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$mapas_markov <- renderPlot({

      anos <- input$anos_markov
      ano_inicial <-anos[1]
      ano_final  <- anos[2]
      crime <- input$crime_markov

      anos_com_dados <- base_crime %>% filter(Crime == crime) %>% pull(Ano) %>% unique()
      shiny::validate(shiny::need(
        ano_inicial %in% anos_com_dados && ano_final %in% anos_com_dados,
        "Um dos anos escolhidos não possui dados para este crime."
      ))

      df_aux_pre_inicial <- filter(base_crime, Ano == ano_inicial & Crime == crime)
      df_aux_pre_final   <- filter(base_crime, Ano == ano_final & Crime == crime)
      df_aux_pre <- filter(base_crime, Ano %in% anos & Crime == crime) %>% 
                    select(CodIBGE, Ano, Qtd) %>% 
                    spread(Ano, Qtd)
      
      names(df_aux_pre)[2] <- "Inicio"
      names(df_aux_pre)[3] <- "Fim"
      
      # 1 - Permaneceu sem crime
      # 2 - Passou a ter crime
      # 3 - Permaneceu com crime
      # 4 - Passou a não ter crime
      cores_T <- c("#b8da6b", "#ff6d00", "#ca0000", "#ffd42a")
      # Cores http://www.ginifab.com/feeds/pms/pms_color_in_image.php
  	
      df_aux <- df_aux_pre %>% 
                mutate(Transicao_Estado = ifelse(Inicio == 0 & Fim == 0, cores_T[1], ifelse(Inicio == 0 & Fim > 0, cores_T[2],ifelse(Inicio > 0 & Fim > 0, cores_T[3], cores_T[4]))))
      
      
      df_mapa_inicial <- merge(mapa_rs, df_aux_pre_inicial, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
      valores_inicial <- df_mapa_inicial@data$Qtd
      cores_inicial <- ifelse(valores_inicial > 0, "darkgrey", NA)
      
      df_mapa_final <- merge(mapa_rs, df_aux_pre_final, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
      valores_final <- df_mapa_final@data$Qtd
      cores_final <- ifelse(valores_final > 0, "darkgrey", NA)
      
      df_mapa_transicao <- merge(mapa_rs, df_aux, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
      cores_transicao <- df_mapa_transicao@data$Transicao_Estado
      
      par(mfrow = c(1,3), mar=c(1,1,2,1), oma=c(0,0,0,0), cex.main = 1.75)
      plot(df_mapa_inicial, lwd = 1.175, col = cores_inicial, border="grey", main = paste0(crime, " em ", ano_inicial))
      plot(df_mapa_final, lwd = 1.175, col = cores_final, border="grey", main = paste0(crime, " em ", ano_final))
      plot(df_mapa_transicao, lwd = 1.175,
           col = cores_transicao,
           border="beige",
           main = "Transição de Estados")
      legend('bottomleft', c("Permaneceu sem crime", "Passou a ter crime", "Permaneceu com crime", "Passou a não ter crime"), 
             fill = cores_T, 
             bg="white", 
             box.col="white", 
             cex = 1.25)
      
      
    })
    
    
    output$tabela_bruta <- renderTable({
      
      anos <- input$anos_markov
      crime <- input$crime_markov

      anos_com_dados <- base_crime %>% filter(Crime == crime) %>% pull(Ano) %>% unique()
      shiny::validate(shiny::need(
        all(anos %in% anos_com_dados),
        "Um dos anos escolhidos não possui dados para este crime."
      ))

      df_aux_pre_markov <- filter(base_crime, Ano %in% anos & Crime == crime) %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov)[4] <- "Inicial"
      names(df_aux_pre_markov)[5] <- "Final"
      
      # Tabela Bruta
      tab_bruta <- table(as.factor(df_aux_pre_markov$Inicial), as.factor(df_aux_pre_markov$Final))
      
      row.names(tab_bruta) <- c("Período Inicial - Sem Ocorrência", "Período Inicial - Com Ocorrência")
      colnames(tab_bruta) <- c("Período Final - Sem Ocorrência", "Período Final - Com Ocorrência")
      
      tab_bruta_pre <- data.frame(tab_bruta) %>%
                       spread(Var2, Freq)
      
      names(tab_bruta_pre)[1] <- ""
      tab_bruta_pre
      
    })
    
    
    output$estatistica_chi <- renderText({
      
      anos <- input$anos_markov
      crime <- input$crime_markov

      anos_com_dados <- base_crime %>% filter(Crime == crime) %>% pull(Ano) %>% unique()
      shiny::validate(shiny::need(
        all(anos %in% anos_com_dados),
        "Um dos anos escolhidos não possui dados para este crime."
      ))

      df_aux_pre_markov <- filter(base_crime, Ano %in% anos & Crime == crime) %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov)[4] <- "Inicial"
      names(df_aux_pre_markov)[5] <- "Final"
      
      # Tabela Bruta
      tab_bruta <- table(as.factor(df_aux_pre_markov$Inicial), as.factor(df_aux_pre_markov$Final))
      
      paste0("Estatística de Teste: ", round(chisq.test(tab_bruta)$statistic, 2), "\n" ,
             "Valor-p: ", round(chisq.test(tab_bruta)$p.value, 8))
      
    })
    
    
    output$tabela_markov_simples <- renderTable({
      
      anos <- input$anos_markov
      crime <- input$crime_markov

      anos_com_dados <- base_crime %>% filter(Crime == crime) %>% pull(Ano) %>% unique()
      shiny::validate(shiny::need(
        all(anos %in% anos_com_dados),
        "Um dos anos escolhidos não possui dados para este crime."
      ))

      df_aux_pre_markov <- filter(base_crime, Ano %in% anos & Crime == crime) %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov)[4] <- "Inicial"
      names(df_aux_pre_markov)[5] <- "Final"
      
      tab_bruta <- table(as.factor(df_aux_pre_markov$Inicial), as.factor(df_aux_pre_markov$Final))
      prop <- prop.table(tab_bruta, 1)# Probabilidades de transição (proporção marginal das linhas) 
      
      row.names(tab_bruta) <- c("Período Inicial - Sem Ocorrência", "Período Inicial - Com Ocorrência")
      colnames(tab_bruta) <- c("Período Final - Sem Ocorrência", "Período Final - Com Ocorrência")
      
      # Ergodico
      erg <- margin.table(tab_bruta, 1)
      ergodico <- c(erg[1]/sum(erg), erg[2]/sum(erg))
      
      # Tabela Final
      tab_markov <- data.frame(rbind(prop, ergodico)) %>% 
  				  mutate(Inicial = c("Período Inicial - Sem Ocorrência", "Período Inicial - Com Ocorrência", "Ergódico")) %>% 
  				  select(Inicial, X0, X1)
      
      row.names(tab_markov) <- c("Período Inicial - Sem Ocorrência", "Período Inicial - Com Ocorrência", "Ergódico")
      colnames(tab_markov) <- c("", "Período Final - Sem Ocorrência", "Período Final - Com Ocorrência")
      
      tab_markov[-3,] # O [-3,] tira a linha de ergódico 
      
    }, digits = 2)
    
    
    output$odds_ratio_simples <- renderTable({
      
      anos <- input$anos_markov
      crime <- input$crime_markov

      anos_com_dados <- base_crime %>% filter(Crime == crime) %>% pull(Ano) %>% unique()
      shiny::validate(shiny::need(
        all(anos %in% anos_com_dados),
        "Um dos anos escolhidos não possui dados para este crime."
      ))

      df_aux_pre_markov <- filter(base_crime, Ano %in% anos & Crime == crime) %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov)[4] <- "Inicial"
      names(df_aux_pre_markov)[5] <- "Final"
      
      tab_bruta <- table(as.factor(df_aux_pre_markov$Inicial), as.factor(df_aux_pre_markov$Final))
      prop <- prop.table(tab_bruta, 1)# Probabilidades de transição (proporção marginal das linhas) 
      
      row.names(tab_bruta) <- c("Período Inicial - Sem Ocorrência", "Período Inicial - Com Ocorrência")
      colnames(tab_bruta) <- c("Período Final - Sem Ocorrência", "Período Final - Com Ocorrência")
      
      # Ergodico
      erg <- margin.table(tab_bruta, 1)
      ergodico <- c(erg[1]/sum(erg), erg[2]/sum(erg))
      
      # Tabela Final
      tab_markov <- data.frame(rbind(prop, ergodico)) %>% mutate(Inicial = c("Período Inicial - Sem Ocorrência", "Período Inicial - Com Ocorrência", "Ergódico")) %>% select(Inicial, X0, X1)
      
      row.names(tab_markov) <- c("Período Inicial - Sem Ocorrência", "Período Inicial - Com Ocorrência", "Ergódico")
      colnames(tab_markov) <- c("", "Período Final - Sem Ocorrência", "Período Final - Com Ocorrência")
      
      aux1 <- c("Chance de transitar para presença de crime", 
                "Chance de transitar para ausência de crime",
                "Chance de permanecer sem crime",
                "Chance de permanecer com crime")
      miolo_markov <- rbind(prop, ergodico)
      aux2 <- c(miolo_markov[1,2]/miolo_markov[1,1], miolo_markov[2,1]/miolo_markov[2,2], miolo_markov[1,1]/miolo_markov[1,2], miolo_markov[2,2]/miolo_markov[2,1])
      tab_odds <- data.frame(Descrição = aux1, Valor = aux2)
      
    }, digits = 2)
    
    
    output$tabela_markov_estratificada <- renderTable({
      
      # Medida que leva em consideracao os vizinhos
      
      anos <- input$anos_markov
      ano_inicial <-anos[1]
      ano_final  <- anos[2]
      crime <- input$crime_markov

      anos_com_dados <- base_crime %>% filter(Crime == crime) %>% pull(Ano) %>% unique()
      shiny::validate(shiny::need(
        ano_inicial %in% anos_com_dados && ano_final %in% anos_com_dados,
        "Um dos anos escolhidos não possui dados para este crime."
      ))

      df_aux_pre_inicial <- filter(base_crime, Ano == ano_inicial & Crime == crime)
      
      df_mapa_inicial <- merge(mapa_rs, df_aux_pre_inicial, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
      
      nbrsm_pre <- poly2nb(df_mapa_inicial, queen = TRUE)
      
      valores_dummies <- data.frame(CodIBGE = df_mapa_inicial@data$GEOCODIG_M, Dummy_Crime = ifelse(df_mapa_inicial@data$Qtd > 0, 1, 0))
      
      # Checa se houve crime nos municípios vizinhos (1 se houve algum crime na vizinhança, 0 caso contrário)
      verifica_vizinhos <- function(x) {if(sum(valores_dummies[x,]$Dummy_Crime) > 0) {1} else {0}}
      
      resultado <- unlist(map(nbrsm_pre, verifica_vizinhos))
      
      mun_estratificados <- data.frame(CodIBGE = valores_dummies$CodIBGE, Estrato = ifelse(resultado == 1, "B", "NB"))
      
      base_crime_estratos <- merge(base_crime, mun_estratificados, by = "CodIBGE")
      
      
      # Matriz de transição NB
      df_aux_pre_markov_NB <- filter(base_crime_estratos, Ano %in% anos & Crime == crime & Estrato == "NB") %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao, -Estrato) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov_NB)[4] <- "Inicial"
      names(df_aux_pre_markov_NB)[5] <- "Final"
      
      # Tabela Bruta
      tab_bruta_NB <- table(as.factor(df_aux_pre_markov_NB$Inicial), as.factor(df_aux_pre_markov_NB$Final))
      prop_NB <- prop.table(tab_bruta_NB, 1) # Probabilidades de transição (proporção marginal das linhas)
      tab_final_NB <- cbind(rowSums(tab_bruta_NB),prop_NB)
      
      
      
      
      
      # Matriz de transição B
      df_aux_pre_markov_B <- filter(base_crime_estratos, Ano %in% anos & Crime == crime & Estrato == "B") %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao, -Estrato) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov_B)[4] <- "Inicial"
      names(df_aux_pre_markov_B)[5] <- "Final"
      
      # Tabela Bruta
      tab_bruta_B <- table(as.factor(df_aux_pre_markov_B$Inicial), as.factor(df_aux_pre_markov_B$Final))
      prop_B <- prop.table(tab_bruta_B, 1) # Probabilidades de transição (proporção marginal das linhas)
      tab_final_B <- cbind(rowSums(tab_bruta_B),prop_B)
      
      # Tabela Final NB + B:
      miolo <- rbind(tab_final_NB, tab_final_B)
      tab_final <- data.frame(Vizinhança = c("Sem crime", "Sem crime", "Com crime", "Com crime"),
                              Inicial = c("Sem crime", "Com crime", "Sem crime", "Com crime"),
                              n = miolo[,1],
                              `Final - Sem crime` = miolo[,2],
                              `Final - Com crime` = miolo[,3])
      names(tab_final)[4:5] <- c("Final - Sem crime", "Final - Com crime")
      tab_final
      
      
    }, digits = 2)
    
    
    output$odds_ratio_estratificado <- renderTable({
      
      # Medida que leva em consideracao os vizinhos
      
      anos <- input$anos_markov
      ano_inicial <-anos[1]
      ano_final  <- anos[2]
      crime <- input$crime_markov

      anos_com_dados <- base_crime %>% filter(Crime == crime) %>% pull(Ano) %>% unique()
      shiny::validate(shiny::need(
        ano_inicial %in% anos_com_dados && ano_final %in% anos_com_dados,
        "Um dos anos escolhidos não possui dados para este crime."
      ))

      df_aux_pre_inicial <- filter(base_crime, Ano == ano_inicial & Crime == crime)
      
      df_mapa_inicial <- merge(mapa_rs, df_aux_pre_inicial, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
      
      nbrsm_pre <- poly2nb(df_mapa_inicial, queen = TRUE)
      
      valores_dummies <- data.frame(CodIBGE = df_mapa_inicial@data$GEOCODIG_M, Dummy_Crime = ifelse(df_mapa_inicial@data$Qtd > 0, 1, 0))
      
      # Checa se houve crime nos municípios vizinhos (1 se houve algum crime na vizinhança, 0 caso contrário)
      verifica_vizinhos <- function(x) {if(sum(valores_dummies[x,]$Dummy_Crime) > 0) {1} else {0}}
      
      resultado <- unlist(map(nbrsm_pre, verifica_vizinhos))
      
      mun_estratificados <- data.frame(CodIBGE = valores_dummies$CodIBGE, Estrato = ifelse(resultado == 1, "B", "NB"))
      
      base_crime_estratos <- merge(base_crime, mun_estratificados, by = "CodIBGE")
      
      
      # Matriz de transição NB
      df_aux_pre_markov_NB <- filter(base_crime_estratos, Ano %in% anos & Crime == crime & Estrato == "NB") %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao, -Estrato) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov_NB)[4] <- "Inicial"
      names(df_aux_pre_markov_NB)[5] <- "Final"
      
      # Tabela Bruta
      tab_bruta_NB <- table(as.factor(df_aux_pre_markov_NB$Inicial), as.factor(df_aux_pre_markov_NB$Final))
      prop_NB <- prop.table(tab_bruta_NB, 1) # Probabilidades de transição (proporção marginal das linhas)
      tab_final_NB <- cbind(rowSums(tab_bruta_NB),prop_NB)
      
      
      
      
      
      # Matriz de transição B
      df_aux_pre_markov_B <- filter(base_crime_estratos, Ano %in% anos & Crime == crime & Estrato == "B") %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao, -Estrato) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov_B)[4] <- "Inicial"
      names(df_aux_pre_markov_B)[5] <- "Final"
      
      # Tabela Bruta
      tab_bruta_B <- table(as.factor(df_aux_pre_markov_B$Inicial), as.factor(df_aux_pre_markov_B$Final))
      prop_B <- prop.table(tab_bruta_B, 1) # Probabilidades de transição (proporção marginal das linhas)
      tab_final_B <- cbind(rowSums(tab_bruta_B),prop_B)
      
      # Tabela Final NB + B:
      miolo <- rbind(tab_final_NB, tab_final_B)
      tab_final <- data.frame(Vizinhança = c("Sem crime", "Sem crime", "Com crime", "Com crime"),
                              Inicial = c("Sem crime", "Com crime", "Sem crime", "Com crime"),
                              n = miolo[,1],
                              `Final - Sem crime` = miolo[,2],
                              `Final - Com crime` = miolo[,3])
      names(tab_final)[4:5] <- c("Final - Sem crime", "Final - Com crime")
      
      miolo_odds <- miolo[,-1]
      
      pi_B_12  <- miolo_odds[3,2]/miolo_odds[3,1]
      pi_NB_12 <- miolo_odds[1,2]/miolo_odds[1,1]
      
      
      pi_B_21  <- miolo_odds[4,1]/miolo_odds[4,2]
      pi_NB_21 <- miolo_odds[2,1]/miolo_odds[2,2]
      
      pi_B_11  <- miolo_odds[3,1]/miolo_odds[3,2]
      pi_NB_11 <- miolo_odds[1,1]/miolo_odds[1,2]
      
      pi_B_22  <- miolo_odds[4,2]/miolo_odds[4,1]
      pi_NB_22 <- miolo_odds[2,2]/miolo_odds[2,1]
      
      theta_12 <- pi_B_12/pi_NB_12
      theta_21 <- pi_B_21/pi_NB_21
      theta_11 <- pi_B_11/pi_NB_11
      theta_22 <- pi_B_22/pi_NB_22
      
      aux1 <- c("Efeito da vizinhança na transição para presença de crime", # vizinhança com crime
                "Efeito da vizinhança na transição para ausência de crime",
                "Efeito da vizinhança em permanecer na ausência de crime",
                "Efeito da vizinhança em permanecer na presença de crime")
      aux2 <- c(theta_12, theta_21, theta_11, theta_22)
      
      tab_odds <- data.frame(Descrição = aux1, Valor = aux2)
      
      
      
    }, digits = 2)
    
    
    output$teste_chi_homog_spat <- renderText({
      
      anos <- input$anos_markov
      ano_inicial <-anos[1]
      ano_final  <- anos[2]
      crime <- input$crime_markov

      anos_com_dados <- base_crime %>% filter(Crime == crime) %>% pull(Ano) %>% unique()
      shiny::validate(shiny::need(
        ano_inicial %in% anos_com_dados && ano_final %in% anos_com_dados,
        "Um dos anos escolhidos não possui dados para este crime."
      ))

      df_aux_pre_inicial <- filter(base_crime, Ano == ano_inicial & Crime == crime)
      
      df_mapa_inicial <- merge(mapa_rs, df_aux_pre_inicial, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
      
      nbrsm_pre <- poly2nb(df_mapa_inicial, queen = TRUE)
      
      valores_dummies <- data.frame(CodIBGE = df_mapa_inicial@data$GEOCODIG_M, Dummy_Crime = ifelse(df_mapa_inicial@data$Qtd > 0, 1, 0))
      
      # Checa se houve crime nos municípios vizinhos (1 se houve algum crime na vizinhança, 0 caso contrário)
      verifica_vizinhos <- function(x) {if(sum(valores_dummies[x,]$Dummy_Crime) > 0) {1} else {0}}
      
      resultado <- unlist(map(nbrsm_pre, verifica_vizinhos))
      
      mun_estratificados <- data.frame(CodIBGE = valores_dummies$CodIBGE, Estrato = ifelse(resultado == 1, "B", "NB"))
      
      base_crime_estratos <- merge(base_crime, mun_estratificados, by = "CodIBGE")
      
      
      # Matriz de transição NB
      df_aux_pre_markov_NB <- filter(base_crime_estratos, Ano %in% anos & Crime == crime & Estrato == "NB") %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao, -Estrato) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov_NB)[4] <- "Inicial"
      names(df_aux_pre_markov_NB)[5] <- "Final"
      
      # Tabela Bruta
      tab_bruta_NB <- table(as.factor(df_aux_pre_markov_NB$Inicial), as.factor(df_aux_pre_markov_NB$Final))

      # Matriz de transição B
      df_aux_pre_markov_B <- filter(base_crime_estratos, Ano %in% anos & Crime == crime & Estrato == "B") %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao, -Estrato) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov_B)[4] <- "Inicial"
      names(df_aux_pre_markov_B)[5] <- "Final"
      
      # Tabela Bruta
      tab_bruta_B <- table(as.factor(df_aux_pre_markov_B$Inicial), as.factor(df_aux_pre_markov_B$Final))

      # Tabela Final NB + B (pg. 520 paper Reis)
      f_sij <- rbind(tab_bruta_NB, tab_bruta_B)
      f_si  <- rowSums(f_sij)
      f_ij  <- matrix(c(f_sij[1,1]+f_sij[3,1], f_sij[1,2]+f_sij[3,2], f_sij[2,1]+f_sij[4,1], f_sij[2,2]+f_sij[4,2]), ncol = 2, byrow = T)
      f_i   <- rowSums(f_ij)
      
      # Detalhe: o f.,i,. é em cima, no numerador, na expressão da fórmula
      est_teste <- 2*(
        f_sij[1,1]*log((f_sij[1,1]/(f_si[1]*f_ij[1,1]))*f_i[1]) + # i = 1, j = 1, s = 1
          f_sij[2,1]*log((f_sij[2,1]/(f_si[2]*f_ij[2,1]))*f_i[2]) + # i = 2, j = 1, s = 1
          f_sij[1,2]*log((f_sij[1,2]/(f_si[1]*f_ij[1,2]))*f_i[1]) + # i = 1, j = 2, s = 1
          f_sij[3,1]*log((f_sij[3,1]/(f_si[3]*f_ij[1,1]))*f_i[1]) + # i = 1, j = 1, s = 2
          
          f_sij[3,2]*log((f_sij[3,2]/(f_si[3]*f_ij[1,2]))*f_i[1]) + # i = 1, j = 2, s = 2
          f_sij[4,1]*log((f_sij[4,1]/(f_si[4]*f_ij[2,1]))*f_i[2]) + # i = 2, j = 1, s = 2
          f_sij[2,2]*log((f_sij[2,2]/(f_si[2]*f_ij[2,2]))*f_i[2]) + # i = 2, j = 2, s = 1
          f_sij[4,2]*log((f_sij[4,2]/(f_si[4]*f_ij[2,2]))*f_i[2]))  # i = 2, j = 2, s = 2
      
      pchisq(est_teste, df = 2, lower.tail = FALSE)
      
      paste0("Estatística de Teste: ", round(est_teste, 2), "\n" ,
             "Valor-p: ", round(pchisq(est_teste, df = 2, lower.tail = FALSE), 4))
      
      
    })
    
    
    output$evolucao_odds_temporal <- renderPlotly({

    crime_escolhido_crimevis <- input$crime_markov

    anos_com_dados <- base_crime %>% filter(Crime == crime_escolhido_crimevis) %>% pull(Ano) %>% unique()
    shiny::validate(shiny::need(
      all(unique(base_crime$Ano) %in% anos_com_dados),
      "Este crime não possui dados para todo o período, então a evolução anual de odds não pode ser calculada."
    ))

     pares_de_anos <- list()
     for(i in 1:(length(unique(base_crime$Ano))-1)) {pares_de_anos[[i]] <- c(min(base_crime$Ano)+i-1, min(base_crime$Ano)+i)}

      calcula_odds_dinamico_temporal <- function(anos_escolhidos, crime_escolhido){
  	  
  	anos <- anos_escolhidos
  	crime <- crime_escolhido

  	df_aux_pre_markov <- filter(base_crime, Ano %in% anos & Crime == crime) %>%
  	  mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
  	  select(-Qtd, -Populacao) %>%
  	  spread(Ano, Dummy_Ocorrencia)

  	names(df_aux_pre_markov)[4] <- "Inicial"
  	names(df_aux_pre_markov)[5] <- "Final"

  	tab_bruta <- table(as.factor(df_aux_pre_markov$Inicial), as.factor(df_aux_pre_markov$Final))
  	prop <- prop.table(tab_bruta, 1)# Probabilidades de transição (proporção marginal das linhas)

  	miolo_markov <- prop
  	odds_temporal <- c(miolo_markov[1,2]/miolo_markov[1,1], miolo_markov[2,1]/miolo_markov[2,2], miolo_markov[1,1]/miolo_markov[1,2], miolo_markov[2,2]/miolo_markov[2,1])

  	}
  	
  	aux <- data.frame(matrix(unlist(map2(pares_de_anos, crime_escolhido_crimevis, calcula_odds_dinamico_temporal)), ncol=length(pares_de_anos), byrow = F)) %>%
         mutate(Tipo_Odds = c("Chance de transitar para presença de crime", 
                              "Chance de transitar para ausência de crime",
                              "Chance de permanecer sem crime",
                              "Chance de permanecer com crime"))

  	names(aux) <- c((min(base_crime$Ano)+1):(max(base_crime$Ano)), "Tipo_Odds")

  	aux2_temp <- aux %>%
  				 gather(Ano, Odds, -Tipo_Odds)



  	plot_ly(aux2_temp, x = ~Ano, y = ~Odds, 
  			type = 'scatter', mode = 'lines', color = ~Tipo_Odds, 
  			hoverinfo="text",
  			text = ~paste0(Tipo_Odds,": ", round(Odds,2), "<br>",
  						   "Ano: ", Ano)) %>%
  	  layout(title = paste0("Efeito instantâneo temporal do ", crime_escolhido_crimevis),
  	         titlefont = list(size = 15),
  			 yaxis = list(title = "Odds Temporal"))
    
    })
    
    
    output$evolucao_odds_espaco_temporal <- renderPlotly({

    crime_escolhido_crimevis <- input$crime_markov

    anos_com_dados <- base_crime %>% filter(Crime == crime_escolhido_crimevis) %>% pull(Ano) %>% unique()
    shiny::validate(shiny::need(
      all(unique(base_crime$Ano) %in% anos_com_dados),
      "Este crime não possui dados para todo o período, então a evolução anual de odds não pode ser calculada."
    ))

    pares_de_anos <- list()
    for(i in 1:(length(unique(base_crime$Ano))-1)) {pares_de_anos[[i]] <- c(min(base_crime$Ano)+i-1, min(base_crime$Ano)+i)}
    
    calcula_odds_dinamico_espaco_temporal <- function(anos_escolhidos, crime_escolhido){
    
    anos <- anos_escolhidos
    ano_inicial <-anos[1]
    ano_final  <- anos[2]
    crime <- crime_escolhido
    
    df_aux_pre_inicial <- filter(base_crime, Ano == ano_inicial & Crime == crime)
    
    df_mapa_inicial <- merge(mapa_rs, df_aux_pre_inicial, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
    
    nbrsm_pre <- poly2nb(df_mapa_inicial, queen = TRUE)
    
    valores_dummies <- data.frame(CodIBGE = df_mapa_inicial@data$GEOCODIG_M, Dummy_Crime = ifelse(df_mapa_inicial@data$Qtd > 0, 1, 0))
    
    # Checa se houve crime nos municípios vizinhos (1 se houve algum crime na vizinhança, 0 caso contrário)
    verifica_vizinhos <- function(x) {if(sum(valores_dummies[x,]$Dummy_Crime) > 0) {1} else {0}}
    
    resultado <- unlist(map(nbrsm_pre, verifica_vizinhos))
    
    mun_estratificados <- data.frame(CodIBGE = valores_dummies$CodIBGE, Estrato = ifelse(resultado == 1, "B", "NB"))
    
    base_crime_estratos <- merge(base_crime, mun_estratificados, by = "CodIBGE")
    
    
    # Matriz de transição NB
    df_aux_pre_markov_NB <- filter(base_crime_estratos, Ano %in% anos & Crime == crime & Estrato == "NB") %>%
      mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
      select(-Qtd, -Populacao, -Estrato) %>%
      spread(Ano, Dummy_Ocorrencia)
    
    names(df_aux_pre_markov_NB)[4] <- "Inicial"
    names(df_aux_pre_markov_NB)[5] <- "Final"
    
    # Tabela Bruta
    tab_bruta_NB <- table(as.factor(df_aux_pre_markov_NB$Inicial), as.factor(df_aux_pre_markov_NB$Final))
    prop_NB <- prop.table(tab_bruta_NB, 1) # Probabilidades de transição (proporção marginal das linhas)
    tab_final_NB <- cbind(rowSums(tab_bruta_NB),prop_NB)
    
    
    
    
    
    # Matriz de transição B
    df_aux_pre_markov_B <- filter(base_crime_estratos, Ano %in% anos & Crime == crime & Estrato == "B") %>%
      mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
      select(-Qtd, -Populacao, -Estrato) %>%
      spread(Ano, Dummy_Ocorrencia)
    
    names(df_aux_pre_markov_B)[4] <- "Inicial"
    names(df_aux_pre_markov_B)[5] <- "Final"
    
    # Tabela Bruta
    tab_bruta_B <- table(as.factor(df_aux_pre_markov_B$Inicial), as.factor(df_aux_pre_markov_B$Final))
    prop_B <- prop.table(tab_bruta_B, 1) # Probabilidades de transição (proporção marginal das linhas)
    tab_final_B <- cbind(rowSums(tab_bruta_B),prop_B)
    
    # Tabela Final NB + B:
    miolo <- rbind(tab_final_NB, tab_final_B)
    tab_final <- data.frame(Vizinhança = c("Sem crime", "Sem crime", "Com crime", "Com crime"),
                            Inicial = c("Sem crime", "Com crime", "Sem crime", "Com crime"),
                            n = miolo[,1],
                            `Final - Sem crime` = miolo[,2],
                            `Final - Com crime` = miolo[,3])
    names(tab_final)[4:5] <- c("Final - Sem crime", "Final - Com crime")
    
    miolo_odds <- miolo[,-1]
    
    pi_B_12  <- miolo_odds[3,2]/miolo_odds[3,1]
    pi_NB_12 <- miolo_odds[1,2]/miolo_odds[1,1]
    
    
    pi_B_21  <- miolo_odds[4,1]/miolo_odds[4,2]
    pi_NB_21 <- miolo_odds[2,1]/miolo_odds[2,2]
    
    pi_B_11  <- miolo_odds[3,1]/miolo_odds[3,2]
    pi_NB_11 <- miolo_odds[1,1]/miolo_odds[1,2]
    
    pi_B_22  <- miolo_odds[4,2]/miolo_odds[4,1]
    pi_NB_22 <- miolo_odds[2,2]/miolo_odds[2,1]
    
    theta_12 <- pi_B_12/pi_NB_12
    theta_21 <- pi_B_21/pi_NB_21
    theta_11 <- pi_B_11/pi_NB_11
    theta_22 <- pi_B_22/pi_NB_22
    
    aux1 <- c("Efeito da vizinhança na transição para presença de crime", # vizinhança com crime
              "Efeito da vizinhança na transição para ausência de crime",
              "Efeito da vizinhança em permanecer na ausência de crime",
              "Efeito da vizinhança em permanecer na presença de crime")
    aux2 <- c(theta_12, theta_21, theta_11, theta_22)
    
    tab_odds <- aux2 # data.frame(Descrição = aux1, Valor = aux2)
    
  }




  aux <- data.frame(matrix(unlist(map2(pares_de_anos, crime_escolhido_crimevis, calcula_odds_dinamico_espaco_temporal)), ncol=length(pares_de_anos), byrow = F)) %>%
         mutate(Efeito_Vizinhanca = c("Transição para presença de crime", # vizinhança com crime
                              "Transição para ausência de crime",
                              "Permanecer na ausência de crime",
                              "Permanecer na presença de crime"))

  names(aux) <- c((min(base_crime$Ano)+1):(max(base_crime$Ano)), "Efeito_Vizinhanca")

  aux2_espaco_temp <- aux %>%
                      gather(Ano, Odds, -Efeito_Vizinhanca)

  plot_ly(aux2_espaco_temp, x = ~Ano, y = ~Odds, 
          type = 'scatter', mode = 'lines', color = ~Efeito_Vizinhanca, 
          hoverinfo="text",
          text = ~paste0(Efeito_Vizinhanca,": ", round(Odds,2), "<br>",
                         "Ano: ", Ano)) %>%
    layout(title = paste0("Efeito instantâneo anual da vizinhança no ", crime_escolhido_crimevis),
           titlefont = list(size = 15),
           yaxis = list(title = "Odds Espaço-Temporal"))
    
    
    
    })

    
    output$teste_chi_homog_temporal <- renderText({
      
      anos_janela_1 <- input$anos_markov_janela_1
      anos_janela_2 <- input$anos_markov_janela_2

      crime <- input$crime_markov

      anos_com_dados <- base_crime %>% filter(Crime == crime) %>% pull(Ano) %>% unique()
      shiny::validate(shiny::need(
        all(c(anos_janela_1, anos_janela_2) %in% anos_com_dados),
        "Um dos anos escolhidos não possui dados para este crime."
      ))

      df_aux_pre_markov_j1 <- filter(base_crime, Ano %in% anos_janela_1 & Crime == crime) %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov_j1)[4] <- "Inicial"
      names(df_aux_pre_markov_j1)[5] <- "Final"
      
      tab_bruta_j1 <- table(as.factor(df_aux_pre_markov_j1$Inicial), as.factor(df_aux_pre_markov_j1$Final))
      
      df_aux_pre_markov_j2 <- filter(base_crime, Ano %in% anos_janela_2 & Crime == crime) %>%
        mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
        select(-Qtd, -Populacao) %>%
        spread(Ano, Dummy_Ocorrencia)
      
      names(df_aux_pre_markov_j2)[4] <- "Inicial"
      names(df_aux_pre_markov_j2)[5] <- "Final"
      
      tab_bruta_j2 <- table(as.factor(df_aux_pre_markov_j2$Inicial), as.factor(df_aux_pre_markov_j2$Final))
      
      # Tabela Final Temporal Adaptada do modelo espacial
      f_sij <- rbind(tab_bruta_j1, tab_bruta_j2)
      f_si  <- rowSums(f_sij)
      f_ij  <- matrix(c(f_sij[1,1]+f_sij[3,1], f_sij[1,2]+f_sij[3,2], f_sij[2,1]+f_sij[4,1], f_sij[2,2]+f_sij[4,2]), ncol = 2, byrow = T)
      f_i   <- rowSums(f_ij)
      
      # Detalhe: o f.,i,. é em cima, no numerador, na expressão da fórmula
      est_teste <- 2*(
        f_sij[1,1]*log((f_sij[1,1]/(f_si[1]*f_ij[1,1]))*f_i[1]) + # i = 1, j = 1, s = 1
          f_sij[2,1]*log((f_sij[2,1]/(f_si[2]*f_ij[2,1]))*f_i[2]) + # i = 2, j = 1, s = 1
          f_sij[1,2]*log((f_sij[1,2]/(f_si[1]*f_ij[1,2]))*f_i[1]) + # i = 1, j = 2, s = 1
          f_sij[3,1]*log((f_sij[3,1]/(f_si[3]*f_ij[1,1]))*f_i[1]) + # i = 1, j = 1, s = 2
          
          f_sij[3,2]*log((f_sij[3,2]/(f_si[3]*f_ij[1,2]))*f_i[1]) + # i = 1, j = 2, s = 2
          f_sij[4,1]*log((f_sij[4,1]/(f_si[4]*f_ij[2,1]))*f_i[2]) + # i = 2, j = 1, s = 2
          f_sij[2,2]*log((f_sij[2,2]/(f_si[2]*f_ij[2,2]))*f_i[2]) + # i = 2, j = 2, s = 1
          f_sij[4,2]*log((f_sij[4,2]/(f_si[4]*f_ij[2,2]))*f_i[2]))  # i = 2, j = 2, s = 2
      
      
      pchisq(est_teste, df = 2, lower.tail = FALSE)
      
      paste0("Estatística de Teste: ", round(est_teste, 2), "\n" ,
             "Valor-p: ", round(pchisq(est_teste, df = 2, lower.tail = FALSE), 4))
      
      
    })
    
    
    output$anos_janelas <- renderText({ paste0(c("Teste de Homogeneidade Temporal entre ", 
                                                 paste0(input$anos_markov_janela_1, collapse = "-"), 
                                                 " e ",  
                                                 paste0(input$anos_markov_janela_2, collapse = "-"))) })
  })
}
