# Módulo "Compara Municípios" (menu Séries Temporais): série de um crime em até
# 5 municípios, com opção de incluir RS ou RMPA.
# Outputs: ts_compara_cidades

comparaMunicipiosUI <- function(id) {
  ns <- NS(id)
  tabPanel("Compara Municípios",
                        
                        sidebarLayout(
                          sidebarPanel(
                            
                            selectizeInput(ns('cidades_compara'), 'Escolha até 5 cidades', 
                                        as.character(unique(base_crime$Mun)),
                                        selected = c("Porto Alegre"),
                                        options = list(maxItems = 5,
                                                       placeholder = 'Selecione uma lista de municípios...')),
                            
                            selectInput(ns('crime_compara'), 'Crime a ser escolhido', 
                                        as.character(unique(base_crime$Crime)),
										selected = "Roubo"),
                            
                            radioButtons(ns("tipo_dado_compara_municipio"), "Tipo de Informação:",
                                         c("Número de Ocorrências" = "ocorre_radio_compara_municipio",
                                           "Taxa por 100.000" = "taxa_radio_compara_municipio")),
                            
                            checkboxInput(ns("checkbox_inclui_rs_rmpa_ts"), label = "Incluir outra série?", value = FALSE),
							
							conditionalPanel(condition = 'input.checkbox_inclui_rs_rmpa_ts', ns = ns,
                            radioButtons(ns("radio_estado_rmpa"), "Qual região?",
                                         c("Rio Grande do Sul" = "radio_rs",
                                           "Região Metropolitana de Porto Alegre (RMPA)" = "radio_rmpa"
                                           ))
                            )
							
                            
                          ),
                          
                          # Show the plots
                          mainPanel(
                            plotlyOutput(ns("ts_compara_cidades"))
                          )
                        ))
}

comparaMunicipiosServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$ts_compara_cidades <- renderPlotly({
      
      if(length(input$cidades_compara)==0) return(NULL) # Para não aparecer uma mensagem de erro
      
      tb1 <- base_crime %>% 
        mutate(Taxa = round(Qtd/Populacao*100000,2)) %>%
        select(Ano, Crime, Qtd, Populacao, Taxa, Mun)
      
      tb2 <- base_crime %>%
        select(Mun, Ano, Crime, Qtd, Populacao) %>%
        group_by(Ano, Crime) %>% # Quero Varrer os municípios
        summarize(Qtd = sum(Qtd), Populacao = sum(Populacao)) %>%
        mutate(Taxa = round(Qtd/Populacao*100000,2)) %>%
        arrange(Crime, Ano) %>%
        ungroup() %>%
        mutate(Mun = "Rio Grande do Sul")
      
      tb3 <- base_crime %>%
        filter(CodIBGE %in% cods_rmpa) %>%
        select(Mun, Ano, Crime, Qtd, Populacao) %>%
        group_by(Ano, Crime) %>% # Quero Varrer os municípios
        summarize(Qtd = sum(Qtd), Populacao = sum(Populacao)) %>%
        mutate(Taxa = round(Qtd/Populacao*100000,2)) %>%
        arrange(Crime, Ano) %>%
        ungroup() %>%
        mutate(Mun = "RMPA")
      
      tb4 <- tb1 %>%
             union(tb2) %>%
             union(tb3) # Cria uma base que o estado e a RMPA é como se fosse um município

      cidades <- input$cidades_compara
      crime <- input$crime_compara
      
      if (!input$checkbox_inclui_rs_rmpa_ts)  {tb_aux <- filter(tb4, Mun %in% cidades, Crime == crime)}
      else
      if (input$radio_estado_rmpa == "radio_rs") {tb_aux <- filter(tb4, Mun %in% c(cidades,"Rio Grande do Sul"), Crime == crime)}
      else
      if (input$radio_estado_rmpa == "radio_rmpa") {tb_aux <- filter(tb4, Mun %in% c(cidades,"RMPA"), Crime == crime)}
      
      
      if(input$tipo_dado_compara_municipio == "ocorre_radio_compara_municipio") {
        tb_aux_ts <- rename(tb_aux, Variavel=Qtd)
        y_attr <- list(title = "Nº de Ocorrências")
        nome <- "Qtd.: "
      }
      else {
        tb_aux_ts <- rename(tb_aux, Variavel=Taxa)
        y_attr <- list(title = "Taxa por 100.000", digits=2)
        nome <- "Taxa: "
      }
      
      tb_aux_ts2 <- tb_aux_ts %>% arrange(Mun, Ano) # Para o eixo x ficar ordenado
      
      plot_ly(tb_aux_ts2, x = ~Ano, y = ~Variavel, 
              type = 'scatter', mode = 'lines', color = ~Mun, 
              hoverinfo="text", # Para tirar os tool tips pq a formatacao era feia
              text = ~paste0(Mun, "<br>",
                             nome, Variavel, "<br>",
                             "Ano: ", Ano)) %>%
        layout(title = crime, yaxis = y_attr)
      
    })
  })
}
