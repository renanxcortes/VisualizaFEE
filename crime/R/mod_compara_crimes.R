# Módulo "Compara Crimes" (menu Séries Temporais): séries de até 5 crimes
# em um município e no estado.
# Outputs: ts_compara_crime_cidades, ts_compara_crime_rs

comparaCrimesUI <- function(id) {
  ns <- NS(id)
  tabPanel("Compara Crimes",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(ns('cidade_compara_crime'), 'Cidade a ser escolhida', 
                                      as.character(unique(base_crime$Mun)),
                                      selected = "Porto Alegre"),
                          
                          selectizeInput(ns('crimes_compara_crimes'), 'Escolha até 5 crimes', 
                                      as.character(unique(base_crime$Crime)),
                                      selected = c("Homicídio Doloso"),
                                      options = list(maxItems = 5,
                                                     placeholder = 'Selecione uma lista de crimes...')),
                          
                          radioButtons(ns("tipo_dado_compara_crime"), "Tipo de Informação:",
                                       c("Número de Ocorrências" = "ocorre_radio_compara_crime",
                                         "Taxa por 100.000" = "taxa_radio_compara_crime"))
                          
                        ),
                        
                        # Show the plots
                        mainPanel(
                          tabsetPanel(type = "tabs", 
                                      tabPanel("Municípios", plotlyOutput(ns("ts_compara_crime_cidades"))), 
                                      tabPanel("Estado", plotlyOutput(ns("ts_compara_crime_rs")))
                          )
                        )
                      ))
}

comparaCrimesServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$ts_compara_crime_cidades <- renderPlotly({
      
      if(length(input$crimes_compara_crimes)==0) return(NULL) # Para não aparecer uma mensagem de erro

    crimes <- input$crimes_compara_crimes
    cidade <- input$cidade_compara_crime

      df_aux_ts <- filter(base_crime, Crime %in% crimes & Mun == cidade) %>%
        mutate(Taxa = round(Qtd/Populacao*100000,2))
      
      if(input$tipo_dado_compara_crime == "ocorre_radio_compara_crime") {
        df_aux_ts2 <- rename(df_aux_ts, Variavel=Qtd)
        y_attr <- list(title = "Nº de Ocorrências")
        nome <- "Qtd.: "
      }
      else {
        df_aux_ts2 <- rename(df_aux_ts, Variavel=Taxa)
        y_attr <- list(title = "Taxa por 100.000", digits=2)
        nome <- "Taxa: "
      }
      
      plot_ly(df_aux_ts2, x = ~Ano, y = ~Variavel, 
              type = 'scatter', mode = 'lines', color = ~Crime, 
              hoverinfo="text", # Para tirar os tool tips pq a formatacao era feia
              text = ~paste0(Crime, "<br>",
                            nome, Variavel, "<br>",
                            "Ano: ", Ano)) %>%
        layout(title = cidade, yaxis = y_attr)
      
      
      
    })
    
    
    output$ts_compara_crime_rs <- renderPlotly({
      
      if(length(input$crimes_compara_crimes)==0) return(NULL)

      crimes <- input$crimes_compara_crimes
      
      df_aux_ts <- filter(base_crime, Crime %in% crimes) # Note que aqui não tem o filtro de Município
      
      df_aux_ts_rs <- df_aux_ts %>%
                      select(Mun, Ano, Crime, Qtd, Populacao) %>%
                      group_by(Ano, Crime) %>% # Quero Varrer os municípios
                      summarize(Qtd = sum(Qtd), Populacao = sum(Populacao)) %>%
                      mutate(Taxa = round(Qtd/Populacao*100000,2)) %>%
                      arrange(Crime, Ano) %>%
                      ungroup() # Note que eu tenho que dar o ungroup depois de tudo, porque ele se perde porque eu agrupei por ano, mas depois eu quero usar o ano como uma variável numérica no eixo x.
      
      if(input$tipo_dado_compara_crime == "ocorre_radio_compara_crime") {
        df_aux_ts_rs2 <- rename(df_aux_ts_rs, Variavel=Qtd)
        y_attr <- list(title = "Nº de Ocorrências")
        nome <- "Qtd.: "
      }
      else {
        df_aux_ts_rs2 <- rename(df_aux_ts_rs, Variavel=Taxa)
        y_attr <- list(title = "Taxa por 100.000", digits=2)
        nome <- "Taxa: "
      }
      
      
      plot_ly(df_aux_ts_rs2, x = ~Ano, y = ~Variavel, 
              type = 'scatter', mode = 'lines', color = ~Crime, 
              hoverinfo="text", # Para tirar os tool tips pq a formatacao era feia
              text = ~paste0(Crime, "<br>",
                             nome, Variavel, "<br>",
                             "Ano: ", Ano)) %>%
        layout(title = "Rio Grande do Sul", yaxis = y_attr)
      
      
      
    })
  })
}
