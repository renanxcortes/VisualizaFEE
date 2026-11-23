# Módulo "Mapas Dinâmicos" (aba Mapas): mapa coroplético/bolhas por município e
# cálculo opcional da autocorrelação espacial (I de Moran).
# Outputs: mapa_final, gauge_moran, mapinha_grafo

mapaUI <- function(id) {
  ns <- NS(id)
  tabPanel("Mapas Dinâmicos",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput(ns('ano_mapa'), 'Ano a ser escolhido', 
                                      min = min(base_crime$Ano),
                                      max = max(base_crime$Ano),
                                      value = max(base_crime$Ano),
                                      step=1,
                                      animate = animationOptions(interval = 1750, loop = TRUE), sep=""),
                          
                          conditionalPanel(condition = ("input.tipo_dado_mapa != 'pop_radio_mapa'"), ns = ns,
                                           selectInput(ns('crime_mapa'), 'Crime a ser escolhido', 
                                                       as.character(unique(base_crime$Crime)),
                                                       selected = "Homicídio Doloso")),
                          
                          radioButtons(ns("tipo_dado_mapa"), "Tipo de Informação:",
                                       c("Número de Ocorrências" = "ocorre_radio_mapa",
                                         "Taxa por 100.000" = "taxa_radio_mapa",
                                         "População" = "pop_radio_mapa")),
                          
                          conditionalPanel(condition = ("input.tipo_dado_mapa == 'pop_radio_mapa'"), ns = ns, 
                                           sliderInput(ns("sens_mapa"), "Sensibilidade do Mapa:", 
                                                       min=1.5, max=2.5, value=1.9)),
                          
                          fluidRow(
                            column(8, checkboxInput(ns("checkbox_moran"), label = "Calcular autocorrelação espacial?", 
                                                    value = FALSE)),
                            column(4, bsButton(ns("q_moran"), label = "Ajuda", icon = icon("info"),
                                   style = "info", size = "medium"),
                          bsPopover(id = ns("q_moran"), title = "Autocorrelação Espacial",
                                    content = paste0("A autocorrelação espacial é uma medida estatística que mede o quanto um município é afetado pelos seus vizinhos e, para isso, é necessário determinar uma estrutura de vizinhança. A medida utilizada é o índice ", strong("I de Moran"), " que varia de -1 até 1. Índices próximos de 1 significam que municípios vizinhos possuem valores similares, já o valor de -1 significa que a relação criminal é inversa, ou seja, altos valores implicam em valor baixos de vizinhos e vice-versa. O valor zero indica ausência de autocorrelação espacial. Para maiores informações veja este ", 
                                                     a("link metodológico", 
                                                       href = "http://www.leg.ufpr.br/lib/exe/fetch.php/disciplinas:cieg:intro-areas.pdf",
                                                       target="_blank"),"."),
                                    placement = "right", 
                                    trigger = "focus", # O 'click' só fechava quando clicava denovo 
                                    options = list(container = "body")
                          ))),
                          
                          conditionalPanel(condition = 'input.checkbox_moran', ns = ns,
                                           selectInput(ns('tipo_estrutura_espacial'), "Tipo de estrutura de vizinhança:", 
                                                       choices = c("Municípios que fazem fronteira", "Municipios mais próximos"))),
                          
                          conditionalPanel(condition = 'input.tipo_estrutura_espacial == "Municipios mais próximos"', ns = ns,
                                           conditionalPanel(condition = 'input.checkbox_moran', ns = ns,
                                           numericInput(ns("n_vizinhos_moran"), "Número de vizinhos:", 2, min = 1, max = 496))
                          )
                          
                        ),
                        
                        # Show the plots
                        mainPanel(
                          leafletOutput(ns("mapa_final")),
                          hr(),
                          conditionalPanel(condition = 'input.checkbox_moran', ns = ns,
                                           fluidRow(column(4, gaugeOutput(ns("gauge_moran")),
										                     bsTooltip(ns("gauge_moran"), title = "Autocorrelação espacial calculada", placement = "top", 
                                                                     trigger = "hover",
                                                                     options = NULL)),
                                                    column(8, plotOutput(ns("mapinha_grafo")))))
                        )
                      ))
}

mapaServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$mapa_final <- renderLeaflet({
      
      ano_mapa   <- input$ano_mapa
      crime_mapa <- input$crime_mapa
      
      df_aux <- base_crime %>%
                filter(Ano == ano_mapa & Crime == crime_mapa) %>%
                mutate(Taxa = Qtd / Populacao * 100000)

      shiny::validate(shiny::need(nrow(df_aux) > 0, "Sem dados disponíveis para este crime neste ano."))

      df_mapa <- merge(mapa_rs, df_aux, by.x = "GEOCODIG_M", by.y = "CodIBGE", all.x = FALSE)
      
      # Este naco e para o mapa de bolhas (latitudes e longitudes)
      am <- df_mapa %>% 
            coordinates() %>% 
            data.frame()
      
      names(am) <- c("Long", "Lat") 
      
      if (input$tipo_dado_mapa == "ocorre_radio_mapa") {
      
      gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Qtd)
      
      leaflet(data = df_mapa) %>% 
  	  addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, # Grossura das borda
                    fillColor = ~gradiente(df_mapa$Qtd),
                    color = "grey", # Cor das bordas
                    fillOpacity = 0.5, # Tranparência
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Nome_Munic, "<br>",
                                   "Pop.: ", df_mapa$Populacao, "<br>",
                                   "Qtd.: ", df_mapa$Qtd, "<br>",
                                   "Tx.: ", round(df_mapa$Taxa,2))) %>% 
        addLegend(position = "bottomright", 
                  pal = gradiente,
                  values = ~Qtd)
      
      } else { 
        if (input$tipo_dado_mapa == "taxa_radio_mapa") {
        
        gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Taxa)

        leaflet(data = df_mapa) %>% 
  		  addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addPolygons(weight = 0.5, # Grossura das borda
                      fillColor = ~gradiente(df_mapa$Taxa), # Weight e a grossura das bordas
                      color = "grey", # Cor das bordas
                      fillOpacity = 0.5, # Tranparência
                      smoothFactor = 0.25,
                      popup = paste0(df_mapa$Nome_Munic, "<br>",
                                     "Pop.: ", df_mapa$Populacao, "<br>",
                                     "Qtd.: ", df_mapa$Qtd, "<br>",
                                     "Tx.: ", round(df_mapa$Taxa,2))) %>% 
          addLegend(position = "bottomright", 
                    pal = gradiente,
                    values = ~Taxa)
      }
      
        else 
        
      leaflet(data = df_mapa) %>% 
        addTiles() %>%
        addCircles(lng = ~am$Long, 
                   lat = ~am$Lat, 
                   weight = 1, 
                   color = "IndianRed",
                   radius = ~ df_mapa$Populacao ^(1/input$sens_mapa) * 30, 
                   popup = paste0(df_mapa$Nome_Munic, "<br>",
                                  "Pop.: ", df_mapa$Populacao, "<br>",
                                  "Qtd.: ", df_mapa$Qtd, "<br>",
                                  "Tx.: ", round(df_mapa$Taxa,2)))
      
      }
      
    })
    
    
    output$gauge_moran = renderGauge({
      
      df_aux_pre <- filter(base_crime, Ano == input$ano_mapa & Crime == input$crime_mapa)
      shiny::validate(shiny::need(nrow(df_aux_pre) > 0, "Sem dados disponíveis para este crime neste ano."))
      df_aux <- mutate(df_aux_pre, Taxa = Qtd / Populacao * 100000)
      df_mapa <- merge(mapa_rs, df_aux, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)

      if (input$tipo_estrutura_espacial == "Municípios que fazem fronteira") {

        nbrsm_pre <- poly2nb(df_mapa, queen = TRUE)
        nbrsm <- nb2listw(nbrsm_pre)

      }

      else {

        nbrsm_pre <- knearneigh(coordinates(df_mapa), k=input$n_vizinhos_moran)
        nbrsm_aux <- knn2nb(nbrsm_pre)
        nbrsm <- nb2listw(nbrsm_aux)

      }

      n <- nrow(df_mapa)
      
      if (input$tipo_dado_mapa == "ocorre_radio_mapa")
        variavel <- df_mapa@data$Qtd 
      else {
        if (input$tipo_dado_mapa == "taxa_radio_mapa") variavel <- df_mapa@data$Taxa
        else variavel <- df_mapa@data$Populacao }
      
      indice_moran <- round(moran(variavel, nbrsm, n, Szero(nbrsm))$I, 3)
      
      gauge(indice_moran, 
            min = -1, 
            max = 1, 
            abbreviateDecimals = 3,
            gaugeSectors(success = c(0.5, 1), warning = c(-0.5, 0.5), danger = c(-1, -0.5)),
            label = "I de Moran")
      
    })
      
      
    output$mapinha_grafo = renderPlot({
        
        df_aux_pre <- filter(base_crime, Ano == input$ano_mapa & Crime == input$crime_mapa)
        shiny::validate(shiny::need(nrow(df_aux_pre) > 0, "Sem dados disponíveis para este crime neste ano."))
        df_aux <- mutate(df_aux_pre, Taxa = Qtd / Populacao * 100000)
        df_mapa <- merge(mapa_rs, df_aux, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)

        if (input$tipo_estrutura_espacial == "Municípios que fazem fronteira") {
          
          nbrsm_pre <- poly2nb(df_mapa, queen = TRUE)
          nbrsm <- nb2listw(nbrsm_pre)
          
        }
        
        else {
          
          nbrsm_pre <- knearneigh(coordinates(df_mapa), k=input$n_vizinhos_moran)
          nbrsm <- knn2nb(nbrsm_pre)
          
        }
        
        plot(df_mapa, border="grey", main="Estrutura de Dependência Espacial")
        plot(nbrsm, coordinates(df_mapa), add=TRUE, cex=0.1)
      })
  })
}
