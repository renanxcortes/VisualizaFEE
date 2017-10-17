require(tidyverse)
require(dplyr)
require(tidyr)
require(stringi)

#load("C:/Users/renan/Desktop/Shiny Apps/ProjetoAppCrime/Imagem_AppCrime.Rdata")
#load("C:/Users/Windows 8.1/Desktop/Shiny Apps/ProjetoAppCrime/Imagem_AppCrime.Rdata")
#setwd("C:/Users/Windows 8.1/Desktop/Shiny Apps/ProjetoAppCrime")

#base_crime <- readRDS("BaseCrime_2016.rds") # Está em tibble
base_crime <- readRDS("base_crimevis_2016_pop_ok.rds") # Está em tibble

base_crime$Mun <- stri_conv(as.character(base_crime$Mun), "latin1", "UTF-8")
base_crime$Crime <- stri_conv(as.character(base_crime$Crime), "latin1", "UTF-8")

mapa_rs <- readRDS("MapaRS.rds")
mapa_rs@data$Nome_Munic <- stri_conv(as.character(mapa_rs@data$Nome_Munic), "latin1", "UTF-8")

cods_rmpa <- c(4300604,4300877,4301107,4303103,4303905,4304606,4304689,4305355,4306403,4306767,4307609,4307708,4309050,4309209,4309308,4310108,4310801,4312401,4313060,4313375,4313409,4314050,4314803,4314902,4316006,4317608,4318408,4318705,4319505,4319901,4320008,4321204,4322004,4323002)

options(shiny.sanitize.errors = FALSE)


# Define server logic required
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
  
  output$ts_compara_crime_cidades <- renderPlotly({
    
    if(length(input$crimes_compara_crimes)==0) return(NULL) # Para não aparecer uma mensagem de erro
    
    
	
	crimes <- stri_conv(as.character(input$crimes_compara_crimes), "UTF-8", "latin1")
    cidade <- stri_conv(as.character(input$cidade_compara_crime), "UTF-8", "latin1")

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
    
    crimes <- stri_conv(as.character(input$crimes_compara_crimes), "UTF-8", "latin1")
    
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
    
    
	cidades <- stri_conv(as.character(input$cidades_compara), "UTF-8", "latin1")
    crime <- stri_conv(as.character(input$crime_compara), "UTF-8", "latin1") 
    
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
  
  
#  output$distTaxacompara <- renderDygraph({
#    
#    if(length(input$cidades_compara)==0) return(NULL)
#    
#    df_aux <- filter(base_crime, Crime == as.character(input$crime_compara) & Mun %in% input$cidades_compara)
#    df_aux_aux <- mutate(df_aux, Taxa = Qtd / Populacao * 100000)
#    cc2 <- "Taxa"
#    df <- df_aux_aux[,c("Mun","Ano", cc2)]
#    
#    df_aux2 <- reshape(df, v.names=cc2, idvar="Ano", direction="wide", timevar="Mun")
#    names(df_aux2) <- c("Ano", substring(names(df_aux2[,-1]), first=nchar(cc2)+2)) # O first depende do número de caracteres do nome da variável
#    dygraph(foo, main = input$crime_compara) %>%
#      dyAxis("x", drawGrid = FALSE) %>%
#      dyRangeSelector(height = 20)
#    
#  })
  
  output$dispersao <- renderPlotly({
    
    ano <- input$ano_disp
    slider_pop_min <-input$dispersao_mun_pop_control[1]
    slider_pop_max <- input$dispersao_mun_pop_control[2]
    
    crime_x <- input$crimex
    crime_y <- input$crimey
    
    n_grupos <- input$n_grupos_kmeans
    
    if(input$tipo_dado_disp == "ocorre_radio_disp") {
      varx    <- filter(base_crime, Ano == ano & Crime == crime_x & Populacao > slider_pop_min & Populacao < slider_pop_max)$Qtd
      vary    <- filter(base_crime, Ano == ano & Crime == crime_y & Populacao > slider_pop_min & Populacao < slider_pop_max)$Qtd
    }
    else {
      varx    <- round(filter(base_crime, Ano == ano & Crime == crime_x & Populacao > slider_pop_min & Populacao < slider_pop_max)$Qtd / 
                         filter(base_crime, Ano == ano & Crime == crime_x & Populacao > slider_pop_min & Populacao < slider_pop_max)$Populacao * 100000, 2)
      
      vary    <- round(filter(base_crime, Ano == ano & Crime == crime_y & Populacao > slider_pop_min & Populacao < slider_pop_max)$Qtd / 
                         filter(base_crime, Ano == ano & Crime == crime_y & Populacao > slider_pop_min & Populacao < slider_pop_max)$Populacao * 100000, 2)
    }
    
    muni <- filter(base_crime, Ano == ano & Crime == crime_x & Populacao > slider_pop_min & Populacao < slider_pop_max)$Mun
    pop     <- filter(base_crime, Ano == ano & Crime == crime_y & Populacao > slider_pop_min & Populacao < slider_pop_max)$Populacao
    
    x_attr <- list(title = crime_x, showgrid = TRUE)
    y_attr <- list(title = crime_y, showgrid = TRUE)
    
    dfdf <- data.frame(X = varx, Y = vary, Pop = pop)
    row.names(dfdf) <- muni
    grupos <- kmeans(scale(dfdf[,c("X","Y")]), centers = n_grupos, iter.max = 10, nstart = 1)$cluster
    dfdf$Grupos <- as.factor(paste("Grupo",grupos))
    
    t <- list(size = 10, color = "black")
    
    
    if (!input$checkbox_kmeans) {
      
      plot_ly(dfdf, x = ~X, y = ~Y, type = 'scatter', mode = 'markers', size=~dfdf$Pop,
              hoverinfo="text", # Para tirar os tickmarks dos pontos (tooltips)
              text = paste0(muni, "<br>", 
                           "População: ", dfdf$Pop, "<br>",
                           crime_x, ": ", dfdf$X, "<br>",
                           crime_y, ": ", dfdf$Y),
              marker = list(opacity = 0.5, sizemode = 'diameter'), showlegend = FALSE) %>%
        add_text(text = muni, textfont = t, fill=NULL, 
                 name=paste("Esconder","<br>",
                            "ou mostrar","<br>",
                            "nome do", "<br>",
                            "municipio"), showlegend = TRUE, marker = NULL, visible="legendonly") %>%
        layout(title = 'Gráfico de Dispersão',
               xaxis = x_attr,
               yaxis = y_attr)
      
    }
    else {
      
      plot_ly(dfdf, x = ~X, y = ~Y, type = 'scatter', mode = 'markers', size=~dfdf$Pop, color = ~Grupos,
              hoverinfo="text", # Para tirar os tickmarks dos pontos (tooltips)
              text = paste("", dfdf$Grupo, "<br>",
                           muni, "<br>", 
                           "População :", dfdf$Pop, "<br>",
                           crime_x, ":", dfdf$X, "<br>",
                           crime_y, ":", dfdf$Y),
              marker = list(opacity = 0.5, sizemode = 'diameter'), showlegend = FALSE) %>%
        add_text(text = muni, textfont = t, fill=NULL, 
                 showlegend = TRUE,
                 marker = NULL, visible="legendonly") %>%
        layout(title = 'Gráfico de Dispersão',
               xaxis = x_attr,
               yaxis = y_attr)
      
    }
  })
  
  output$mapa_final <- renderLeaflet({
    
    df_aux_pre <- filter(base_crime, Ano == input$ano_mapa & Crime == input$crime_mapa)
    df_aux <- mutate(df_aux_pre, Taxa = Qtd / Populacao * 100000)
    df_mapa <- merge(mapa_rs, df_aux, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
    
    # Este naco e para o mapa de bolhas. Nao e possivel colocar um loop "for" depois de um condicional 'else'
    aux <- numeric()
    am <- numeric()
    for (i in 1:dim(df_mapa)[1]){
      aux <- df_mapa@polygons[[i]]@labpt
      am <- rbind(am, aux)
    }
    am <- data.frame(am, row.names=NULL)
    names(am) <- c("Long", "Lat") 
    
    
    if (input$tipo_dado_mapa == "ocorre_radio_mapa") {
    
    gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Qtd)
    #gradiente = colorNumeric('Reds', domain = df_mapa$Qtd)
    leaflet(data = df_mapa) %>% 
      #addProviderTiles("CartoDB.Positron") %>%
	  addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Qtd), # Weight e a grossura das bordas
                  color = "grey", fillOpacity = 0.5, # color e a cor das bordas e o fillopacity e tranparencia
                  smoothFactor = 0.25,
                  popup = paste0(df_mapa$Nome_Munic, "<br>",
                                 "Pop.: ", df_mapa$Populacao, "<br>",
                                 "Qtd.: ", df_mapa$Qtd, "<br>",
                                 "Tx.: ", round(df_mapa$Taxa,2))) %>% 
      addLegend(position = "bottomright", pal = gradiente,values = ~Qtd)
    
    } else { 
      if (input$tipo_dado_mapa == "taxa_radio_mapa") {
      
      gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Taxa)
      #gradiente = colorNumeric('Reds', domain = df_mapa$Taxa)
      leaflet(data = df_mapa) %>% 
        #addProviderTiles("CartoDB.Positron") %>%
		addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Taxa), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.5, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Nome_Munic, "<br>",
                                   "Pop.: ", df_mapa$Populacao, "<br>",
                                   "Qtd.: ", df_mapa$Qtd, "<br>",
                                   "Tx.: ", round(df_mapa$Taxa,2))) %>% 
        addLegend(position = "bottomright", pal = gradiente,values = ~Taxa)
    }
    
      else 
      
    leaflet(data = df_mapa) %>% addTiles() %>%
      addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "IndianRed",
                 radius = ~ df_mapa$Populacao ^(1/input$sens_mapa) * 30, popup = paste0(df_mapa$Nome_Munic, "<br>",
                                                                            "Pop.: ", df_mapa$Populacao, "<br>",
                                                                            "Qtd.: ", df_mapa$Qtd, "<br>",
                                                                            "Tx.: ", round(df_mapa$Taxa,2)))
    
    }
    
  })
  
    output$gauge_moran = renderGauge({
    
    df_aux_pre <- filter(base_crime, Ano == input$ano_mapa & Crime == input$crime_mapa)
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
    
    gauge(indice_moran, min = -1, max = 1, 
          gaugeSectors(success = c(0.5, 1), warning = c(-0.5, 0.5), danger = c(-1, -0.5)),
          label = "I de Moran")
    
  })
    
    output$mapinha_grafo = renderPlot({
      
      df_aux_pre <- filter(base_crime, Ano == input$ano_mapa & Crime == input$crime_mapa)
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
  
  output$tree_map <- renderD3plus({
    df_aux <- filter(base_crime, Ano == input$ano_tree & Crime == input$crime_tree)
    if (input$tipo_dado_tree == "ocorre_radio_tree") {
      df <- df_aux[,c(1,5)]
    }
    else {
      if (input$tipo_dado_tree == "pop_radio_tree") df <- df_aux[,c(1,6)] 
      else
      df <- mutate(df_aux, Taxa = Qtd/Populacao * 100000)[,c(1,7)]
      }
      
    d3plus("tree",df)
  })
  
  
  output$tabela = renderDataTable({
    datatable(base_crime, options(list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
      pageLength = 15)))
  })
  
  
    datasetInput <- reactive({
    base_crime
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('base_crime', '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  

    
    

})
