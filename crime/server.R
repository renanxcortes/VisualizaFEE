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
        layout(title = 'Gráfico de Bolhas',
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
        layout(title = 'Gráfico de Bolhas',
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
      write.csv(datasetInput(), file, fileEncoding = "latin1")
    }
  )
  
  
  
  
  
  output$mapas_markov <- renderPlot({

    anos <- input$anos_markov
    ano_inicial <-anos[1]
    ano_final  <- anos[2]
    crime <- input$crime_markov
    
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
    #cores_T <- c("lightgreen", "red", "black", "blue")
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
         #border="grey", 
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
    
    tab_markov
    
  }, digits = 2)
  
  
  output$odds_ratio_simples <- renderTable({
    
    anos <- input$anos_markov
    crime <- input$crime_markov
    
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
  

    
    

})
