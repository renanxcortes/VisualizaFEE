##########
# Server #
##########

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
    
    
	# Conversão de strings para Shiny server
	#crimes <- stri_conv(as.character(input$crimes_compara_crimes), "UTF-8", "latin1")
  #cidade <- stri_conv(as.character(input$cidade_compara_crime), "UTF-8", "latin1")
  
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
    
    # Conversão de strings para Shiny server
    #crimes <- stri_conv(as.character(input$crimes_compara_crimes), "UTF-8", "latin1")
    
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
    
    # Conversão de string para Shiny server
	  # cidades <- stri_conv(as.character(input$cidades_compara), "UTF-8", "latin1")
    # crime <- stri_conv(as.character(input$crime_compara), "UTF-8", "latin1")
    
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
  
  
  output$dispersao <- renderPlotly({
    
    ano <- input$ano_disp
    slider_pop_min <-input$dispersao_mun_pop_control[1]
    slider_pop_max <- input$dispersao_mun_pop_control[2]
    
    crime_x <- input$crimex
    crime_y <- input$crimey
    
    n_grupos <- input$n_grupos_kmeans
    
    if(input$tipo_dado_disp == "ocorre_radio_disp") {
      base_aux <- base_crime %>%
        filter(Ano == ano, Crime %in% c(crime_x, crime_y), Populacao > slider_pop_min & Populacao < slider_pop_max) %>%
        mutate(Tx = Qtd/Populacao * 100000) %>%
        select(-Tx) %>%
        spread(Crime, Qtd)
      names(base_aux)[which(names(base_aux) == crime_x)] <- "X"
      names(base_aux)[which(names(base_aux) == crime_y)] <- "Y"
    }
    else {
      base_aux <- base_crime %>%
        filter(Ano == ano, Crime %in% c(crime_x, crime_y), Populacao > slider_pop_min & Populacao < slider_pop_max) %>%
        mutate(Tx = Qtd/Populacao * 100000) %>%
        select(-Qtd) %>%
        spread(Crime, Tx)
      names(base_aux)[which(names(base_aux) == crime_x)] <- "X"
      names(base_aux)[which(names(base_aux) == crime_y)] <- "Y"
    }
    
    x_attr <- list(title = crime_x, showgrid = TRUE)
    y_attr <- list(title = crime_y, showgrid = TRUE)
    
    grupos <- kmeans(scale(base_aux[,c("X","Y")]), centers = n_grupos, iter.max = 10, nstart = 1)$cluster
    base_aux$Grupos <- as.factor(paste("Grupo",grupos))
    
    t <- list(size = 10, color = "black")
    
    
    if (!input$checkbox_kmeans) {
      
      plot_ly(base_aux, 
              x = ~X, 
              y = ~Y, 
              type = 'scatter', 
              mode = 'markers', 
              size = ~base_aux$Populacao,
              hoverinfo = "text", # Para tirar os tickmarks dos pontos (tooltips)
              text = paste0(base_aux$Mun, "<br>", 
                            "População: ", base_aux$Populacao, "<br>",
                            crime_x, ": ", base_aux$X, "<br>",
                            crime_y, ": ", base_aux$Y),
              marker = list(opacity = 0.5, 
                            sizemode = 'diameter'), 
              showlegend = FALSE) %>%
        add_text(text = base_aux$Mun, textfont = t, fill=NULL, 
                 name=paste("Esconder","<br>",
                            "ou mostrar","<br>",
                            "nome das", "<br>",
                            "cidades"), 
                 showlegend = TRUE, 
                 marker = NULL, 
                 visible = "legendonly") %>%
        layout(title = 'Gráfico de Bolhas',
               xaxis = x_attr,
               yaxis = y_attr,
               showlegend = TRUE)
        # Adaptação dos argumentos de showlegend para versão mais recente do plotly
      
    }
    else {
      
      plot_ly(base_aux, 
              x = ~X, 
              y = ~Y, 
              type = 'scatter', 
              mode = 'markers', 
              size = ~base_aux$Populacao, 
              color = ~Grupos,
              hoverinfo = "text", # Para tirar os tickmarks dos pontos (tooltips)
              text = paste("", base_aux$Grupos, "<br>",
                           base_aux$Mun, "<br>", 
                           "Population :", base_aux$Populacao, "<br>",
                           crime_x, ":", base_aux$X, "<br>",
                           crime_y, ":", base_aux$Y),
              marker = list(opacity = 0.5, 
                            sizemode = 'diameter'), 
              showlegend = FALSE) %>%
        add_text(text = base_aux$Mun, 
                 textfont = t, 
                 fill = NULL,
                 showlegend = TRUE,
                 marker = NULL, 
                 visible = "legendonly") %>%
        layout(title = 'Bubble Plot',
               xaxis = x_attr,
               yaxis = y_attr)
      
    }
  })
  
  
  output$mapa_final <- renderLeaflet({
    
    ano_mapa   <- input$ano_mapa
    crime_mapa <- input$crime_mapa
    
    df_aux <- base_crime %>% 
              filter(Ano == ano_mapa & Crime == crime_mapa) %>%
              mutate(Taxa = Qtd / Populacao * 100000)
    
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
    
  
  output$tree_map_antigo <- renderD3plus({
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
  
  
  output$tree_map <- renderD3plus({
    
    ano_tree <- input$ano_tree
    crime_tree <- input$crime_tree
    
    df_aux <- base_crime %>% filter(Ano == ano_tree & Crime == crime_tree)
    
    if (input$tipo_dado_tree == "ocorre_radio_tree") {
      
      df <- df_aux %>% 
        select(Mun, Qtd) %>% 
        rename('Ocorrências' = Qtd)
      
      d3plus(data = df,
             type = "tree_map",
             id = c('Mun'),
             width = "100%",
             locale = "pt_BR",
             clean_previous = TRUE) %>%
        d3plusSize("Ocorrências")
      
    }
    else {
      if (input$tipo_dado_tree == "pop_radio_tree") {
      df <- df_aux %>% 
            select(Mun, Populacao) %>%
            rename('População' = Populacao)
      
      d3plus(data = df,
             type = "tree_map",
             id = c('Mun'),
             width = "100%",
             locale = "pt_BR",
             clean_previous = TRUE) %>%
        d3plusSize("População")
      }
      else {
        df <- df_aux %>% 
              mutate(Taxa = Qtd/Populacao * 100000) %>%
              select(Mun, Taxa)
        
        d3plus(data = df,
               type = "tree_map",
               id = c('Mun'),
               width = "100%",
               locale = "pt_BR",
               clean_previous = TRUE) %>%
          d3plusSize("Taxa")}
    }
    
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
  
  
  output$teste_chi_homog_spat <- renderText({
    
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
    #prop_NB <- prop.table(tab_bruta_NB, 1) # Probabilidades de transição (proporção marginal das linhas)
    #tab_final_NB <- cbind(rowSums(tab_bruta_NB),prop_NB)
    
    
    
    
    
    # Matriz de transição B
    df_aux_pre_markov_B <- filter(base_crime_estratos, Ano %in% anos & Crime == crime & Estrato == "B") %>%
      mutate(Dummy_Ocorrencia = ifelse(Qtd > 0, 1, 0)) %>%
      select(-Qtd, -Populacao, -Estrato) %>%
      spread(Ano, Dummy_Ocorrencia)
    
    names(df_aux_pre_markov_B)[4] <- "Inicial"
    names(df_aux_pre_markov_B)[5] <- "Final"
    
    # Tabela Bruta
    tab_bruta_B <- table(as.factor(df_aux_pre_markov_B$Inicial), as.factor(df_aux_pre_markov_B$Final))
    #prop_B <- prop.table(tab_bruta_B, 1) # Probabilidades de transição (proporção marginal das linhas)
    #tab_final_B <- cbind(rowSums(tab_bruta_B),prop_B)
    
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
  #anos_escolhidos_crimevis <- input$anos_markov
  
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
			 yaxis = list(title = "Odds Temporal")#,
			 #legend = list(orientation = 'h')
			 )
  
  })
  
  
  output$evolucao_odds_espaco_temporal <- renderPlotly({
  
  crime_escolhido_crimevis <- input$crime_markov
  
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
         yaxis = list(title = "Odds Espaço-Temporal")#,
         #legend = list(orientation = 'h')
         )
  
  
  
  })

  
  output$teste_chi_homog_temporal <- renderText({
    
    anos_janela_1 <- input$anos_markov_janela_1
    anos_janela_2 <- input$anos_markov_janela_2
    
    crime <- input$crime_markov
    
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
