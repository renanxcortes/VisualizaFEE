# Packages
library(rgdal)
library(plotly)
library(stringi)
library(tidyverse)
library(dplyr)
library(tidyr)
library(d3plus)
library(DT)

df_ppp <- readRDS("popvisBase_2016.rds")

df_proj <- readRDS("proj_rs.rds")

df_pre_pre <- df_ppp[df_ppp$Mun != "Pinto Bandeira",]
corresp_pre <- readRDS("Corresp_Mun_PopRS.rds")
corresp <- corresp_pre[corresp_pre$Cidade != "Pinto Bandeira", ]

df_pre <- inner_join(df_pre_pre, corresp, by = "CodIBGE")

mapa_rs <- readRDS("mapaRS.rds")
mapa_cr <- readRDS("mapaCR.rds")
mapa_rf <- readRDS("mapaRF.rds")

levels(mapa_cr$Corede) <- stri_conv(as.character(levels(mapa_cr$Corede)), "latin1", "UTF-8") # Para codificação dos mapas dos coredes
levels(mapa_cr$Corede)[14] <- "Metropolitano Delta do Jacuí"

mapa_rs@data$Nome_Munic <- stri_conv(as.character(mapa_rs@data$Nome_Munic), "latin1", "UTF-8")



df_pre$Mun <- stri_conv(as.character(df_pre$Mun), "latin1", "UTF-8")
df_pre$Classe <- stri_conv(as.character(df_pre$Classe), "latin1", "UTF-8")
df_pre$Corede <- stri_conv(as.character(df_pre$Corede), "latin1", "UTF-8")

df_pre_download <- select(df_pre, -OrdemClasse, -Estado, -CodIBGE6, -CodUG, -CodMeso, -Meso, -CodMicro, -Micro, -CodMunicipio, -CodCorede, -Cidade) # Tabela para Download

options(shiny.sanitize.errors = FALSE)

mapinha_RS <- function() {
    
    leaflet(mapa_rs) %>%
      addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 1, smoothFactor = 0.2, color = '#00008B', 
                  fillColor = '#00008B', label = ~Nome_Munic,  layerId = ~Nome_Munic)
    
}

mapinha_COREDE <- function() {
    
    leaflet(mapa_cr) %>%
      addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 1, smoothFactor = 0.2, color = '#00008B', 
                  fillColor = '#00008B', label = ~Corede, layerId = ~Corede)
    
  }
  
mapinha_RF <- function() {
    
    leaflet(mapa_rf) %>%
      addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 1, smoothFactor = 0.2, color = '#00008B', 
                  fillColor = '#00008B', label = ~paste0("Região ", OBJECTID), layerId = ~OBJECTID) # O OBJECTID não aparecia no tooltip, aparentemente, por ser numérico.

      #Foi trocado label para paste0 para exibir o nome "REGIÂO "
    
  }



# Define server logic required
shinyServer(function(input, session, output) {
  
  
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
  })
  
  output$piramide_municipio <- renderPlotly({
    
    ano <- input$ano_piramide
    #municipio <- stri_conv(as.character(input$cidade_piramide), "UTF-8", "latin1")
    municipio <- input$cidade_piramide
    
    df_aux_universo <- filter(df_pre, Mun == municipio, Classe != "Total") # Para fixar o eixo x na pirâmide ao longo do tempo
    
    df_aux <- filter(df_pre, Ano == ano, Mun == municipio, Classe != "Total")
    
    df_arrumado <- df_aux %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      mutate(Homens = -1 * Homens) %>%
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    df_arrumado_universo <- df_aux_universo %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      mutate(Homens = -1 * Homens) %>%
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    plot_ly(df_arrumado, x = ~Populacao, y = ~Classe, color = ~Genero, type = 'bar', orientation = 'h', 
            hoverinfo = 'y+text+name', text = ~abs_pop, colors = c('navy', 'red')) %>%
      layout(title = paste0(municipio, ', ', ano), 
             bargap = 0.3, barmode = 'overlay', 
             xaxis = list(tickmode = 'array', #tickvals = vals, ticktext = ticks(vals), 
                          title = "População", 
                          range = c(min(df_arrumado_universo$Populacao) + min(df_arrumado_universo$Populacao) / 6, 
                                    max(df_arrumado_universo$Populacao) + max(df_arrumado_universo$Populacao) / 6)), 
             yaxis = list(title = ""))
    
  })
  
  
  selected_city <- reactive({
    
    b <- mapa_rs[mapa_rs$Nome_Munic == input$cidade_piramide & !is.na(mapa_rs$Nome_Munic), ]
    
    return(b)
    
  })
  
  
  output$mapinha_municipios <- renderLeaflet({
    
    mapinha_RS() %>%
      addPolygons(data = selected_city(), fill = FALSE, color = '#FFFF00', 
                  opacity = 1, layerId = 'sel_cty') %>%
      fitBounds(lng1 = bbox(selected_city())[1], 
                lat1 = bbox(selected_city())[2], 
                lng2 = bbox(selected_city())[3], 
                lat2 = bbox(selected_city())[4])
    
  })

  muni_click <- eventReactive(input$mapinha_municipios_shape_click, {
      
      x <- input$mapinha_municipios_shape_click # O "_shape_click" é criado internamente pelo leaflet.
      
      y <- x$id
      
      return(y)
      
    })
    
  observe({
      
      updateSelectInput(session, 'cidade_piramide', selected = muni_click())
      
    })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$piramide_corede <- renderPlotly({
    
    ano <- input$ano_piramide_corede
    #corede <- stri_conv(as.character(input$corede_piramide), "UTF-8", "latin1")
    corede <- input$corede_piramide
    
    df_aux_universo <- filter(df_pre, Corede == corede, Classe != "Total") # Para fixar o eixo x na pirâmide ao longo do tempo
    
    df_aux <- filter(df_pre, Ano == ano, Corede == corede, Classe != "Total")
    
    df_arrumado <- df_aux %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      #
      mutate(Homens = -1 * Homens) %>%
      group_by(Ano, Classe) %>%
      #
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres)) %>%
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    df_arrumado_universo <- df_aux_universo %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      #
      mutate(Homens = -1 * Homens) %>%
      group_by(Ano, Classe) %>%
      #
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres)) %>%
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    plot_ly(df_arrumado, x = ~Populacao, y = ~Classe, color = ~Genero, type = 'bar', orientation = 'h', 
            hoverinfo = 'y+text+name', text = ~abs_pop, colors = c('navy', 'red')) %>%
      layout(title = paste0(corede, ', ', ano), 
             bargap = 0.3, barmode = 'overlay', 
             xaxis = list(tickmode = 'array', #tickvals = vals, ticktext = ticks(vals), 
                          title = "População", 
                          range = c(min(df_arrumado_universo$Populacao) + min(df_arrumado_universo$Populacao) / 6, 
                                    max(df_arrumado_universo$Populacao) + max(df_arrumado_universo$Populacao) / 6)), 
             yaxis = list(title = ""))
    
  })
  
  
  
  
  selected_corede <- reactive({
    
    b <- mapa_cr[mapa_cr$Corede == input$corede_piramide & !is.na(mapa_cr$Corede), ]
    
    return(b)
    
  })
  
  
  output$mapinha_corede <- renderLeaflet({
    
  	mapinha_COREDE() %>%
      addPolygons(data = selected_corede(), fill = FALSE, color = '#FFFF00',
                  opacity = 1, layerId = 'sel_cor') %>%
      fitBounds(lng1 = bbox(selected_corede())[1],
                lat1 = bbox(selected_corede())[2],
                lng2 = bbox(selected_corede())[3],
                lat2 = bbox(selected_corede())[4])
    
  })

   cor_click <- eventReactive(input$mapinha_corede_shape_click, {
      
      x <- input$mapinha_corede_shape_click
      
      y <- x$id
      
      return(y)
      
    })
    
  observe({
      
      updateSelectInput(session, 'corede_piramide', selected = cor_click())
      
    })

  
  
  
  
  
  output$piramide_rf <- renderPlotly({
    
    ano <- input$ano_piramide_rf
    #RF <- stri_conv(as.character(input$rf_piramide), "UTF-8", "latin1")
    RF <- input$rf_piramide
    
    df_aux_universo <- filter(df_pre, CodRF == RF, Classe != "Total") # Para fixar o eixo x na pirâmide ao longo do tempo
    
    df_aux <- filter(df_pre, Ano == ano, CodRF == RF, Classe != "Total")
    
    df_arrumado <- df_aux %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      mutate(Homens = -1 * Homens) %>%
      #
      group_by(Ano, Classe) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres)) %>%
      #
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    df_arrumado_universo <- df_aux_universo %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      mutate(Homens = -1 * Homens) %>%
      #
      group_by(Ano, Classe) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres)) %>%
      #
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    plot_ly(df_arrumado, x = ~Populacao, y = ~Classe, color = ~Genero, type = 'bar', orientation = 'h', 
            hoverinfo = 'y+text+name', text = ~abs_pop, colors = c('navy', 'red')) %>%
      layout(title = paste0("Região Funcional ",RF, ', ', ano), 
             bargap = 0.3, barmode = 'overlay', 
             xaxis = list(tickmode = 'array', #tickvals = vals, ticktext = ticks(vals), 
                          title = "População", 
                          range = c(min(df_arrumado_universo$Populacao) + min(df_arrumado_universo$Populacao) / 6, 
                                    max(df_arrumado_universo$Populacao) + max(df_arrumado_universo$Populacao) / 6)), 
             yaxis = list(title = ""))
    
  })
  
  selected_rf <- reactive({
    
    b <- mapa_rf[mapa_rf$OBJECTID == input$rf_piramide & !is.na(mapa_rf$OBJECTID), ]
    
    return(b)
    
  })
  
  
  output$mapinha_rf <- renderLeaflet({
    
    mapinha_RF() %>%
      addPolygons(data = selected_rf(), fill = FALSE, color = '#FFFF00',
                  opacity = 1, layerId = 'sel_rf') %>%
      fitBounds(lng1 = bbox(selected_rf())[1],
                lat1 = bbox(selected_rf())[2],
                lng2 = bbox(selected_rf())[3],
                lat2 = bbox(selected_rf())[4])
    
  })

  rf_click <- eventReactive(input$mapinha_rf_shape_click, {
      
      x <- input$mapinha_rf_shape_click
      
      y <- x$id
      
      return(y)
      
    })
    
  observe({
      
      updateSelectInput(session, 'rf_piramide', selected = rf_click())
      
    })

  
  
  
  output$piramide_rs <- renderPlotly({
    
    ano <- input$ano_piramide_rs
    
    df_aux_rs_universo <- filter(df_pre, Classe != "Total") # Para fixar o eixo x na pirâmide ao longo do tempo
    
    df_aux_rs <- filter(df_pre, Ano == ano, Classe != "Total")
    
    df_arrumado_rs <- df_aux_rs %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      mutate(Homens = -1 * Homens) %>%
      group_by(Ano, Classe) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres)) %>%
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    df_arrumado_rs_universo <- df_aux_rs_universo %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      mutate(Homens = -1 * Homens) %>%
      group_by(Ano, Classe) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres)) %>%
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    plot_ly(df_arrumado_rs, x = ~Populacao, y = ~Classe, color = ~Genero, type = 'bar', orientation = 'h', 
            hoverinfo = 'y+text+name', text = ~abs_pop, colors = c('navy', 'red')) %>%
      layout(title = paste0("Rio Grande do Sul", ', ', ano), 
             bargap = 0.3, barmode = 'overlay', 
             xaxis = list(tickmode = 'array', #tickvals = vals, ticktext = ticks(vals), 
                          title = "População", 
                          range = c(min(df_arrumado_rs_universo$Populacao) + min(df_arrumado_rs_universo$Populacao) / 6, 
                                    max(df_arrumado_rs_universo$Populacao) + max(df_arrumado_rs_universo$Populacao) / 6)), 
             yaxis = list(title = ""))
    
  })
  
  
  
  
  output$piramide_rs_proj <- renderPlotly({
    
    ano <- input$ano_piramide_rs_proj
    
    df_aux_rs_universo <- filter(df_proj, Classe != "Total") # Para fixar o eixo x na pirâmide ao longo do tempo
    
    df_aux_rs <- filter(df_proj, Ano == ano, Classe != "Total")
    
    df_arrumado_rs <- df_aux_rs %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      mutate(Homens = -1 * Homens) %>%
      group_by(Ano, Classe) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres)) %>%
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    df_arrumado_rs_universo <- df_aux_rs_universo %>% 
      select(Ano, Homens, Mulheres, Classe) %>%
      mutate(Homens = -1 * Homens) %>%
      group_by(Ano, Classe) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres)) %>%
      gather(Genero, Populacao, -Classe, -Ano) %>%
      mutate(abs_pop = abs(Populacao))
    
    plot_ly(df_arrumado_rs, x = ~Populacao, y = ~Classe, color = ~Genero, type = 'bar', orientation = 'h', 
            hoverinfo = 'y+text+name', text = ~abs_pop, colors = c('navy', 'red')) %>%
      layout(title = paste0("Rio Grande do Sul", ', ', ano), 
             bargap = 0.3, barmode = 'overlay', 
             xaxis = list(tickmode = 'array', #tickvals = vals, ticktext = ticks(vals), 
                          title = "População", 
                          range = c(min(df_arrumado_rs_universo$Populacao) + min(df_arrumado_rs_universo$Populacao) / 6, 
                                    max(df_arrumado_rs_universo$Populacao) + max(df_arrumado_rs_universo$Populacao) / 6)), 
             yaxis = list(title = ""))
    
  })
  
  
  
  
  
  
  output$mapa_genero <- renderLeaflet({
    
    #classe <- reactive({
    #validate(
    #  need(try(input$classe_mapa != ""), "Please select a data set")
    #)
    #input$classe_mapa
    #})
    
    #classe <- stri_conv(as.character(input$classe_mapa), "UTF-8", "latin1")
    classe <- input$classe_mapa
    ano <- input$ano_mapa
    sens <- input$sens_mapa
    
    df_aux_mapa_pre <- filter(df_pre, Ano == ano, Classe %in% classe)
    
    df_aux_mapa <- df_aux_mapa_pre %>% 
      select(Mun, CodIBGE, Ano, Homens, Mulheres, Total, Estado, CodIBGE) %>%
      group_by(Mun, CodIBGE, Ano, Estado) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    
    df_mapa <- merge(mapa_rs, df_aux_mapa, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
    
    # Este naco é para o mapa de bolhas. Não é possível colocar um loop "for" depois de um condicional 'else'
    aux <- numeric()
    am <- numeric()
    for (i in 1:dim(df_mapa)[1]){
      aux <- df_mapa@polygons[[i]]@labpt
      am <- rbind(am, aux)
    }
    am <- data.frame(am, row.names=NULL)
    names(am) <- c("Long", "Lat")
    
    if (input$genero_mapa == "total_mapa_radio") {
      
      leaflet(data = df_mapa) %>% addTiles() %>%
        addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "Green",
                   radius = ~ df_mapa$Total^(1/sens)*30, popup = paste0(df_mapa$Nome_Munic, "<br>",
                                                                                   "Pop.: ", df_mapa$Total))
    }
    
    else {
      
      if (input$genero_mapa == "homens_mapa_radio") {
        
        leaflet(data = df_mapa) %>% addTiles() %>%
          addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "navy",
                     radius = ~ df_mapa$Homens^(1/sens)*30, popup = paste0(df_mapa$Nome_Munic, "<br>",
                                                                                      "Homens: ", df_mapa$Homens))
        
      } else {
        
        leaflet(data = df_mapa) %>% addTiles() %>%
          addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "red",
                     radius = ~ df_mapa$Mulheres^(1/sens)*30, popup = paste0(df_mapa$Nome_Munic, "<br>",
                                                                                        "Mulheres: ", df_mapa$Mulheres))
      }
      
    }
   
    
  })
  
  
  
  
  
  output$mapa_genero_percent <- renderLeaflet({
    
    #classe <- reactive({
    #validate(
    #  need(try(input$classe_mapa != ""), "Please select a data set")
    #)
    #input$classe_mapa
    #})
    
    #classe <- stri_conv(as.character(input$classe_mapa), "UTF-8", "latin1")
    classe <- input$classe_mapa
    ano <- input$ano_mapa
    
    df_aux_mapa_pre <-  df_pre %>% 
      select(Mun, CodIBGE, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodIBGEAno = paste0(CodIBGE,Ano)) %>%
      filter(Ano == ano, Classe %in% classe)
    
    # AUXILIAR PARA CALCULAR O TOTAL DA REGIAO
    df_aux_mapa_pre_pre <-  df_pre %>% 
      select(Mun, CodIBGE, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodIBGEAno = paste0(CodIBGE,Ano)) %>%
      filter(Ano == ano, Classe == "Total")
    
    df_aux_mapa <- df_aux_mapa_pre %>%
      select(Mun, CodIBGE, Ano, Homens, Mulheres, Total, CodIBGEAno) %>%
      group_by(Mun, CodIBGE, Ano, CodIBGEAno) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    
    aux_tot_mun <-   df_aux_mapa_pre_pre %>%
      mutate(CodIBGEAno = paste0(CodIBGE,Ano)) %>%
      group_by(CodIBGEAno) %>%
      summarize(Total_REGIAO=sum(Total))
    
    tb_aux_mapa <- inner_join(df_aux_mapa, aux_tot_mun, by = "CodIBGEAno")
    
    tb_mapa_final <- tb_aux_mapa %>% mutate(H_Perc = Homens/Total_REGIAO*100, 
                                            M_Perc = Mulheres/Total_REGIAO*100,
                                            T_Perc = Total/Total_REGIAO*100)
    
    
    
    df_mapa <- merge(mapa_rs, tb_mapa_final, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
    
    if (input$genero_mapa == "total_mapa_radio") {
      
      #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$T_Perc)
	  gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen"), domain = df_mapa$T_Perc)
      
      leaflet(data = df_mapa) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$T_Perc), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.5, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Nome_Munic, "<br>",
                                   "Pessoas: ", df_mapa$Total, "<br>",
                                   "Total: ", df_mapa$Total_REGIAO, "<br>",
                                   "Percent. (%): ", round(df_mapa$T_Perc,2))) %>% 
        addLegend(position = "bottomright", pal = gradiente,values = ~T_Perc)
    }
    
    else {
      
      if (input$genero_mapa == "homens_mapa_radio") {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$H_Perc)
        gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen"), domain = df_mapa$H_Perc)
		
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$H_Perc), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.5, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(df_mapa$Nome_Munic, "<br>",
                                     "Pessoas: ", df_mapa$Homens, "<br>",
                                     "Total: ", df_mapa$Total_REGIAO, "<br>",
                                     "Percent. (%): ", round(df_mapa$H_Perc,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~H_Perc)
        
      } else {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$M_Perc)
		gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen"), domain = df_mapa$M_Perc)
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$M_Perc), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.5, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(df_mapa$Nome_Munic, "<br>",
                                     "Pessoas: ", df_mapa$Mulheres, "<br>",
                                     "Total: ", df_mapa$Total_REGIAO, "<br>",
                                     "Percent. (%): ", round(df_mapa$M_Perc,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~M_Perc)
      }
      
    }
    
    
  })
  
  
    output$mapa_genero_variacao <- renderLeaflet({
    
    #classe <- reactive({
    #validate(
    #  need(try(input$classe_mapa != ""), "Please select a data set")
    #)
    #input$classe_mapa
    #})
    
    classe <- stri_conv(as.character(input$classe_mapa), "UTF-8", "latin1")
    #classe <- input$classe_mapa
    #classe <- c("00 a 04", "25 a 29")
    ano_inicial <- min(df_pre$Ano)
    ano_final   <- max(df_pre$Ano)
    
    # Inicial
    df_aux_mapa_pre_inicial <-  df_pre %>% 
      select(Mun, CodIBGE, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodIBGEAno = paste0(CodIBGE,Ano)) %>%
      filter(Ano == ano_inicial, Classe %in% classe) %>%
      group_by(Mun, CodIBGE, Ano, CodIBGEAno) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    # Final
    df_aux_mapa_pre_final <-  df_pre %>% 
      select(Mun, CodIBGE, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodIBGEAno = paste0(CodIBGE,Ano)) %>%
      filter(Ano == ano_final, Classe %in% classe) %>%
      group_by(Mun, CodIBGE, Ano, CodIBGEAno) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    
    base_joineada <- inner_join(df_aux_mapa_pre_inicial, df_aux_mapa_pre_final, by = "CodIBGE")
    
    
    tb_mapa_final <- base_joineada %>% mutate(Var_H = (Homens.y/Homens.x-1)*100, 
                                              Var_M = (Mulheres.y/Mulheres.x-1)*100, 
                                              Var_T = (Total.y/Total.x-1)*100)
    
    
    df_mapa <- merge(mapa_rs, tb_mapa_final, by.x = "GEOCODIG_M", by.y="CodIBGE", all.x = FALSE)
    
    if (input$genero_mapa == "total_mapa_radio") {
      
      #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Var_T)
	  #gradiente = colorNumeric(c("Red", "yellow", "green3", "darkgreen"), domain = df_mapa$Var_T)
	  #gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen") domain = df_mapa$Var_T)
	  
	  # NOVO GRADIENTE
	  aux <- abs(range(df_mapa$Var_T)[which.max(abs(range(df_mapa$Var_T)))])
	  gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = c(-aux, aux))

      
      leaflet(data = df_mapa) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Var_T), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Nome_Munic, "<br>", # Na horado merge duplicou o nome do Corede
                                   "Pessoas em ",ano_inicial,": ", df_mapa$Total.x, "<br>",
                                   "Pessoas em ",ano_final,": ", df_mapa$Total.y, "<br>",
                                   "Variação (%): ", round(df_mapa$Var_T,2))) %>% 
        addLegend(position = "bottomright", pal = gradiente,values = ~Var_T)
    }
    
    else {
      
      if (input$genero_mapa == "homens_mapa_radio") {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Var_H)
		#gradiente = colorNumeric(c("Red", "yellow", "green3", "darkgreen"), domain = df_mapa$Var_H)
		
	  # NOVO GRADIENTE
	  aux <- abs(range(df_mapa$Var_H)[which.max(abs(range(df_mapa$Var_H)))])
	  gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = c(-aux, aux))
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Var_H), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(df_mapa$Nome_Munic, "<br>", # Na horado merge duplicou o nome do Corede
                                     "Pessoas em ",ano_inicial,": ", df_mapa$Homens.x, "<br>",
                                     "Pessoas em ",ano_final,": ", df_mapa$Homens.y, "<br>",
                                     "Variação (%): ", round(df_mapa$Var_H,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~Var_H)
        
      } else {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Var_M)
		#gradiente = colorNumeric(c("Red", "yellow", "green3", "darkgreen"), domain = df_mapa$Var_M)
		
	  # NOVO GRADIENTE
	  aux <- abs(range(df_mapa$Var_M)[which.max(abs(range(df_mapa$Var_M)))])
	  gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = c(-aux, aux))
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Var_M), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(df_mapa$Nome_Munic, "<br>", # Na horado merge duplicou o nome do Corede
                                     "Pessoas em ",ano_inicial,": ", df_mapa$Mulheres.x, "<br>",
                                     "Pessoas em ",ano_final,": ", df_mapa$Mulheres.y, "<br>",
                                     "Variação (%): ", round(df_mapa$Var_M,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~Var_M)
      }
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$mapa_genero_cr <- renderLeaflet({
    
    #classe <- reactive({
    #validate(
    #  need(try(input$classe_mapa != ""), "Please select a data set")
    #)
    #input$classe_mapa
    #})
    
    classe <- stri_conv(as.character(input$classe_mapa_cr), "UTF-8", "latin1")
    #classe <- input$classe_mapa_cr
    ano <- input$ano_mapa_cr
    sens <- input$sens_mapa_cr
    
    df_aux_mapa_pre <- filter(df_pre, Ano == ano, Classe %in% classe)
    
    df_aux_mapa <- df_aux_mapa_pre %>% 
      select(CodCorede, Ano, Homens, Mulheres, Total, Estado) %>%
      group_by(CodCorede, Ano, Estado) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    
    df_mapa <- merge(mapa_cr, df_aux_mapa, by.x = "OBJECTID", by.y="CodCorede", all.x = FALSE)
    
    # Este naco é para o mapa de bolhas. Não é possível colocar um loop "for" depois de um condicional 'else'
    aux <- numeric()
    am <- numeric()
    for (i in 1:dim(df_mapa)[1]){
      aux <- df_mapa@polygons[[i]]@labpt
      am <- rbind(am, aux)
    }
    am <- data.frame(am, row.names=NULL)
    names(am) <- c("Long", "Lat")
    
    if (input$genero_mapa_cr == "total_mapa_radio") {
      
      leaflet(data = df_mapa) %>% addTiles() %>%
        addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "Green",
                   radius = ~ df_mapa$Total^(1/sens)*30, popup = paste0(df_mapa$Corede, "<br>",
                                                                                   "Pop.: ", df_mapa$Total))
    }
    
    else {
      
      if (input$genero_mapa_cr == "homens_mapa_radio") {
        
        leaflet(data = df_mapa) %>% addTiles() %>%
          addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "navy",
                     radius = ~ df_mapa$Homens^(1/sens)*30, popup = paste0(df_mapa$Corede, "<br>",
                                                                                      "Homens: ", df_mapa$Homens))
        
      } else {
        
        leaflet(data = df_mapa) %>% addTiles() %>%
          addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "red",
                     radius = ~ df_mapa$Mulheres^(1/sens)*30, popup = paste0(df_mapa$Corede, "<br>",
                                                                                        "Mulheres: ", df_mapa$Mulheres))
      }
      
    }
    
  })
  
  
  
  
  
  output$mapa_genero_cr_percent <- renderLeaflet({
    
    #classe <- reactive({
    #validate(
    #  need(try(input$classe_mapa != ""), "Please select a data set")
    #)
    #input$classe_mapa
    #})
    
    #classe <- stri_conv(as.character(input$classe_mapa_cr), "UTF-8", "latin1")
    classe <- input$classe_mapa_cr
    ano <- input$ano_mapa_cr
    
    df_aux_mapa_pre <-  df_pre %>% 
      select(Corede, CodCorede, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodCoredeAno = paste0(CodCorede,Ano)) %>%
      filter(Ano == ano, Classe %in% classe)
    
    # AUXILIAR PARA CALCULAR O TOTAL DA REGIAO
    df_aux_mapa_pre_pre <-  df_pre %>% 
      select(Corede, CodCorede, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodCoredeAno = paste0(CodCorede,Ano)) %>%
      filter(Ano == ano, Classe == "Total")
    
    df_aux_mapa <- df_aux_mapa_pre %>%
      select(Corede, CodCorede, Ano, Homens, Mulheres, Total, CodCoredeAno) %>%
      group_by(Corede, CodCorede, Ano, CodCoredeAno) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    
    aux_tot_cr <-   df_aux_mapa_pre_pre %>%
      mutate(CodCoredeAno = paste0(CodCorede,Ano)) %>%
      group_by(CodCoredeAno) %>%
      summarize(Total_REGIAO=sum(Total))
    
    tb_aux_mapa <- inner_join(df_aux_mapa, aux_tot_cr, by = "CodCoredeAno")
    
    tb_mapa_final <- tb_aux_mapa %>% mutate(H_Perc = Homens/Total_REGIAO*100, 
                                            M_Perc = Mulheres/Total_REGIAO*100,
                                            T_Perc = Total/Total_REGIAO*100)
    

    df_mapa <- merge(mapa_cr, tb_mapa_final, by.x = "OBJECTID", by.y="CodCorede", all.x = FALSE)
    
    if (input$genero_mapa_cr == "total_mapa_radio") {
      
      #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$T_Perc)
	  gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen"), domain = df_mapa$T_Perc)
	  
      leaflet(data = df_mapa) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$T_Perc), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Corede.x, "<br>", # Na horado merge duplicou o nome do Corede
                                   "Pessoas: ", df_mapa$Total, "<br>",
                                   "Total: ", df_mapa$Total_REGIAO, "<br>",
                                   "Percent. (%): ", round(df_mapa$T_Perc,2))) %>% 
        addLegend(position = "bottomright", pal = gradiente,values = ~T_Perc)
    }
    
    else {
      
      if (input$genero_mapa_cr == "homens_mapa_radio") {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$H_Perc)
		gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen"), domain = df_mapa$H_Perc)
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$H_Perc), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(df_mapa$Corede.x, "<br>", # Na horado merge duplicou o nome do Corede
                                     "Pessoas: ", df_mapa$Homens, "<br>",
                                     "Total: ", df_mapa$Total_REGIAO, "<br>",
                                     "Percent. (%): ", round(df_mapa$H_Perc,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~H_Perc)
        
      } else {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$M_Perc)
		gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen"), domain = df_mapa$M_Perc)
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$M_Perc), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(df_mapa$Corede.x, "<br>", # Na horado merge duplicou o nome do Corede
                                     "Pessoas: ", df_mapa$Mulheres, "<br>",
                                     "Total: ", df_mapa$Total_REGIAO, "<br>",
                                     "Percent. (%): ", round(df_mapa$M_Perc,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~M_Perc)
      }
      
    }
    
    
  })
  
  
  output$mapa_genero_cr_variacao <- renderLeaflet({
    
    #classe <- reactive({
    #validate(
    #  need(try(input$classe_mapa != ""), "Please select a data set")
    #)
    #input$classe_mapa
    #})
    
    classe <- stri_conv(as.character(input$classe_mapa_cr), "UTF-8", "latin1")
    #classe <- input$classe_mapa_cr
    #classe <- c("00 a 04", "25 a 29")
    ano_inicial <- min(df_pre$Ano)
    ano_final   <- max(df_pre$Ano)
    
    # Inicial
    df_aux_mapa_pre_inicial <-  df_pre %>% 
      select(Corede, CodCorede, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodCoredeAno = paste0(CodCorede,Ano)) %>%
      filter(Ano == ano_inicial, Classe %in% classe) %>%
      group_by(Corede, CodCorede, Ano, CodCoredeAno) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    # Final
    df_aux_mapa_pre_final <-  df_pre %>% 
      select(Corede, CodCorede, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodCoredeAno = paste0(CodCorede,Ano)) %>%
      filter(Ano == ano_final, Classe %in% classe) %>%
      group_by(Corede, CodCorede, Ano, CodCoredeAno) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    
    base_joineada <- inner_join(df_aux_mapa_pre_inicial, df_aux_mapa_pre_final, by = "CodCorede")
    
    
    tb_mapa_final <- base_joineada %>% mutate(Var_H = (Homens.y/Homens.x-1)*100, 
                                              Var_M = (Mulheres.y/Mulheres.x-1)*100, 
                                              Var_T = (Total.y/Total.x-1)*100)
    
    
    df_mapa <- merge(mapa_cr, tb_mapa_final, by.x = "OBJECTID", by.y="CodCorede", all.x = FALSE)
    
    if (input$genero_mapa_cr == "total_mapa_radio") {
      
      #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Var_T)
	  #gradiente = colorNumeric(c("Red", "yellow", "green3", "darkgreen"), domain = df_mapa$Var_T)
	  
	  # NOVO GRADIENTE
	  aux <- abs(range(df_mapa$Var_T)[which.max(abs(range(df_mapa$Var_T)))])
	  gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = c(-aux, aux))
	  	        
      leaflet(data = df_mapa) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Var_T), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Corede.x, "<br>", # Na horado merge duplicou o nome do Corede
                                   "Pessoas em ",ano_inicial,": ", df_mapa$Total.x, "<br>",
                                   "Pessoas em ",ano_final,": ", df_mapa$Total.y, "<br>",
                                   "Variação (%): ", round(df_mapa$Var_T,2))) %>% 
        addLegend(position = "bottomright", pal = gradiente,values = ~Var_T)
    }
    
    else {
      
      if (input$genero_mapa_cr == "homens_mapa_radio") {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Var_H)
		#gradiente = colorNumeric(c("Red", "yellow", "green3", "darkgreen"), domain = df_mapa$Var_H)
		
	  # NOVO GRADIENTE
	  aux <- abs(range(df_mapa$Var_H)[which.max(abs(range(df_mapa$Var_H)))])
	  gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = c(-aux, aux))
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Var_H), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(df_mapa$Corede.x, "<br>", # Na horado merge duplicou o nome do Corede
                                     "Pessoas em ",ano_inicial,": ", df_mapa$Homens.x, "<br>",
                                     "Pessoas em ",ano_final,": ", df_mapa$Homens.y, "<br>",
                                     "Variação (%): ", round(df_mapa$Var_H,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~Var_H)
        
      } else {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Var_M)
		#gradiente = colorNumeric(c("Red", "yellow", "green3", "darkgreen"), domain = df_mapa$Var_M)
		
	  # NOVO GRADIENTE
	  aux <- abs(range(df_mapa$Var_M)[which.max(abs(range(df_mapa$Var_M)))])
	  gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = c(-aux, aux))
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Var_M), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(df_mapa$Corede.x, "<br>", # Na horado merge duplicou o nome do Corede
                                     "Pessoas em ",ano_inicial,": ", df_mapa$Mulheres.x, "<br>",
                                     "Pessoas em ",ano_final,": ", df_mapa$Mulheres.y, "<br>",
                                     "Variação (%): ", round(df_mapa$Var_M,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~Var_M)
      }
      
    }
    
    
  })  
  
  
  
  
  
  
  
  
  
  
  output$mapa_genero_rf <- renderLeaflet({
    
    #classe <- reactive({
    #validate(
    #  need(try(input$classe_mapa != ""), "Please select a data set")
    #)
    #input$classe_mapa
    #})
    
    #classe <- stri_conv(as.character(input$classe_mapa), "UTF-8", "latin1")
    classe <- input$classe_mapa_rf
    ano <- input$ano_mapa_rf
    sens <- input$sens_mapa_rf
    
    df_aux_mapa_pre <- filter(df_pre, Ano == ano, Classe %in% classe)
    
    df_aux_mapa <- df_aux_mapa_pre %>% 
      select(CodRF, Ano, Homens, Mulheres, Total) %>%
      group_by(CodRF, Ano) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    
    df_mapa <- merge(mapa_rf, df_aux_mapa, by.x = "OBJECTID", by.y="CodRF", all.x = FALSE)
    
    # Este naco é para o mapa de bolhas. Não é possível colocar um loop "for" depois de um condicional 'else'
    aux <- numeric()
    am <- numeric()
    for (i in 1:dim(df_mapa)[1]){
      aux <- df_mapa@polygons[[i]]@labpt
      am <- rbind(am, aux)
    }
    am <- data.frame(am, row.names=NULL)
    names(am) <- c("Long", "Lat")
    
    if (input$genero_mapa_rf == "total_mapa_radio") {
      
      leaflet(data = df_mapa) %>% addTiles() %>%
        addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "Green",
                   radius = ~ df_mapa$Total^(1/sens)*30, popup = paste0("RF ", df_mapa$OBJECTID, "<br>",
                                                                                      "Pop.: ", df_mapa$Total))
    }
    
    else {
      
      if (input$genero_mapa_rf == "homens_mapa_radio") {
        
        leaflet(data = df_mapa) %>% addTiles() %>%
          addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "navy",
                     radius = ~ df_mapa$Homens^(1/sens)*30, popup = paste0("RF ", df_mapa$OBJECTID, "<br>",
                                                                                         "Homens: ", df_mapa$Homens))
        
      } else {
        
        leaflet(data = df_mapa) %>% addTiles() %>%
          addCircles(lng = ~am$Long, lat = ~am$Lat, weight = 1, color = "red",
                     radius = ~ df_mapa$Mulheres^(1/sens)*30, popup = paste0("RF ", df_mapa$OBJECTID, "<br>",
                                                                                           "Mulheres: ", df_mapa$Mulheres))
      }
      
    }
    
  })
  
  
  
  
  
  
  output$mapa_genero_rf_percent <- renderLeaflet({
    
    #classe <- reactive({
    #validate(
    #  need(try(input$classe_mapa != ""), "Please select a data set")
    #)
    #input$classe_mapa
    #})
    
    #classe <- stri_conv(as.character(input$classe_mapa), "UTF-8", "latin1")
    classe <- input$classe_mapa_rf
    ano <- input$ano_mapa_rf
    
    df_aux_mapa_pre <-  df_pre %>% 
      select(CodRF, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodRFAno = paste0(CodRF,Ano)) %>%
      filter(Ano == ano, Classe %in% classe)
    
    # AUXILIAR PARA CALCULAR O TOTAL DA REGIAO
    df_aux_mapa_pre_pre <-  df_pre %>% 
      select(CodRF, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodRFAno = paste0(CodRF,Ano)) %>%
      filter(Ano == ano, Classe == "Total")
    
    df_aux_mapa <- df_aux_mapa_pre %>%
      select(CodRF, Ano, Homens, Mulheres, Total, CodRFAno) %>%
      group_by(CodRF, Ano, CodRFAno) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    
    aux_tot_rf <-   df_aux_mapa_pre_pre %>%
      mutate(CodRFAno = paste0(CodRF,Ano)) %>%
      group_by(CodRFAno) %>%
      summarize(Total_REGIAO=sum(Total))
    
    tb_aux_mapa <- inner_join(df_aux_mapa, aux_tot_rf, by = "CodRFAno")
    
    tb_mapa_final <- tb_aux_mapa %>% mutate(H_Perc = Homens/Total_REGIAO*100, 
                                            M_Perc = Mulheres/Total_REGIAO*100,
                                            T_Perc = Total/Total_REGIAO*100)
    
    
    df_mapa <- merge(mapa_rf, tb_mapa_final, by.x = "OBJECTID", by.y="CodRF", all.x = FALSE)
    
    if (input$genero_mapa_rf == "total_mapa_radio") {
      
      #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$T_Perc)
	  gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen"), domain = df_mapa$T_Perc)
	  
	       
      leaflet(data = df_mapa) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$T_Perc), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0("RF ", df_mapa$OBJECTID, "<br>",
                                   "Pessoas: ", df_mapa$Total, "<br>",
                                   "Total: ", df_mapa$Total_REGIAO, "<br>",
                                   "Percent. (%): ", round(df_mapa$T_Perc,2))) %>% 
        addLegend(position = "bottomright", pal = gradiente,values = ~T_Perc)
    }
    
    else {
      
      if (input$genero_mapa_rf == "homens_mapa_radio") {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$H_Perc)
		gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen"), domain = df_mapa$H_Perc)
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$H_Perc), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0("RF ", df_mapa$OBJECTID, "<br>",
                                     "Pessoas: ", df_mapa$Homens, "<br>",
                                     "Total: ", df_mapa$Total_REGIAO, "<br>",
                                     "Percent. (%): ", round(df_mapa$H_Perc,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~H_Perc)
        
      } else {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$M_Perc)
		gradiente = colorNumeric(c("lightgrey", "yellow", "green3", "darkgreen"), domain = df_mapa$M_Perc)
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$M_Perc), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0("RF ", df_mapa$OBJECTID, "<br>",
                                     "Pessoas: ", df_mapa$Mulheres, "<br>",
                                     "Total: ", df_mapa$Total_REGIAO, "<br>",
                                     "Percent. (%): ", round(df_mapa$M_Perc,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~M_Perc)
      }
      
    }
    
    
  })
  
  
  
    output$mapa_genero_rf_variacao <- renderLeaflet({
    
    #classe <- reactive({
    #validate(
    #  need(try(input$classe_mapa != ""), "Please select a data set")
    #)
    #input$classe_mapa
    #})
    
    classe <- stri_conv(as.character(input$classe_mapa_rf), "UTF-8", "latin1")
    #classe <- input$classe_mapa_cr
    #classe <- c("00 a 04", "25 a 29")
    ano_inicial <- min(df_pre$Ano)
    ano_final   <- max(df_pre$Ano)
    
    # Inicial
    df_aux_mapa_pre_inicial <-  df_pre %>% 
      select(Corede, CodRF, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodRFAno = paste0(CodRF,Ano)) %>%
      filter(Ano == ano_inicial, Classe %in% classe) %>%
      group_by(CodRF, Ano, CodRFAno) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    # Final
    df_aux_mapa_pre_final <-  df_pre %>% 
      select(CodRF, Ano, Homens, Mulheres, Total, Classe) %>% # Pra dar uma enxugada na tabela
      mutate(CodRFAno = paste0(CodRF,Ano)) %>%
      filter(Ano == ano_final, Classe %in% classe) %>%
      group_by(CodRF, Ano, CodRFAno) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    
    base_joineada <- inner_join(df_aux_mapa_pre_inicial, df_aux_mapa_pre_final, by = "CodRF")
    
    
    tb_mapa_final <- base_joineada %>% mutate(Var_H = (Homens.y/Homens.x-1)*100, 
                                              Var_M = (Mulheres.y/Mulheres.x-1)*100, 
                                              Var_T = (Total.y/Total.x-1)*100)
    
    
    df_mapa <- merge(mapa_rf, tb_mapa_final, by.x = "OBJECTID", by.y="CodRF", all.x = FALSE)
    
    if (input$genero_mapa_rf == "total_mapa_radio") {
      
      #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Var_T)
	  #gradiente = colorNumeric(c("Red", "yellow", "green3", "darkgreen"), domain = df_mapa$Var_T)
	  
	  # NOVO GRADIENTE
	  aux <- abs(range(df_mapa$Var_T)[which.max(abs(range(df_mapa$Var_T)))])
	  gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = c(-aux, aux))
      
      leaflet(data = df_mapa) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Var_T), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0("RF ", df_mapa$OBJECTID, "<br>", # Na horado merge duplicou o nome do Corede
                                   "Pessoas em ",ano_inicial,": ", df_mapa$Total.x, "<br>",
                                   "Pessoas em ",ano_final,": ", df_mapa$Total.y, "<br>",
                                   "Variação (%): ", round(df_mapa$Var_T,2))) %>% 
        addLegend(position = "bottomright", pal = gradiente,values = ~Var_T)
    }
    
    else {
      
      if (input$genero_mapa_rf == "homens_mapa_radio") {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Var_H)
		#gradiente = colorNumeric(c("Red", "yellow", "green3", "darkgreen"), domain = df_mapa$Var_H)
		
	  # NOVO GRADIENTE
	  aux <- abs(range(df_mapa$Var_H)[which.max(abs(range(df_mapa$Var_H)))])
	  gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = c(-aux, aux))
			
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Var_H), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0("RF ", df_mapa$OBJECTID, "<br>", # Na horado merge duplicou o nome do Corede
                                     "Pessoas em ",ano_inicial,": ", df_mapa$Homens.x, "<br>",
                                     "Pessoas em ",ano_final,": ", df_mapa$Homens.y, "<br>",
                                     "Variação (%): ", round(df_mapa$Var_H,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~Var_H)
        
      } else {
        
        #gradiente = colorNumeric(c("lightgrey", "yellow", "orange", "Red"), domain = df_mapa$Var_M)
		#gradiente = colorNumeric(c("Red", "yellow", "green3", "darkgreen"), domain = df_mapa$Var_M)
		
      # NOVO GRADIENTE
	  aux <- abs(range(df_mapa$Var_M)[which.max(abs(range(df_mapa$Var_M)))])
	  gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = c(-aux, aux))
        
        leaflet(data = df_mapa) %>% 
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(weight = 0.5, fillColor = ~gradiente(df_mapa$Var_M), # Weight e a grossura das bordas
                      color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0("RF ", df_mapa$OBJECTID, "<br>", # Na horado merge duplicou o nome do Corede
                                     "Pessoas em ",ano_inicial,": ", df_mapa$Mulheres.x, "<br>",
                                     "Pessoas em ",ano_final,": ", df_mapa$Mulheres.y, "<br>",
                                     "Variação (%): ", round(df_mapa$Var_M,2))) %>% 
          addLegend(position = "bottomright", pal = gradiente,values = ~Var_M)
      }
      
    }
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$tree_mun <- renderD3plus({
  
    classe <- input$classe_tree_mun
    ano <- input$ano_tree_mun
    
    df_aux_tree_pre <- filter(df_pre, Ano == ano, Classe %in% classe)
    
    df_aux_tree <- df_aux_tree_pre %>% 
      select(Mun, Homens, Mulheres, Total) %>%
      group_by(Mun) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    if (input$genero_tree_mun == "homens_mapa_radio") {
      
      df <- df_aux_tree[,c(1,2)] # Homens
      d3plus("tree",df) } else 
        
      {
        if (input$genero_tree_mun == "mulheres_mapa_radio") {
          df <- df_aux_tree[,c(1,3)] # Mulheres
          d3plus("tree",df) } else  
           
          
            df <- df_aux_tree[,c(1,4)] # Total
            d3plus("tree",df) }
          
    })
  
  
  
  
  
  
  output$tree_cr <- renderD3plus({
    
    classe <- input$classe_tree_cr
    ano <- input$ano_tree_cr
    
    df_aux_tree_pre <- filter(df_pre, Ano == ano, Classe %in% classe)
    
    df_aux_tree <- df_aux_tree_pre %>% 
      select(Corede, Homens, Mulheres, Total) %>%
      group_by(Corede) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    if (input$genero_tree_cr == "homens_mapa_radio") {
      
      df <- df_aux_tree[,c(1,2)] # Homens
      d3plus("tree",df) } else 
        
      {
        if (input$genero_tree_cr == "mulheres_mapa_radio") {
          df <- df_aux_tree[,c(1,3)] # Mulheres
          d3plus("tree",df) } else  
            
            
            df <- df_aux_tree[,c(1,4)] # Total
          d3plus("tree",df) }
    
  })
  
  
  
  output$tree_rf <- renderD3plus({
    
    classe <- input$classe_tree_rf
    ano <- input$ano_tree_rf
    
    df_aux_tree_pre <- filter(df_pre, Ano == ano, Classe %in% classe)
    
    df_aux_tree <- df_aux_tree_pre %>% 
      select(CodRF, Homens, Mulheres, Total) %>%
      group_by(CodRF) %>%
      summarize(Homens = sum(Homens), Mulheres = sum(Mulheres), Total=sum(Total)) %>%
	  mutate(CodRF = paste("Região",CodRF)) %>%
      ungroup() # Tem que dar o ungroup pq ele não tava fazendo o merge logo abaixo
    
    if (input$genero_tree_rf == "homens_mapa_radio") {
      
      df <- df_aux_tree[,c(1,2)] # Homens
      d3plus("tree",df) } else 
        
      {
        if (input$genero_tree_rf == "mulheres_mapa_radio") {
          df <- df_aux_tree[,c(1,3)] # Mulheres
          d3plus("tree",df) } else  
            
            
            df <- df_aux_tree[,c(1,4)] # Total
          d3plus("tree",df) }
    
  })
  
  
  
        output$tabela = renderDataTable({
      datatable(df_pre_download, options(list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
        pageLength = 15)))
    })
    
    
      datasetInput <- reactive({
      df_pre_download
    })
    
    output$downloadData_est <- downloadHandler(
      filename = function() { paste('estimativas', '.csv', sep='') },
      content = function(file) {
        write.csv(datasetInput(), file)
      }
    )
    
    
    
     output$tabela_proj = renderDataTable({
      datatable(df_proj, options(list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
        pageLength = 15)))
    })
    
    
      datasetInput_proj <- reactive({
      df_proj
    })
    
    output$downloadData_proj <- downloadHandler(
      filename = function() { paste('projecoes', '.csv', sep='') },
      content = function(file) {
        write.csv(datasetInput_proj(), file)
      }
    )
  
  
  

  
  
  
  
  
  
  
})