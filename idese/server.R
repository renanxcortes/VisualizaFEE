library(shiny)
library(plotly)
library(dplyr)
require(stringi)

main_data <- readRDS("IDESE_TOTAL_V0506_1724.rds")
main_data$VALOR[main_data$VALOR == 0] <- NA
main_data <- na.omit(main_data)

mapa_RS_old <- readRDS("mapaRS_old.rds")
mapa_RS_old@data$Nome_Munic <- stri_conv(as.character(mapa_RS_old@data$Nome_Munic), "latin1", "UTF-8")


mapa_RS <- readRDS("MapaRSMunicipios.RDS")
mapa_RS@data$NOME <- stri_conv(as.character(mapa_RS@data$NOME), "latin1", "UTF-8")

mapa_Cor <- readRDS("MapaRSCoredes.RDS")
mapa_Meso <- readRDS("MapaRSMeso.RDS")
mapa_Micro <- readRDS("MapaRSMicro.RDS")
mapa_RF <- readRDS("MapaRSRF.RDS")

#Função Mapinhas Leaflet
mapinha_RS <- function() {
    
    leaflet(mapa_RS) %>%
      addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 1, smoothFactor = 0.2, color = '#00008B', 
                  fillColor = '#00008B', label = ~NOME,  layerId = ~NOME)
    
}

mapinha_COREDE <- function() {
    
    leaflet(mapa_Cor) %>%
      addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 1, smoothFactor = 0.2, color = '#00008B', 
                  fillColor = '#00008B', label = ~Corede, layerId = ~Corede)
    
  }
  
mapinha_RF <- function() {
    
    leaflet(mapa_RF) %>%
      addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 1, smoothFactor = 0.2, color = '#00008B', 
                  fillColor = '#00008B', label = ~RF, layerId = ~RF)
    
  }
  
mapinha_MICRO <- function() {
    
    leaflet(mapa_Micro) %>%
      addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 1, smoothFactor = 0.2, color = '#00008B', 
                  fillColor = '#00008B', label = ~Micro, layerId = ~Micro)
    
  }
  
mapinha_MESO <- function() {
    
    leaflet(mapa_Meso) %>%
      addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
      addPolygons(weight = 1, smoothFactor = 0.2, color = '#00008B', 
                  fillColor = '#00008B', label = ~Meso, layerId = ~Meso)
    
  }

##Base Collapsible Tree

idese_struct <- data.frame(H1 = rep("Idese", 12), 
                           H2 = rep(c("Educação", "Renda", "Saúde"), c(5, 2, 5)),
                           H3 = c("Pré-Escola", "Fundamental", "Fundamental", "Médio", "Esc. Adulta", 
                                  "Apropriação", "Geração",
                                  "Condições Gerais", "Condições Gerais",
                                  "Longevidade",
                                  "Saúde Materno-Infantil", "Saúde Materno-Infantil"
                           ),
                           H4 = c(NA, "Anos Iniciais", "Anos Finais", NA, NA, 
                                  NA, NA,
                                  "Mortes por Causas Evitáveis", "Óbitos por causas mal definidas", NA, "Consultas Pré-Natal", "Mortalidade"))



# Define server logic required
shinyServer(function(input, session, output){
  #FRAME da FEE no topo da página
  output$frame <- renderUI({
    my_test <- tags$iframe(src="http://www.fee.tche.br/barra/index.html",
                           height = "31px",
                           width = "100%",
                           borderbottom = "1px",
                           solid = "#6CB3D4",
                           bordertop = "0px",
                           borderleft = "0px",
                           borderright = "0px",
                           margin = "0px",
                           padding = "0px")
    print(my_test)
  })

  ##COLLAPSIBLETREERENDER

    output$idese_tree_struc <- renderCollapsibleTree({
    
    collapsibleTree(
      idese_struct,
      hierarchy = c("H2", "H3", "H4"),
      root = "Idese",
      fontSize = 11
      # fill = c("lightgrey",
      #          "Yellow", "Blue", "Green",
      #          "Yellow","Yellow","Yellow","Yellow","Blue", "Blue","Green","Green", "Green",
      #          "Yellow", "Yellow","Green","Green","Green","Green")
    )
    
  })

  ##SÉRIES TEMPORAIS Idese COMP_MUN
 output$ts_vis_idese_est <- renderPlotly({
      
      cat <- input$categoria_idese_est
      escala <- input$fixed_scale_est
      
      if(cat == "Idese"){
        cat_2 <- cat
      } else
        if(cat == "Bloco Educação"){
          cat_2 <- input$subcat_idese_est_educ
        } else
          if(cat == "Bloco Renda"){
            cat_2 <- input$subcat_idese_est_renda
          } else{
            cat_2 <- input$subcat_idese_est_saude
          }
      
      df_aux <- main_data %>%
                filter(TIPO_UNID == "Estado", CATEGORIA == cat_2)
      
      if(escala){
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
      } else{
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
      }
      
      x_attr <- "Ano"
      
      df_aux_ts <- df_aux %>%
        arrange(NOME,ANO)
      
      plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
              type = 'scatter', mode = 'lines', color = ~NOME,
              hoverinfo="text", ##Tirar tool tips pq formatacao era feia
              text = ~paste0(NOME, "<br>",
                             "Índice: ", round(VALOR, 3),"<br>",
                             "Ano: ", ANO)) %>%
        layout(title = "Rio Grande do Sul", yaxis = y_attr)
    })

 output$ts_vis_idese_mun <- renderPlotly({
    
    cidade <- input$idese_municipios
    cat <- input$categoria_idese_mun
    escala <- input$fixed_scale_mun

    if(cat == "Idese"){
      cat_2 <- cat
    } else
      if(cat == "Bloco Educação"){
        cat_2 <- input$subcat_idese_mun_educ
      } else
        if(cat == "Bloco Renda"){
          cat_2 <- input$subcat_idese_mun_renda
        } else{
          cat_2 <- input$subcat_idese_mun_saude
        }
    
    if(escala){
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
    } else{
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
    }
    
    if (!input$checkbox_inclui_rs_ts_idese){ 
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Municípios" & CATEGORIA == cat_2)
      df_aux <- filter(Idese_mun, NOME %in% cidade)} 
    else{                             
      Idese_mun <- main_data %>%
        filter(TIPO_UNID %in% c("Municípios","Estado") & CATEGORIA == cat_2)
      df_aux <- filter(Idese_mun, NOME %in% c(cidade, "Rio Grande do Sul"))
    }

    df_aux_ts <- df_aux %>%
      arrange(NOME,ANO)
    
    plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
            type = 'scatter', mode = 'lines', color = ~NOME,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(NOME, "<br>",
                           "Índice: ", round(VALOR, 3),"<br>",
                           "Ano: ", ANO)) %>%
      layout(title = cidade, yaxis = y_attr)
  })

 output$ts_vis_idese_cor <- renderPlotly({
    
    corede <- input$idese_corede_idese
    cat <- input$categoria_idese_cor
    
    escala <- input$fixed_scale_cor
    
    if(cat == "Idese"){
      cat_2 <- cat
    } else
      if(cat == "Bloco Educação"){
        cat_2 <- input$subcat_idese_cor_educ
      } else
        if(cat == "Bloco Renda"){
          cat_2 <- input$subcat_idese_cor_renda
        } else{
          cat_2 <- input$subcat_idese_cor_saude
        }
    
    if(escala){
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
    } else{
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
    }
    
    if (!input$checkbox_inclui_rs_ts_idese_corede){ 
      Idese_cor <- main_data %>%
        filter(TIPO_UNID == "Coredes. Def: 2011-atual" & CATEGORIA == cat_2)
      df_aux <- filter(Idese_cor, NOME %in% corede)} 
    else{                             
      Idese_cor <- main_data %>%
        filter(TIPO_UNID %in% c("Coredes. Def: 2011-atual","Estado") & CATEGORIA == cat_2)
      df_aux <- filter(Idese_cor, NOME %in% c(corede, "Rio Grande do Sul"))
    }

    df_aux_ts <- df_aux %>%
      arrange(NOME,ANO)
    
    plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
            type = 'scatter', mode = 'lines', color = ~NOME,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(NOME, "<br>",
                           "Índice: ", round(VALOR, 3),"<br>",
                           "Ano: ", ANO)) %>%
      layout(title = corede, yaxis = y_attr)
  })

 output$ts_vis_idese_rf <- renderPlotly({
    
    rf <- input$idese_rf_idese
    cat <- input$categoria_idese_rf
    
    escala <- input$fixed_scale_rf
    
    if(cat == "Idese"){
      cat_2 <- cat
    } else
      if(cat == "Bloco Educação"){
        cat_2 <- input$subcat_idese_rf_educ
      } else
        if(cat == "Bloco Renda"){
          cat_2 <- input$subcat_idese_rf_renda
        } else{
          cat_2 <- input$subcat_idese_rf_saude
        }
    
    if(escala){
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
    } else{
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
    }
    
    if (!input$checkbox_inclui_rs_ts_idese_rf){ 
      Idese_rf <- main_data %>%
        filter(TIPO_UNID == "Regiões Funcionais. Def: 2008-atual" & CATEGORIA == cat_2)
      df_aux <- filter(Idese_rf, NOME %in% rf)} 
    else{                             
      Idese_rf <- main_data %>%
        filter(TIPO_UNID %in% c("Regiões Funcionais. Def: 2008-atual","Estado") & CATEGORIA == cat_2)
      df_aux <- filter(Idese_rf, NOME %in% c(rf, "Rio Grande do Sul"))
    }
    
  
    df_aux_ts <- df_aux %>%
      arrange(NOME,ANO)
    
    plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
            type = 'scatter', mode = 'lines', color = ~NOME,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(NOME, "<br>",
                           "Índice: ", round(VALOR, 3),"<br>",
                           "Ano: ", ANO)) %>%
      layout(title = rf, yaxis = y_attr)
  })
  
  output$ts_vis_idese_micro <- renderPlotly({
    
    micro <- input$idese_micro
    cat <- input$categoria_idese_micro
    
    escala <- input$fixed_scale_micro

    if(cat == "Idese"){
      cat_2 <- cat
    } else
      if(cat == "Bloco Educação"){
        cat_2 <- input$subcat_idese_micro_educ
      } else
        if(cat == "Bloco Renda"){
          cat_2 <- input$subcat_idese_micro_renda
        } else{
          cat_2 <- input$subcat_idese_micro_saude
        }
    
    if (!input$checkbox_inclui_rs_ts_idese_micro){ 
      Idese_micro <- main_data %>%
        filter(TIPO_UNID == "Microrregiões" & CATEGORIA == cat_2)
      df_aux <- filter(Idese_micro, NOME %in% micro)} 
    else{                             
      Idese_micro <- main_data %>%
        filter(TIPO_UNID %in% c("Microrregiões","Estado") & CATEGORIA == cat_2)
      df_aux <- filter(Idese_micro, NOME %in% c(micro, "Rio Grande do Sul"))
    }
    
    if(escala){
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
    } else{
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
    }
    
    x_attr <- "Ano"
    
    df_aux_ts <- df_aux %>%
      arrange(NOME,ANO)
    
    plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
            type = 'scatter', mode = 'lines', color = ~NOME,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(NOME, "<br>",
                           "Índice: ", round(VALOR, 3),"<br>",
                           "Ano: ", ANO)) %>%
      layout(title = micro, yaxis = y_attr)
  })
  
  output$ts_vis_idese_meso <- renderPlotly({
    
    meso <- input$idese_meso
    cat <- input$categoria_idese_meso
    escala <- input$fixed_scale_meso

    if(cat == "Idese"){
      cat_2 <- cat
    } else
      if(cat == "Bloco Educação"){
        cat_2 <- input$subcat_idese_meso_educ
      } else
        if(cat == "Bloco Renda"){
          cat_2 <- input$subcat_idese_meso_renda
        } else{
          cat_2 <- input$subcat_idese_meso_saude
        }
    
    if (!input$checkbox_inclui_rs_ts_idese_meso){ 
      Idese_meso <- main_data %>%
        filter(TIPO_UNID == "Mesorregiões" & CATEGORIA == cat_2)
      df_aux <- filter(Idese_meso, NOME %in% meso)} 
    else{                             
      Idese_meso <- main_data %>%
        filter(TIPO_UNID %in% c("Mesorregiões","Estado") & CATEGORIA == cat_2)
      df_aux <- filter(Idese_meso, NOME %in% c(meso, "Rio Grande do Sul"))
    }
    
    if(escala){
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
    } else{
      y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
    }
    
    x_attr <- "Ano"
    
    df_aux_ts <- df_aux %>%
      arrange(NOME,ANO)
    
    plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
            type = 'scatter', mode = 'lines', color = ~NOME,
            hoverinfo="text", ##Tirar tool tips pq formatacao era feia
            text = ~paste0(meso, "<br>",
                           "Índice: ", round(VALOR, 3),"<br>",
                           "Ano: ", ANO)) %>%
      layout(title = meso, yaxis = y_attr)
  })
  

  
  ##SÉRIES TEMPORAIS Idese COMP_INDI
  
    
    output$ts_vis_idese_est_indi <- renderPlotly({
      
      cat <- input$categoria_idese_est_indi
      escala <- input$fixed_scale_est_indicadores
      
      df_aux <- main_data %>%
        filter(TIPO_UNID == "Estado" & CATEGORIA %in% cat)
      
      if(escala){
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
      } else{
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
      }
      
      x_attr <- "Ano"
      
      df_aux_ts <- df_aux %>%
        arrange(CATEGORIA,ANO)
      
      plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
              type = 'scatter', mode = 'lines', color = ~CATEGORIA,
              hoverinfo="text", ##Tirar tool tips pq formatacao era feia
              text = ~paste0(CATEGORIA, "<br>",
                             "Índice: ", round(VALOR, 3),"<br>",
                             "Ano: ", ANO)) %>%
        layout(title = "Rio Grande do Sul", yaxis = y_attr, legend = list(orientation = 'h'))
      
    })
    
    output$ts_vis_idese_mun_indi <- renderPlotly({
      
      cidade <- input$idese_mun_compara
      cat <- input$mun_indicador_compara
      escala <- input$fixed_scale_mun_indicadores
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Municípios" & CATEGORIA %in% cat)
      df_aux <- filter(Idese_mun, NOME == cidade)
      
      if(escala){
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
      } else{
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
      }
      
      x_attr <- "Ano"
      
      df_aux_ts <- df_aux %>%
        arrange(CATEGORIA,ANO)
      
      plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
              type = 'scatter', mode = 'lines', color = ~CATEGORIA,
              hoverinfo="text", ##Tirar tool tips pq formatacao era feia
              text = ~paste0(CATEGORIA, "<br>",
                             "Índice: ", round(VALOR, 3),"<br>",
                             "Ano: ", ANO)) %>%
        layout(title = cidade, yaxis = y_attr, legend = list(orientation = 'h'))
    })

       
    output$ts_vis_idese_cor_indi <- renderPlotly({
      
      cidade <- input$idese_cor_compara
      cat <- input$cor_indicador_compara
      escala <- input$fixed_scale_cor_indicadores
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Coredes. Def: 2011-atual" & CATEGORIA %in% cat)
      df_aux <- filter(Idese_mun, NOME == cidade)
      
      if(escala){
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
      } else{
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
      }
      
      x_attr <- "Ano"
      
      df_aux_ts <- df_aux %>%
        arrange(ANO, CATEGORIA)
      
      plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
              type = 'scatter', mode = 'lines', color = ~CATEGORIA,
              hoverinfo="text", ##Tirar tool tips pq formatacao era feia
              text = ~paste0(CATEGORIA, "<br>",
                             "Índice: ", round(VALOR, 3),"<br>",
                             "Ano: ", ANO)) %>%
        layout(title = cidade, yaxis = y_attr, legend = list(orientation = 'h'))
    })
    
    output$ts_vis_idese_rf_indi <- renderPlotly({
      
      cidade <- input$idese_rf_compara
      cat <- input$rf_indicador_compara
      escala <- input$fixed_scale_rf_indicadores
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Regiões Funcionais. Def: 2008-atual" & CATEGORIA %in% cat)
      df_aux <- filter(Idese_mun, NOME == cidade)
      
      if(escala){
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
      } else{
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
      }
      
      x_attr <- "Ano"
      
      df_aux_ts <- df_aux %>%
        arrange(ANO, CATEGORIA)
      
      plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
              type = 'scatter', mode = 'lines', color = ~CATEGORIA,
              hoverinfo="text", ##Tirar tool tips pq formatacao era feia
              text = ~paste0(CATEGORIA, "<br>",
                             "Índice: ", round(VALOR, 3),"<br>",
                             "Ano: ", ANO)) %>%
        layout(title = cidade, yaxis = y_attr, legend = list(orientation = 'h'))
    })
    
    output$ts_vis_idese_micro_indi <- renderPlotly({
      
      cidade <- input$idese_micro_compara
      cat <- input$micro_indicador_compara
      escala <- input$fixed_scale_micro_indicadores
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Microrregiões" & CATEGORIA %in% cat)
      df_aux <- filter(Idese_mun, NOME == cidade)
      
      if(escala){
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
      } else{
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
      }
      
      x_attr <- "Ano"
      
      df_aux_ts <- df_aux %>%
        arrange(ANO, CATEGORIA)
      
      plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
              type = 'scatter', mode = 'lines', color = ~CATEGORIA,
              hoverinfo="text", ##Tirar tool tips pq formatacao era feia
              text = ~paste0(CATEGORIA, "<br>",
                             "Índice: ", round(VALOR, 3),"<br>",
                             "Ano: ", ANO)) %>%
        layout(title = cidade, yaxis = y_attr, legend = list(orientation = 'h'))
    })
    
    output$ts_vis_idese_meso_indi <- renderPlotly({
      
      cidade <- input$idese_meso_compara
      cat <- input$meso_indicador_compara
      escala <- input$fixed_scale_meso_indicadores
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Mesorregiões" & CATEGORIA %in% cat)
      df_aux <- filter(Idese_mun, NOME == cidade)
      df_aux <- filter(Idese_mun, NOME %in% c(cidade, "Rio Grande do Sul"))
      
      if(escala){
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico", range = c(0,1))
      } else{
        y_attr <- list(title = "Índice de Desenvolvimento Socioeconômico")
      }
      
      x_attr <- "Ano"
      
      df_aux_ts <- df_aux %>%
        arrange(ANO, CATEGORIA)
      
      plot_ly(df_aux_ts, x = ~ANO, y = ~VALOR,
              type = 'scatter', mode = 'lines', color = ~CATEGORIA,
              hoverinfo="text", ##Tirar tool tips pq formatacao era feia
              text = ~paste0(CATEGORIA, "<br>",
                             "Índice: ", round(VALOR, 3),"<br>",
                             "Ano: ", ANO)) %>%
        layout(title = cidade, yaxis = y_attr, legend = list(orientation = 'h'))
    })
  #MAPAS Idese
  
    output$mapa_idese_mun <- renderLeaflet({
      
      year <- input$ano_idese_mun
      cat <- input$mapa_idese_mun
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Municípios" & CATEGORIA == cat)
      
      mapa_db <- Idese_mun %>%
        filter(ANO == year)
      
      if(year < 2013){

        df_mapa <- merge(mapa_RS_old, mapa_db, by.x = "GEOCODIG_M", by.y = "COD", all.x = FALSE)
        
        #gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = df_mapa$VALOR)
        
        classes <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
        pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = df_mapa$VALOR, bins = classes)

      
      leaflet(data = df_mapa) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, fillColor = ~pal_cor(VALOR), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Nome_Munic, "<br>",
                                   "Índice: ", round(df_mapa$VALOR, 3))) %>% 
        addLegend(position = "bottomright", pal = pal_cor,values = ~VALOR)
      }
      else{

        df_mapa <- merge(mapa_RS, mapa_db, by.x  = "GEOCODIGO", by.y = "COD")
        
        #gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = df_mapa$VALOR)
        
        classes <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
        pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = df_mapa$VALOR, bins = classes)

      
      leaflet(data = df_mapa) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, fillColor = ~pal_cor(VALOR), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Cidade, "<br>",
                                   "Índice: ", round(df_mapa$VALOR, 3))) %>% 
        addLegend(position = "bottomright", pal = pal_cor,values = ~VALOR)
      }
      
      
    })
    
    output$mapa_idese_cor <- renderLeaflet({
      
      year <- input$ano_idese_corede
      cat <- input$mapa_idese_cor
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Coredes. Def: 2011-atual" & CATEGORIA == cat)
      
      mapa_db <- Idese_mun %>%
        filter(ANO == year)
      
      df_mapa <- merge(mapa_Cor, mapa_db, by.x = "Corede", by.y = "NOME")
      
      #gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = df_mapa$VALOR)

    classes <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
        pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = df_mapa$VALOR, bins = classes)

      
      leaflet(data = df_mapa) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, fillColor = ~pal_cor(VALOR), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Corede, "<br>",
                                   "Índice: ", round(df_mapa$VALOR, 3))) %>% 
        addLegend(position = "bottomright", pal = pal_cor,values = ~VALOR)
      
    })
    
    output$mapa_idese_rf <- renderLeaflet({
      
      year <- input$ano_idese_rf
      cat <- input$mapa_idese_rf
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Regiões Funcionais. Def: 2008-atual" & CATEGORIA == cat)
      
      mapa_db <- Idese_mun %>%
        filter(ANO == year)
      
      df_mapa <- merge(mapa_RF, mapa_db, by.x = "RF", by.y = "NOME")
      
      #gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = df_mapa$VALOR)
      
      classes <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
        pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = df_mapa$VALOR, bins = classes)

      
      leaflet(data = df_mapa) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, fillColor = ~pal_cor(VALOR), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$RF, "<br>",
                                   "Índice: ", round(df_mapa$VALOR, 3))) %>% 
        addLegend(position = "bottomright", pal = pal_cor,values = ~VALOR)
      
    })

    output$mapa_idese_micro <- renderLeaflet({
      
      year <- input$ano_idese_micro
      cat <- input$mapa_idese_micro
      
      Idese_micro <- main_data %>%
        filter(TIPO_UNID == "Microrregiões" & CATEGORIA == cat)
      
      mapa_db <- Idese_micro %>%
        filter(ANO == year)
      
      df_mapa <- merge(mapa_Micro, mapa_db, by.x = "Micro", by.y = "NOME")
      
      #gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = df_mapa$VALOR)
      
      classes <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
        pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = df_mapa$VALOR, bins = classes)

      
      leaflet(data = df_mapa) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, fillColor = ~pal_cor(VALOR), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Micro, "<br>",
                                   "Índice: ", round(df_mapa$VALOR, 3))) %>% 
        addLegend(position = "bottomright", pal = pal_cor,values = ~VALOR)
      
    })
    
    output$mapa_idese_meso <- renderLeaflet({
      
      year <- input$ano_idese_meso
      cat <- input$mapa_idese_meso
      
      Idese_meso <- main_data %>%
        filter(TIPO_UNID == "Mesorregiões" & CATEGORIA == cat)
      
      mapa_db <- Idese_meso %>%
        filter(ANO == year)
      
      df_mapa <- merge(mapa_Meso, mapa_db, by.x = "Meso", by.y = "NOME")
      
      #gradiente = colorNumeric(c("#720000", "Red", "yellow", "#00f010", "#149800"), domain = df_mapa$VALOR)
      
      classes <- c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
        pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = df_mapa$VALOR, bins = classes)

      
      leaflet(data = df_mapa) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, fillColor = ~pal_cor(VALOR), # Weight e a grossura das bordas
                    color = "grey", fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(df_mapa$Meso, "<br>",
                                   "Índice: ", round(df_mapa$VALOR, 3))) %>% 
        addLegend(position = "bottomright", pal = pal_cor,values = ~VALOR)
      
    })
  #SCATTER Idese
  
    output$scat_idese_mun <- renderPlotly({
      
      year <- input$ano_idese_mun_scat
      var_x <- input$mun_var_x
      var_y <- input$mun_var_y
      var_color <- input$mun_var_color
      escala <- input$fixed_scale_scat_mun
      # var_size <- input$mun_var_size
      

      if(escala){
      		y_attr <- list(title = paste0(var_y), range = c(0,1))
      		x_attr <- list(title = paste0(var_x), range = c(0,1))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		} else{
			y_attr <- list(title = paste0(var_y))
      		x_attr <- list(title = paste0(var_x))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		}

      z_attr <- list(colorbar = list(title = var_color))
      z_attr_hover <- list(title = paste0(var_color))
      # k_attr <- list(title = paste0(var_size))
      
      Idese_mun <- main_data %>%
                   filter(TIPO_UNID == "Municípios" & ANO == year)
      
      eixo_x <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == var_x])
      eixo_x[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == var_x])
      colnames(eixo_x) <- c("V1", "V_X")
      
      eixo_y <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == var_y])
      eixo_y[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == var_y])
      colnames(eixo_y) <- c("V1", "V_Y")
      
      eixo_z <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == var_color])
      eixo_z[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == var_color])
      colnames(eixo_z) <- c("V1", "V_Z")
      
      eixo_k <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == "População"])
      eixo_k[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == "População"])
      colnames(eixo_k) <- c("V1", "V_K")
      
      tb <- inner_join(eixo_x, eixo_y, by = "V1")
      tb2 <- inner_join(tb, eixo_z, by = "V1")
      tb3 <- inner_join(tb2, eixo_k, by = "V1")
      
      #gradiente = colorNumeric(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = c(0,1))

      colnames(tb3) <- c("V1", var_x, var_y, var_color, "POP")
      
      plot_ly(data=tb3, x = ~get(var_x), y = ~get(var_y), size = ~POP,
              type = 'scatter',
              mode = 'markers',
              marker = z_attr,
              sizes = c(20, 1000),
              hoverinfo="text", # Para tirar os tool tips pq a formatacao era feia
              text = ~paste0(V1, "<br>",
                             x_attr_hover,": ", round(get(var_x), 3), "<br>",
                             y_attr_hover,": ", round(get(var_y), 3), "<br>",
                             z_attr_hover,": ", round(get(var_color), 3), "<br>",
                             "População: ", round(POP, 3), "<br>"), 
              colors=c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"),
              color = ~get(var_color)) %>%
        layout(title = "Gráfico de Dispersão", yaxis = y_attr, xaxis = x_attr)
    })
    
    output$scat_idese_cor <- renderPlotly({
      
      year <- input$ano_idese_corede_scat
      var_x <- input$corede_var_x
      var_y <- input$corede_var_y
      var_color <- input$corede_var_color
      escala <- input$fixed_scale_scat_cor
      # var_size <- input$corede_var_size
      
      if(escala){
      		y_attr <- list(title = paste0(var_y), range = c(0,1))
      		x_attr <- list(title = paste0(var_x), range = c(0,1))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		} else{
			y_attr <- list(title = paste0(var_y))
      		x_attr <- list(title = paste0(var_x))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		}

      z_attr <- list(colorbar = list(title = var_color))
      z_attr_hover <- list(title = paste0(var_color))
      # k_attr <- list(title = paste0(var_size))
      
      Idese_cor <- main_data %>%
                   filter(TIPO_UNID == "Coredes. Def: 2011-atual" & ANO == year)
      
      eixo_x <- as.data.frame(Idese_cor$NOME[Idese_cor$CATEGORIA == var_x])
      eixo_x[,2] <- as.data.frame(Idese_cor$VALOR[Idese_cor$CATEGORIA == var_x])
      colnames(eixo_x) <- c("V1", "V_X")
      
      eixo_y <- as.data.frame(Idese_cor$NOME[Idese_cor$CATEGORIA == var_y])
      eixo_y[,2] <- as.data.frame(Idese_cor$VALOR[Idese_cor$CATEGORIA == var_y])
      colnames(eixo_y) <- c("V1", "V_Y")
      
      eixo_z <- as.data.frame(Idese_cor$NOME[Idese_cor$CATEGORIA == var_color])
      eixo_z[,2] <- as.data.frame(Idese_cor$VALOR[Idese_cor$CATEGORIA == var_color])
      colnames(eixo_z) <- c("V1", "V_Z")
      
      eixo_k <- as.data.frame(Idese_cor$NOME[Idese_cor$CATEGORIA == "População"])
      eixo_k[,2] <- as.data.frame(Idese_cor$VALOR[Idese_cor$CATEGORIA == "População"])
      colnames(eixo_k) <- c("V1", "V_K")
      
      tb <- inner_join(eixo_x, eixo_y, by = "V1")
      tb2 <- inner_join(tb, eixo_z, by = "V1")
      tb3 <- inner_join(tb2, eixo_k, by = "V1")
      
      colnames(tb3) <- c("V1", var_x, var_y, var_color, "POP")
      
      plot_ly(data = tb3, x = ~get(var_x),y = ~get(var_y), size = ~POP,
              type = 'scatter',
              mode = 'markers',
              marker = z_attr ,
              sizes = c(20, 1000),
              hoverinfo="text", # Para tirar os tool tips pq a formatacao era feia
              text = ~paste0(V1, "<br>",
                             x_attr_hover, ":", round(get(var_x), 3), "<br>",
                             y_attr_hover, ":", round(get(var_y), 3), "<br>",
                             z_attr_hover, ":", round(get(var_color), 3), "<br>",
                             "População: ", round(POP,3), "<br>"), 
              colors=c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"),
              color = ~get(var_color)) %>%
        layout(title = "Gráfico de Dispersão", yaxis = y_attr, xaxis = x_attr)
    })
    
    output$scat_idese_rf <- renderPlotly({
      
      year <- input$ano_idese_rf_scat
      var_x <- input$rf_var_x
      var_y <- input$rf_var_y
      var_color <- input$rf_var_color
      escala <- input$fixed_scale_scat_rf

      # var_size <- input$rf_var_size
      
      if(escala){
      		y_attr <- list(title = paste0(var_y), range = c(0,1))
      		x_attr <- list(title = paste0(var_x), range = c(0,1))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		} else{
			y_attr <- list(title = paste0(var_y))
      		x_attr <- list(title = paste0(var_x))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		}


      z_attr <- list(colorbar = list(title = var_color))
      z_attr_hover <- list(title = paste0(var_color))
      # k_attr <- list(title = paste0(var_size))
      
      Idese_cor <- main_data %>%
        filter(TIPO_UNID == "Regiões Funcionais. Def: 2008-atual" & ANO == year)
      
      eixo_x <- as.data.frame(Idese_cor$NOME[Idese_cor$CATEGORIA == var_x])
      eixo_x[,2] <- as.data.frame(Idese_cor$VALOR[Idese_cor$CATEGORIA == var_x])
      colnames(eixo_x) <- c("V1", "V_X")
      
      eixo_y <- as.data.frame(Idese_cor$NOME[Idese_cor$CATEGORIA == var_y])
      eixo_y[,2] <- as.data.frame(Idese_cor$VALOR[Idese_cor$CATEGORIA == var_y])
      colnames(eixo_y) <- c("V1", "V_Y")
      
      eixo_z <- as.data.frame(Idese_cor$NOME[Idese_cor$CATEGORIA == var_color])
      eixo_z[,2] <- as.data.frame(Idese_cor$VALOR[Idese_cor$CATEGORIA == var_color])
      colnames(eixo_z) <- c("V1", "V_Z")
      
      eixo_k <- as.data.frame(Idese_cor$NOME[Idese_cor$CATEGORIA == "População"])
      eixo_k[,2] <- as.data.frame(Idese_cor$VALOR[Idese_cor$CATEGORIA == "População"])
      colnames(eixo_k) <- c("V1", "V_K")
      
      tb <- inner_join(eixo_x, eixo_y, by = "V1")
      tb2 <- inner_join(tb, eixo_z, by = "V1")
      tb3 <- inner_join(tb2, eixo_k, by = "V1")
      
      colnames(tb3) <- c("V1", var_x, var_y, var_color, "POP")
      
      plot_ly(data = tb3, x = ~get(var_x),y = ~get(var_y), size = ~POP,
              type = 'scatter',
              mode = 'markers',
              marker = z_attr,
              sizes = c(20, 1000),
              hoverinfo="text", # Para tirar os tool tips pq a formatacao era feia
              text = ~paste0(V1, "<br>",
                             x_attr_hover, ":", round(get(var_x), 3), "<br>",
                             y_attr_hover, ":", round(get(var_y), 3), "<br>",
                             z_attr_hover, ":", round(get(var_color), 3), "<br>",
                             "População: ", round(POP,3), "<br>"), 
              colors=c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"),
              color = ~get(var_color)) %>%
        layout(title = "Gráfico de Dispersão", yaxis = y_attr, xaxis = x_attr)
    })
    
    output$scat_idese_micro <- renderPlotly({
      
      year <- input$ano_idese_micro_scat
      var_x <- input$micro_var_x
      var_y <- input$micro_var_y
      var_color <- input$micro_var_color
      escala <- input$fixed_scale_scat_micro

      # var_size <- input$micro_var_size
      
      if(escala){
      		y_attr <- list(title = paste0(var_y), range = c(0,1))
      		x_attr <- list(title = paste0(var_x), range = c(0,1))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		} else{
			y_attr <- list(title = paste0(var_y))
      		x_attr <- list(title = paste0(var_x))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		}


      z_attr <- list(colorbar = list(title = var_color))
      z_attr_hover <- list(title = paste0(var_color))
      # k_attr <- list(title = paste0(var_size))
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Microrregiões" & ANO == year)
      
      eixo_x <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == var_x])
      eixo_x[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == var_x])
      colnames(eixo_x) <- c("V1", "V_X")
      
      eixo_y <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == var_y])
      eixo_y[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == var_y])
      colnames(eixo_y) <- c("V1", "V_Y")
      
      eixo_z <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == var_color])
      eixo_z[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == var_color])
      colnames(eixo_z) <- c("V1", "V_Z")
      
      eixo_k <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == "População"])
      eixo_k[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == "População"])
      colnames(eixo_k) <- c("V1", "V_K")
      
      tb <- inner_join(eixo_x, eixo_y, by = "V1")
      tb2 <- inner_join(tb, eixo_z, by = "V1")
      tb3 <- inner_join(tb2, eixo_k, by = "V1")
      
      colnames(tb3) <- c("V1", var_x, var_y, var_color, "POP")
      
      plot_ly(data=tb3, x = ~get(var_x), y = ~get(var_y), size = ~POP,
              type = 'scatter',
              mode = 'markers',
              marker = z_attr,
              sizes = c(20, 1000),
              hoverinfo="text", # Para tirar os tool tips pq a formatacao era feia
              text = ~paste0(V1, "<br>",
                             x_attr_hover,": ", round(get(var_x), 3), "<br>",
                             y_attr_hover,": ", round(get(var_y), 3), "<br>",
                             z_attr_hover,": ", round(get(var_color), 3), "<br>",
                             "População: ", round(POP, 3), "<br>"), 
              colors=c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"),
              color = ~get(var_color)) %>%
        layout(title = "Gráfico de Dispersão", yaxis = y_attr, xaxis = x_attr)
    })
    
    output$scat_idese_meso <- renderPlotly({
      
      year <- input$ano_idese_meso_scat
      var_x <- input$meso_var_x
      var_y <- input$meso_var_y
      var_color <- input$meso_var_color
      escala <- input$fixed_scale_scat_meso
      # var_size <- input$meso_var_size
      
      if(escala){
      		y_attr <- list(title = paste0(var_y), range = c(0,1))
      		x_attr <- list(title = paste0(var_x), range = c(0,1))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		} else{
			y_attr <- list(title = paste0(var_y))
      		x_attr <- list(title = paste0(var_x))
      		y_attr_hover <- list(title = paste0(var_y))
      		x_attr_hover <- list(title = paste0(var_x))
		}


      z_attr <- list(colorbar = list(title = var_color))
      z_attr_hover <- list(title = paste0(var_color))
      # k_attr <- list(title = paste0(var_size))
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Mesorregiões" & ANO == year)
      
      eixo_x <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == var_x])
      eixo_x[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == var_x])
      colnames(eixo_x) <- c("V1", "V_X")
      
      eixo_y <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == var_y])
      eixo_y[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == var_y])
      colnames(eixo_y) <- c("V1", "V_Y")
      
      eixo_z <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == var_color])
      eixo_z[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == var_color])
      colnames(eixo_z) <- c("V1", "V_Z")
      
      eixo_k <- as.data.frame(Idese_mun$NOME[Idese_mun$CATEGORIA == "População"])
      eixo_k[,2] <- as.data.frame(Idese_mun$VALOR[Idese_mun$CATEGORIA == "População"])
      colnames(eixo_k) <- c("V1", "POP")
      
      tb <- inner_join(eixo_x, eixo_y, by = "V1")
      tb2 <- inner_join(tb, eixo_z, by = "V1")
      tb3 <- inner_join(tb2, eixo_k, by = "V1")
      
      colnames(tb3) <- c("V1", var_x, var_y, var_color, "POP")
      
      plot_ly(data=tb3, x = ~get(var_x), y = ~get(var_y), size = ~POP,
              type = 'scatter',
              mode = 'markers',
              marker = z_attr,
              sizes = c(20, 1000),
              hoverinfo="text", # Para tirar os tool tips pq a formatacao era feia
              text = ~paste0(V1, "<br>",
                             x_attr_hover,": ", round(get(var_x), 3), "<br>",
                             y_attr_hover,": ", round(get(var_y), 3), "<br>",
                             z_attr_hover,": ", round(get(var_color), 3), "<br>",
                             "População: ", round(POP, 3), "<br>"), 
              colors=c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"),
              color = ~get(var_color)) %>%
        layout(title = "Gráfico de Dispersão", yaxis = y_attr, xaxis = x_attr) 
    })
    
  #TABELA DINÂMICA Idese
  
    output$table_idese_mun <- renderFormattable({
      
      year <- input$ano_idese_mun_table
      slider_pop_min <-input$table_mun_pop_control[1]
      slider_pop_max <- input$table_mun_pop_control[2]
      
      Idese_mun <- main_data %>%
                   filter(TIPO_UNID == "Municípios" & ANO == year)
      
      tb_zinho <- as.data.frame(unique(Idese_mun$NOME))
      tb_zinho[,2] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Idese"]))
      tb_zinho[,3] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Educação"]))
      tb_zinho[,4] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Renda"]))
      tb_zinho[,5] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Saúde"]))
      
      tb_pop <- Idese_mun %>%
                select(NOME, CATEGORIA, VALOR) %>%
                filter(CATEGORIA == "População")
      
      colnames(tb_pop) <- c("NOME", "Delete" ,"População")
      
      tb_pop <- tb_pop %>%
                select(NOME, População)
      
      colnames(tb_zinho) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde")
      
      tb_zao <- inner_join(tb_zinho, tb_pop, by = "NOME")
      
      tb <- tb_zao %>%
            filter(População > slider_pop_min & População < slider_pop_max)
      
      
      
      tb$População <- NULL
      
      if(input$inverte_ordem_mun == F){
        if(input$ranking_table_mun == "ranking_nome_mun"){
          tb <- tb %>%
            arrange(NOME)
        } else
          if(input$ranking_table_mun == "ranking_educ_mun"){
            tb <- tb %>%
              arrange(desc(tb$"Bloco Educação"))
          }else
            if(input$ranking_table_mun == "ranking_renda_mun"){
              tb <- tb %>%
                arrange(desc(tb$"Bloco Renda"))
            }else
              if(input$ranking_table_mun == "ranking_saude_mun"){
                tb <- tb %>%
                  arrange(desc(tb$"Bloco Saúde"))
              }else{
                tb <- tb %>%
                  arrange(desc(Idese))
              }
        
        
        if(input$ranking_escolha_mun == "escolhe_nome_mun"){
          tb <- tb %>% 
            mutate(Ranking = rank(tb$NOME))
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome dos Municípios")
        } else
          if(input$ranking_escolha_mun == "escolhe_idese_mun"){
            tb <- tb %>%
              mutate(Ranking = rank(desc(tb$"Idese")))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_mun == "escolhe_saude_mun"){
              tb <- tb %>%
                mutate(Ranking = rank(desc(tb$"Bloco Saúde")))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_mun == "escolhe_renda_mun"){
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Renda")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Educação")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
      } else {
        if(input$ranking_table_mun == "ranking_nome_mun"){
          tb <- tb %>%
            arrange(desc(NOME))
        } else
          if(input$ranking_table_mun == "ranking_educ_mun"){
            tb <- tb %>%
              arrange(tb$"Bloco Educação")
          }else
            if(input$ranking_table_mun == "ranking_renda_mun"){
              tb <- tb %>%
                arrange(tb$"Bloco Renda")
            }else
              if(input$ranking_table_mun == "ranking_saude_mun"){
                tb <- tb %>%
                  arrange(tb$"Bloco Saúde")
              }else{
                tb <- tb %>%
                  arrange(Idese)
              }
        
        
        if(input$ranking_escolha_mun == "escolhe_nome_mun"){
          tb <- tb %>% 
            mutate(Ranking = rank(desc(tb$NOME)),0)
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome dos Municípios")
        } else
          if(input$ranking_escolha_mun == "escolhe_idese_mun"){
            tb <- tb %>%
              mutate(Ranking = rank(tb$"Idese"))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_mun == "escolhe_saude_mun"){
              tb <- tb %>%
                mutate(Ranking = rank(tb$"Bloco Saúde"))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_mun == "escolhe_renda_mun"){
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Renda"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Educação"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
      }

      tb$Idese <- round(tb$Idese, 3)
      tb$"Bloco Educação" <- round(tb$"Bloco Educação", 3)
      tb$"Bloco Renda" <- round(tb$"Bloco Renda", 3)
      tb$"Bloco Saúde" <- round(tb$"Bloco Saúde", 3)
      
      fixedWidth = 100
      
      formattable(tb, list(
        Idese = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                   direction = "rtl", `border-radius` = "4px", 
                                                                   `padding-right` = "2px", `background-color` = csscolor("#ddbaf6"),
                                                                   width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Saúde" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                           direction = "rtl", `border-radius` = "4px", 
                                                                           `padding-right` = "2px", `background-color` = csscolor("#f9c2c2"),
                                                                           width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Educação" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                              direction = "rtl", `border-radius` = "4px", 
                                                                              `padding-right` = "2px", `background-color` = csscolor("#a6ddff"),
                                                                              width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Renda" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                           direction = "rtl", `border-radius` = "4px", 
                                                                           `padding-right` = "2px", `background-color` = csscolor("#fbceb1"),
                                                                           width = paste0(fixedWidth*proportion(x), "px", sep = "")))
      ))
      
      
    })
    
    output$table_idese_corede <- renderFormattable({
      
      year <- input$ano_idese_corede_table
      slider_pop_min <-input$table_cor_pop_control[1]
      slider_pop_max <- input$table_cor_pop_control[2]
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Coredes. Def: 2011-atual" & ANO == year)
      
      tb_zinho <- as.data.frame(unique(Idese_mun$NOME))
      tb_zinho[,2] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Idese"]))
      tb_zinho[,3] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Educação"]))
      tb_zinho[,4] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Renda"]))
      tb_zinho[,5] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Saúde"]))
      
      tb_pop <- Idese_mun %>%
        select(NOME, CATEGORIA, VALOR) %>%
        filter(CATEGORIA == "População")
      
      colnames(tb_pop) <- c("NOME", "Delete" ,"População")
      
      tb_pop <- tb_pop %>%
        select(NOME, População)
      
      colnames(tb_zinho) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde")
      
      tb_zao <- inner_join(tb_zinho, tb_pop, by = "NOME")
      
      tb <- tb_zao %>%
        filter(População > slider_pop_min & População < slider_pop_max)
      
      
      
      tb$População <- NULL
      
      if(input$inverte_ordem_corede == F){
        if(input$ranking_table_cor == "ranking_nome_cor"){
          tb <- tb %>%
            arrange(NOME)
        } else
          if(input$ranking_table_cor == "ranking_educ_cor"){
            tb <- tb %>%
              arrange(desc(tb$"Bloco Educação"))
          }else
            if(input$ranking_table_cor == "ranking_renda_cor"){
              tb <- tb %>%
                arrange(desc(tb$"Bloco Renda"))
            }else
              if(input$ranking_table_cor == "ranking_saude_cor"){
                tb <- tb %>%
                  arrange(desc(tb$"Bloco Saúde"))
              }else{
                tb <- tb %>%
                  arrange(desc(Idese))
              }
        
        
        if(input$ranking_escolha_cor == "escolhe_nome_cor"){
          tb <- tb %>% 
            mutate(Ranking = rank(tb$NOME))
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome dos Coredes")
        } else
          if(input$ranking_escolha_cor == "escolhe_idese_cor"){
            tb <- tb %>%
              mutate(Ranking = rank(desc(tb$"Idese")))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_cor == "escolhe_saude_cor"){
              tb <- tb %>%
                mutate(Ranking = rank(desc(tb$"Bloco Saúde")))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_cor == "escolhe_renda_cor"){
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Renda")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Educação")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
      } else {
        if(input$ranking_table_cor == "ranking_nome_cor"){
          tb <- tb %>%
            arrange(desc(NOME))
        } else
          if(input$ranking_table_cor == "ranking_educ_cor"){
            tb <- tb %>%
              arrange(tb$"Bloco Educação")
          }else
            if(input$ranking_table_cor == "ranking_renda_cor"){
              tb <- tb %>%
                arrange(tb$"Bloco Renda")
            }else
              if(input$ranking_table_cor == "ranking_saude_cor"){
                tb <- tb %>%
                  arrange(tb$"Bloco Saúde")
              }else{
                tb <- tb %>%
                  arrange(Idese)
              }
        
        
        if(input$ranking_escolha_cor == "escolhe_nome_cor"){
          tb <- tb %>% 
            mutate(Ranking = rank(desc(tb$NOME)))
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome dos Coredes")
        } else
          if(input$ranking_escolha_cor == "escolhe_idese_cor"){
            tb <- tb %>%
              mutate(Ranking = rank(tb$"Idese"))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_cor == "escolhe_saude_cor"){
              tb <- tb %>%
                mutate(Ranking = rank(tb$"Bloco Saúde"))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_cor == "escolhe_renda_cor"){
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Renda"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Educação"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
      }
      
	  tb$Idese <- round(tb$Idese, 3)
      tb$"Bloco Educação" <- round(tb$"Bloco Educação", 3)
      tb$"Bloco Renda" <- round(tb$"Bloco Renda", 3)
      tb$"Bloco Saúde" <- round(tb$"Bloco Saúde", 3)

      fixedWidth = 100
      
      formattable(tb, list(
        Idese = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                   direction = "rtl", `border-radius` = "4px", 
                                                                   `padding-right` = "2px", `background-color` = csscolor("#ddbaf6"),
                                                                   width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Saúde" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                           direction = "rtl", `border-radius` = "4px", 
                                                                           `padding-right` = "2px", `background-color` = csscolor("#f9c2c2"),
                                                                           width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Educação" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                              direction = "rtl", `border-radius` = "4px", 
                                                                              `padding-right` = "2px", `background-color` = csscolor("#a6ddff"),
                                                                              width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Renda" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                           direction = "rtl", `border-radius` = "4px", 
                                                                           `padding-right` = "2px", `background-color` = csscolor("#fbceb1"),
                                                                           width = paste0(fixedWidth*proportion(x), "px", sep = "")))
      ))
      
    })
    
    output$table_idese_rf <- renderFormattable({
      
      year <- input$ano_idese_rf_table
      slider_pop_min <-input$table_rf_pop_control[1]
      slider_pop_max <- input$table_rf_pop_control[2]
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Regiões Funcionais. Def: 2008-atual" & ANO == year)
      
      tb_zinho <- as.data.frame(unique(Idese_mun$NOME))
      tb_zinho[,2] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Idese"]))
      tb_zinho[,3] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Educação"]))
      tb_zinho[,4] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Renda"]))
      tb_zinho[,5] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Saúde"]))
      
      tb_pop <- Idese_mun %>%
        select(NOME, CATEGORIA, VALOR) %>%
        filter(CATEGORIA == "População")
      
      colnames(tb_pop) <- c("NOME", "Delete" ,"População")
      
      tb_pop <- tb_pop %>%
        select(NOME, População)
      
      colnames(tb_zinho) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde")
      
      tb_zao <- inner_join(tb_zinho, tb_pop, by = "NOME")
      
      tb <- tb_zao %>%
        filter(População > slider_pop_min & População < slider_pop_max)
      
      
      
      tb$População <- NULL
      
      if(input$inverte_ordem_rf == F){
        if(input$ranking_table_rf == "ranking_nome_rf"){
          tb <- tb %>%
            arrange(NOME)
        } else
          if(input$ranking_table_rf == "ranking_educ_rf"){
            tb <- tb %>%
              arrange(desc(tb$"Bloco Educação"))
          }else
            if(input$ranking_table_rf == "ranking_renda_rf"){
              tb <- tb %>%
                arrange(desc(tb$"Bloco Renda"))
            }else
              if(input$ranking_table_rf == "ranking_saude_rf"){
                tb <- tb %>%
                  arrange(desc(tb$"Bloco Saúde"))
              }else{
                tb <- tb %>%
                  arrange(desc(Idese))
              }
        
        
        if(input$ranking_escolha_rf == "escolhe_nome_rf"){
          tb <- tb %>% 
            mutate(Ranking = rank(tb$NOME))
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome das Regiões Funcionais")
        } else
          if(input$ranking_escolha_rf == "escolhe_idese_rf"){
            tb <- tb %>%
              mutate(Ranking = rank(desc(tb$"Idese")))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_rf == "escolhe_saude_rf"){
              tb <- tb %>%
                mutate(Ranking = rank(desc(tb$"Bloco Saúde")))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_rf == "escolhe_renda_rf"){
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Renda")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Educação")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
      } else {
        if(input$ranking_table_rf == "ranking_nome_rf"){
          tb <- tb %>%
            arrange(desc(NOME))
        } else
          if(input$ranking_table_rf == "ranking_educ_rf"){
            tb <- tb %>%
              arrange(tb$"Bloco Educação")
          }else
            if(input$ranking_table_rf == "ranking_renda_rf"){
              tb <- tb %>%
                arrange(tb$"Bloco Renda")
            }else
              if(input$ranking_table_rf == "ranking_saude_rf"){
                tb <- tb %>%
                  arrange(tb$"Bloco Saúde")
              }else{
                tb <- tb %>%
                  arrange(Idese)
              }
        
        
        if(input$ranking_escolha_rf == "escolhe_nome_rf"){
          tb <- tb %>% 
            mutate(Ranking = rank(desc(tb$NOME)))
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome das Regiões Funcionais")
        } else
          if(input$ranking_escolha_rf == "escolhe_idese_rf"){
            tb <- tb %>%
              mutate(Ranking = rank(tb$"Idese"))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_rf == "escolhe_saude_rf"){
              tb <- tb %>%
                mutate(Ranking = rank(tb$"Bloco Saúde"))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_rf == "escolhe_renda_rf"){
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Renda"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Educação"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
      }
      
      tb$Idese <- round(tb$Idese, 3)
      tb$"Bloco Educação" <- round(tb$"Bloco Educação", 3)
      tb$"Bloco Renda" <- round(tb$"Bloco Renda", 3)
      tb$"Bloco Saúde" <- round(tb$"Bloco Saúde", 3)

      fixedWidth = 100
      
      formattable(tb, list(
        Idese = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                   direction = "rtl", `border-radius` = "4px", 
                                                                   `padding-right` = "2px", `background-color` = csscolor("#ddbaf6"),
                                                                   width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Saúde" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                           direction = "rtl", `border-radius` = "4px", 
                                                                           `padding-right` = "2px", `background-color` = csscolor("#f9c2c2"),
                                                                           width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Educação" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                              direction = "rtl", `border-radius` = "4px", 
                                                                              `padding-right` = "2px", `background-color` = csscolor("#a6ddff"),
                                                                              width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Renda" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                           direction = "rtl", `border-radius` = "4px", 
                                                                           `padding-right` = "2px", `background-color` = csscolor("#fbceb1"),
                                                                           width = paste0(fixedWidth*proportion(x), "px", sep = "")))
      ))
      
    })
    
    output$table_idese_micro <- renderFormattable({
      
      year <- input$ano_idese_micro_table
      slider_pop_min <-input$table_micro_pop_control[1]
      slider_pop_max <- input$table_micro_pop_control[2]
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Microrregiões" & ANO == year)
      
      tb_zinho <- as.data.frame(unique(Idese_mun$NOME))
      tb_zinho[,2] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Idese"]))
      tb_zinho[,3] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Educação"]))
      tb_zinho[,4] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Renda"]))
      tb_zinho[,5] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Saúde"]))
      
      tb_pop <- Idese_mun %>%
        select(NOME, CATEGORIA, VALOR) %>%
        filter(CATEGORIA == "População")
      
      colnames(tb_pop) <- c("NOME", "Delete" ,"População")
      
      tb_pop <- tb_pop %>%
        select(NOME, População)
      
      colnames(tb_zinho) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde")
      
      tb_zao <- inner_join(tb_zinho, tb_pop, by = "NOME")
      
      tb <- tb_zao %>%
        filter(População > slider_pop_min & População < slider_pop_max)
      
      
      
      tb$População <- NULL
      
      if(input$inverte_ordem_micro == F){
        if(input$ranking_table_micro == "ranking_nome_micro"){
          tb <- tb %>%
            arrange(NOME)
        } else
          if(input$ranking_table_micro == "ranking_educ_micro"){
            tb <- tb %>%
              arrange(desc(tb$"Bloco Educação"))
          }else
            if(input$ranking_table_micro == "ranking_renda_micro"){
              tb <- tb %>%
                arrange(desc(tb$"Bloco Renda"))
            }else
              if(input$ranking_table_micro == "ranking_saude_micro"){
                tb <- tb %>%
                  arrange(desc(tb$"Bloco Saúde"))
              }else{
                tb <- tb %>%
                  arrange(desc(Idese))
              }
        
        
        if(input$ranking_escolha_micro == "escolhe_nome_micro"){
          tb <- tb %>% 
            mutate(Ranking = rank(tb$NOME))
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome das Microrregiões")
        } else
          if(input$ranking_escolha_micro == "escolhe_idese_micro"){
            tb <- tb %>%
              mutate(Ranking = rank(desc(tb$"Idese")))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_micro == "escolhe_saude_micro"){
              tb <- tb %>%
                mutate(Ranking = rank(desc(tb$"Bloco Saúde")))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_micro == "escolhe_renda_micro"){
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Renda")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Educação")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
      } else {
        if(input$ranking_table_micro == "ranking_nome_micro"){
          tb <- tb %>%
            arrange(desc(NOME))
        } else
          if(input$ranking_table_micro == "ranking_educ_micro"){
            tb <- tb %>%
              arrange(tb$"Bloco Educação")
          }else
            if(input$ranking_table_micro == "ranking_renda_micro"){
              tb <- tb %>%
                arrange(tb$"Bloco Renda")
            }else
              if(input$ranking_table_micro == "ranking_saude_micro"){
                tb <- tb %>%
                  arrange(tb$"Bloco Saúde")
              }else{
                tb <- tb %>%
                  arrange(Idese)
              }
        
        
        if(input$ranking_escolha_micro == "escolhe_nome_micro"){
          tb <- tb %>% 
            mutate(Ranking = rank(desc(tb$NOME)))
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome das Microrregiões")
        } else
          if(input$ranking_escolha_micro == "escolhe_idese_micro"){
            tb <- tb %>%
              mutate(Ranking = rank(tb$"Idese"))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_micro == "escolhe_saude_micro"){
              tb <- tb %>%
                mutate(Ranking = rank(tb$"Bloco Saúde"))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_micro == "escolhe_renda_micro"){
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Renda"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Educação"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
      }

      tb$Idese <- round(tb$Idese, 3)
      tb$"Bloco Educação" <- round(tb$"Bloco Educação", 3)
      tb$"Bloco Renda" <- round(tb$"Bloco Renda", 3)
      tb$"Bloco Saúde" <- round(tb$"Bloco Saúde", 3)
      
      fixedWidth = 100
      
      formattable(tb, list(
        Idese = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                   direction = "rtl", `border-radius` = "4px", 
                                                                   `padding-right` = "2px", `background-color` = csscolor("#ddbaf6"),
                                                                   width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Saúde" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                           direction = "rtl", `border-radius` = "4px", 
                                                                           `padding-right` = "2px", `background-color` = csscolor("#f9c2c2"),
                                                                           width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Educação" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                              direction = "rtl", `border-radius` = "4px", 
                                                                              `padding-right` = "2px", `background-color` = csscolor("#a6ddff"),
                                                                              width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Renda" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                           direction = "rtl", `border-radius` = "4px", 
                                                                           `padding-right` = "2px", `background-color` = csscolor("#fbceb1"),
                                                                           width = paste0(fixedWidth*proportion(x), "px", sep = "")))
      ))
      
    })
    
    output$table_idese_meso <- renderFormattable({
      
      year <- input$ano_idese_meso_table
      slider_pop_min <-input$table_meso_pop_control[1]
      slider_pop_max <- input$table_meso_pop_control[2]
      
      Idese_mun <- main_data %>%
        filter(TIPO_UNID == "Regiões Funcionais. Def: 2008-atual" & ANO == year)
      
      tb_zinho <- as.data.frame(unique(Idese_mun$NOME))
      tb_zinho[,2] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Idese"]))
      tb_zinho[,3] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Educação"]))
      tb_zinho[,4] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Renda"]))
      tb_zinho[,5] <- as.data.frame(unique(Idese_mun$VALOR[Idese_mun$CATEGORIA == "Bloco Saúde"]))
      
      tb_pop <- Idese_mun %>%
        select(NOME, CATEGORIA, VALOR) %>%
        filter(CATEGORIA == "População")
      
      colnames(tb_pop) <- c("NOME", "Delete" ,"População")
      
      tb_pop <- tb_pop %>%
        select(NOME, População)
      
      colnames(tb_zinho) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde")
      
      tb_zao <- inner_join(tb_zinho, tb_pop, by = "NOME")
      
      tb <- tb_zao %>%
        filter(População > slider_pop_min & População < slider_pop_max)
      
      
      tb$População <- NULL
      
      if(input$inverte_ordem_meso == F){
        if(input$ranking_table_meso == "ranking_nome_meso"){
          tb <- tb %>%
            arrange(NOME)
        } else
          if(input$ranking_table_meso == "ranking_educ_meso"){
            tb <- tb %>%
              arrange(desc(tb$"Bloco Educação"))
          }else
            if(input$ranking_table_meso == "ranking_renda_meso"){
              tb <- tb %>%
                arrange(desc(tb$"Bloco Renda"))
            }else
              if(input$ranking_table_meso == "ranking_saude_meso"){
                tb <- tb %>%
                  arrange(desc(tb$"Bloco Saúde"))
              }else{
                tb <- tb %>%
                  arrange(desc(Idese))
              }
        
        
        if(input$ranking_escolha_meso == "escolhe_nome_meso"){
          tb <- tb %>% 
            mutate(Ranking = rank(tb$NOME))
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome das Mesorregiões")
        } else
          if(input$ranking_escolha_meso == "escolhe_idese_meso"){
            tb <- tb %>%
              mutate(Ranking = rank(desc(tb$"Idese")))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_meso == "escolhe_saude_meso"){
              tb <- tb %>%
                mutate(Ranking = rank(desc(tb$"Bloco Saúde")))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_meso == "escolhe_renda_meso"){
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Renda")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(desc(tb$"Bloco Educação")))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
      } else {
        if(input$ranking_table_meso == "ranking_nome_meso"){
          tb <- tb %>%
            arrange(desc(NOME))
        } else
          if(input$ranking_table_meso == "ranking_educ_meso"){
            tb <- tb %>%
              arrange(tb$"Bloco Educação")
          }else
            if(input$ranking_table_meso == "ranking_renda_meso"){
              tb <- tb %>%
                arrange(tb$"Bloco Renda")
            }else
              if(input$ranking_table_meso == "ranking_saude_meso"){
                tb <- tb %>%
                  arrange(tb$"Bloco Saúde")
              }else{
                tb <- tb %>%
                  arrange(Idese)
              }
        
        
        if(input$ranking_escolha_meso == "escolhe_nome_meso"){
          tb <- tb %>% 
            mutate(Ranking = rank(desc(tb$NOME)))
          
          colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Nome das Mesorregiões")
        } else
          if(input$ranking_escolha_meso == "escolhe_idese_meso"){
            tb <- tb %>%
              mutate(Ranking = rank(tb$"Idese"))
            
            colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Idese")
          } else
            if(input$ranking_escolha_meso == "escolhe_saude_meso"){
              tb <- tb %>%
                mutate(Ranking = rank(tb$"Bloco Saúde"))
              
              colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Saúde")
            } else
              if(input$ranking_escolha_meso == "escolhe_renda_meso"){
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Renda"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Renda")
              } else{
                tb <- tb %>%
                  mutate(Ranking = rank(tb$"Bloco Educação"))
                
                colnames(tb) <- c("NOME", "Idese", "Bloco Educação", "Bloco Renda","Bloco Saúde", "Ranking: Educação")
              }
        
      }
      
      tb$Idese <- round(tb$Idese, 3)
      tb$"Bloco Educação" <- round(tb$"Bloco Educação", 3)
      tb$"Bloco Renda" <- round(tb$"Bloco Renda", 3)
      tb$"Bloco Saúde" <- round(tb$"Bloco Saúde", 3)
      
      fixedWidth = 100
      
      formattable(tb, list(
        Idese = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                   direction = "rtl", `border-radius` = "4px", 
                                                                   `padding-right` = "2px", `background-color` = csscolor("#ddbaf6"),
                                                                     width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Saúde" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                   direction = "rtl", `border-radius` = "4px", 
                                                                   `padding-right` = "2px", `background-color` = csscolor("#f9c2c2"),
                                                                   width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Educação" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                           direction = "rtl", `border-radius` = "4px", 
                                                                           `padding-right` = "2px", `background-color` = csscolor("#a6ddff"),
                                                                           width = paste0(fixedWidth*proportion(x), "px", sep = ""))),
        "Bloco Renda" = formatter(.tag = "span", style = function(x) style(display = "inline-block",
                                                                              direction = "rtl", `border-radius` = "4px", 
                                                                              `padding-right` = "2px", `background-color` = csscolor("#fbceb1"),
                                                                              width = paste0(fixedWidth*proportion(x), "px", sep = "")))
      ))
      
    })

 output$tabela_download = renderDataTable({
    datatable(main_data, options(list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
      pageLength = 15)))
  })
  
  
  datasetInput <- reactive({
    main_data
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('base_idese', '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )

   #MapinhaLeaflet Municipios
  selected_mun <- reactive({
      
      mun <- mapa_RS[mapa_RS$NOME == input$idese_mun_compara, ]
      
      return(mun)
      
    })
    
  output$mapinha_municipios <- renderLeaflet({
      
      mapinha_RS() %>%
        addPolygons(data = selected_mun(), fill = FALSE, color = '#FFFF00', 
                    opacity = 1, layerId = 'sel_cty') %>%
        fitBounds(lng1 = bbox(selected_mun())[1], 
                  lat1 = bbox(selected_mun())[2], 
                  lng2 = bbox(selected_mun())[3], 
                  lat2 = bbox(selected_mun())[4])
      
    })
    
  muni_click <- eventReactive(input$mapinha_municipios_shape_click, {
      
      x <- input$mapinha_municipios_shape_click
      
      y <- x$id
      
      return(y)
      
    })
    
  observe({
      
      updateSelectInput(session, 'idese_mun_compara', selected = muni_click())
      
    })

    #MapinhaLeaflet Coredes
    selected_cor <- reactive({
      
      cor <- mapa_Cor[mapa_Cor@data$Corede == input$idese_cor_compara, ]
      
      return(cor)
      
    })
    
    output$mapinha_coredes <- renderLeaflet({
      
      mapinha_COREDE() %>%
        addPolygons(data = selected_cor(), fill = FALSE, color = '#FFFF00', 
                    opacity = 1, layerId = 'sel_cty') %>%
        fitBounds(lng1 = bbox(selected_cor())[1], 
                  lat1 = bbox(selected_cor())[2], 
                  lng2 = bbox(selected_cor())[3], 
                  lat2 = bbox(selected_cor())[4])
      
    })
    
    cor_click <- eventReactive(input$mapinha_coredes_shape_click, {
      
      x <- input$mapinha_coredes_shape_click
      
      y <- x$id
      
      return(y)
      
    })
    
    observe({
      
      updateSelectInput(session, 'idese_cor_compara', selected = cor_click())
      
    })

    #MapinhaLeaflet RF
    selected_rf <- reactive({
      
      rf <- mapa_RF[mapa_RF@data$RF == input$idese_rf_compara, ]
      
      return(rf)
      
    })
    
    output$mapinha_rf <- renderLeaflet({
      
      mapinha_RF() %>%
        addPolygons(data = selected_rf(), fill = FALSE, color = '#FFFF00', 
                    opacity = 1, layerId = 'sel_cty') %>%
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
      
      updateSelectInput(session, 'idese_rf_compara', selected = rf_click())
      
    })

    #MapinhaLeaflet Micro
    selected_micro <- reactive({
      
      micro <- mapa_Micro[mapa_Micro@data$Micro == input$idese_micro_compara, ]
      
      return(micro)
      
    })
    
    output$mapinha_micro <- renderLeaflet({
      
      mapinha_MICRO() %>%
        addPolygons(data = selected_micro(), fill = FALSE, color = '#FFFF00', 
                    opacity = 1, layerId = 'sel_cty') %>%
        fitBounds(lng1 = bbox(selected_micro())[1], 
                  lat1 = bbox(selected_micro())[2], 
                  lng2 = bbox(selected_micro())[3], 
                  lat2 = bbox(selected_micro())[4])
      
    })
    
    micro_click <- eventReactive(input$mapinha_micro_shape_click, {
      
      x <- input$mapinha_micro_shape_click
      
      y <- x$id
      
      return(y)
      
    })
    
    observe({
      
      updateSelectInput(session, 'idese_micro_compara', selected = micro_click())
      
    })

    #MapinhaLeaflet Meso
    selected_meso <- reactive({
      
      meso <- mapa_Meso[mapa_Meso@data$Meso == input$idese_meso_compara, ]
      
      return(meso)
      
    })
    
    output$mapinha_meso <- renderLeaflet({
      
      mapinha_MESO() %>%
        addPolygons(data = selected_meso(), fill = FALSE, color = '#FFFF00', 
                    opacity = 1, layerId = 'sel_cty') %>%
        fitBounds(lng1 = bbox(selected_meso())[1], 
                  lat1 = bbox(selected_meso())[2], 
                  lng2 = bbox(selected_meso())[3], 
                  lat2 = bbox(selected_meso())[4])
      
    })
    
    meso_click <- eventReactive(input$mapinha_meso_shape_click, {
      
      x <- input$mapinha_meso_shape_click
      
      y <- x$id
      
      return(y)
      
    })
    
    observe({
      
      updateSelectInput(session, 'idese_meso_compara', selected = meso_click())
      
    })

})
