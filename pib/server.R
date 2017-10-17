library(shiny)
library(stringi)
library(collapsibleTree)

pib_trim_struct <- data.frame(H1 = rep("PIB", 13),
                              H2 = rep(c("Valor Adicionado Bruto", "Impostos"), c(12,1)),
                              H3 = c(rep(c("Agropecuária", "Indústria", "Serviços"),c(1,4,7)),NA),
                              H4 = c(NA, 
                                     "Construção",
                                     "Eletricidade e gás, água, esgoto e limpeza urbana",
                                     "Extrativa Mineral",
                                     "Transformação",
                                     "APU, educação pública e saúde pública",
                                     "Atividades Imobiliárias",
                                     "Comércio",
                                     "Intermediação financeira e seguros",
                                     "Outros Serviços",
                                     "Serviços de informação",
                                     "Transporte, armazenagem e correio",
                                     NA))


base_pib_mun <- readRDS("base_pib_mun.rds") %>%
  mutate(VAB_OutSer = VAB_Serv - VAB_Apub,
         Pop_PIB = PIB / PIB_pp)

base_pib_mun$Municipio <- stri_conv(as.character(base_pib_mun$Municipio), "latin1", "UTF-8")		 

corresp <- select(readRDS("Corresp_Mun_PopRS.rds"), -Estado, -CodUG, -CodMeso, -Meso, -CodMicro, -Micro, -CodMunicipio)
mapa_mun <- readRDS("MapaRSMunicipios.rds")
mapa_cor <- readRDS("MapaRSCoredes.rds")
mapa_rf <- readRDS("mapaRF.rds")

mapa_RS_old <- readRDS("mapaRS_old.rds")
mapa_RS_old@data$Nome_Munic <- stri_conv(as.character(mapa_RS_old@data$Nome_Munic), "latin1", "UTF-8")

#base_trimestral <- readRDS("base_final_trimestral.rds")
base_trimestral <- readRDS("base_final_trimestral_TRIM_II_2017_V8.rds") #%>% # Renomear e controlar para sair o trimestre de referência
                   #filter(AnoTrim != "2017.II")
                   
#base_trimestral$Valor <- round(base_trimestral$Valor, 3) # formatC(round(base_trimestral$Valor, 1), digits = 1, big.mark = ".", format = "f", decimal.mark = ",") # round(base_trimestral$Valor, 3)
base_trimestral$TipoDado <- stri_conv(as.character(base_trimestral$TipoDado), "latin1", "UTF-8")
base_trimestral$SeRefere <- stri_conv(as.character(base_trimestral$SeRefere), "latin1", "UTF-8")
base_trimestral$DescVar <- stri_conv(as.character(base_trimestral$DescVar), "latin1", "UTF-8")
base_trimestral$DescVarShort <- stri_conv(as.character(base_trimestral$DescVarShort), "latin1", "UTF-8")
base_trimestral$DescVarTabela <- stri_conv(as.character(base_trimestral$DescVarTabela), "latin1", "UTF-8")

tabela_brasileirao_pib_trim <- base_trimestral %>% select(-CodTipoDado) %>% spread(TipoDado, Valor)
tabela_tempo_transposto_pib_trim <- base_trimestral %>% select(-CodTipoDado, -Ano, -Trimestre) %>% spread(AnoTrim, Valor)


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

pib_mun_struct <- data.frame(H1 = rep("PIB", 5), 
                             H2 = c(rep("Valor Adicionado Bruto", 4), "Impostos"),
                             H3 = c("Agropecuária", "Indústria", "Serviços", "Serviços", NA),
                             H4 = c(NA, NA, "Administração Pública", "Outros Serviços", NA))


mapinha_RS <- function() {
  
  leaflet(mapa_mun) %>%
    addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
    addPolygons(weight = 1, smoothFactor = 0.2, color = '#00008B', 
                fillColor = '#00008B', label = ~Cidade,  layerId = ~Cidade)
  
}

mapinha_COREDE <- function() {
  
  leaflet(mapa_cor) %>%
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



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

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
  
  
  output$pib_mun_struct <- renderCollapsibleTree({
    
    collapsibleTree(
      pib_mun_struct,
      hierarchy = c("H2", "H3", "H4"),
      root = "PIB",
      fontSize = 15
    )
    
  })
  
  
  
  output$mapa_mun_valores <- renderLeaflet({
    
    input_ano <- input$ano_pib_mun_valores
    input_setor <- input$setor_mun_valores
    
    df_aux <- base_pib_mun %>% 
      gather(Setor, Valor, VAB_Agro, VAB_Ind, VAB_Serv, VAB_Apub, VAB_OutSer, VAB, Imp, PIB, PIB_pp) %>%
      mutate(DescSetor = retorna_setor(Setor)) %>%
      filter(Ano == input_ano, DescSetor == input_setor) %>%
      mutate(Valor_Percent = Valor / sum(Valor, na.rm = T) * 100)
    
    
	if(input_ano >= 2013) {df_mapa <- merge(mapa_mun, df_aux, by.x = "GEOCODIGO", by.y = "CodIBGE")}
	if(input_ano < 2013)  {df_mapa <- merge(mapa_RS_old, df_aux, by.x = "GEOCODIG_M", by.y = "CodIBGE", all.x = FALSE)} # all.x = FALSE para tirar as lagos do merge que são NA
	
    
    if(input$tipo_dado_mapa_valor_mun == "radio_absoluto_mun") {
      #gradiente = colorNumeric(c("gray", "lightgray", "yellow", "#00f010", "#149800"), domain = df_mapa$Valor)
	  
	  #classes <- c(0, 1000, 100000, 1000000, 10000000, 100000000, 1000000000, 100000000000)
	  #classes <- quantile(df_mapa$Valor, c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1))
	  classes <- c(0, 70000000, 100000000, 200000000, 600000000, 1500000000, 100000000000)
      pal_cor <- colorBin(c("#6A1103", "#BA0004", "#E7400B", "#FEA527", "#FDFE65", "#9CE400", "#359800"), domain = df_mapa$Valor, bins = classes)
      
      leaflet(data = df_mapa) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, 
                    fillColor = ~pal_cor(df_mapa$Valor), # gradiente(df_mapa$Valor)
                    color = "grey", 
                    fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(paste("<strong>", df_mapa$Municipio, "</strong>"), "<br>",
                                   "Valor (R$): ", format(df_mapa$Valor, big.mark = ".", decimal.mark = ","), "<br>",
                                   "Participação no RS (%): ", format(round(df_mapa$Valor_Percent, 2), decimal.mark = ","))) %>% 
        addLegend(position = "bottomright", pal = pal_cor, values = ~Valor) # pal = gradiente
    }
    
    else
      
      if(input$tipo_dado_mapa_valor_mun == "radio_percent_mun") {
        gradiente = colorNumeric(c("gray", "lightgray", "yellow", "#00f010", "#149800"), domain = df_mapa$Valor_Percent)
        
        leaflet(data = df_mapa) %>% 
          addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addPolygons(weight = 0.5, 
                      fillColor = ~gradiente(df_mapa$Valor_Percent), # Weight e a grossura das bordas
                      color = "grey", 
                      fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(paste("<strong>", df_mapa$Municipio, "</strong>"), "<br>",
                                     "Valor (R$): ", format(df_mapa$Valor, big.mark = ".", decimal.mark = ","), "<br>",
                                     "Participação no RS (%): ", format(round(df_mapa$Valor_Percent, 2), decimal.mark = ","))) %>% 
          addLegend(position = "bottomright", pal = gradiente, values = ~Valor_Percent)
      }
    
  })
  
  output$mapa_cor_valores <- renderLeaflet({
    
    input_ano <- input$ano_pib_cor_valores
    input_setor <- input$setor_cor_valores
    
    df_aux <- base_pib_cor %>% 
      gather(Setor, Valor, VAB_Agro, VAB_Ind, VAB_Serv, VAB_Apub, VAB_OutSer, VAB, Imp, PIB, PIB_pp) %>%
      mutate(DescSetor = retorna_setor(Setor)) %>%
      filter(Ano == input_ano, DescSetor == input_setor) %>%
      mutate(Valor_Percent = Valor / sum(Valor, na.rm = T) * 100)
    
    df_mapa <- merge(mapa_cor, df_aux, by.x = "CodCorede", by.y = "CodCorede")
    
    if(input$tipo_dado_mapa_valor_cor == "radio_absoluto_cor") {
      gradiente = colorNumeric(c("gray", "lightgray", "yellow", "#00f010", "#149800"), domain = df_mapa$Valor)
      
      leaflet(data = df_mapa) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, 
                    fillColor = ~gradiente(df_mapa$Valor), # Weight e a grossura das bordas
                    color = "grey", 
                    fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(paste("<strong>", df_mapa$Corede.x, "</strong>"), "<br>",
                                   "Valor (R$): ", format(df_mapa$Valor, big.mark = ".", decimal.mark = ","), "<br>",
                                   "Participação no RS (%): ", format(round(df_mapa$Valor_Percent, 2), decimal.mark = ","))) %>% 
        addLegend(position = "bottomright", pal = gradiente, values = ~Valor)
    }
    
    else
      
      if(input$tipo_dado_mapa_valor_cor == "radio_percent_cor") {
        gradiente = colorNumeric(c("gray", "lightgray", "yellow", "#00f010", "#149800"), domain = df_mapa$Valor_Percent)
        
        leaflet(data = df_mapa) %>% 
          addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addPolygons(weight = 0.5, 
                      fillColor = ~gradiente(df_mapa$Valor_Percent), # Weight e a grossura das bordas
                      color = "grey", 
                      fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(paste("<strong>", df_mapa$Corede.x, "</strong>"), "<br>",
                                     "Valor (R$): ", format(df_mapa$Valor, big.mark = ".", decimal.mark = ","), "<br>",
                                     "Participação no RS (%): ", format(round(df_mapa$Valor_Percent, 2), decimal.mark = ","))) %>% 
          addLegend(position = "bottomright", pal = gradiente, values = ~Valor_Percent)
      }
    
  })
  
  output$mapa_rf_valores <- renderLeaflet({
    
    input_ano <- input$ano_pib_rf_valores
    input_setor <- input$setor_rf_valores
    
    df_aux <- base_pib_rf %>% 
      gather(Setor, Valor, VAB_Agro, VAB_Ind, VAB_Serv, VAB_Apub, VAB_OutSer, VAB, Imp, PIB, PIB_pp) %>%
      mutate(DescSetor = retorna_setor(Setor)) %>%
      filter(Ano == input_ano, DescSetor == input_setor) %>%
      mutate(Valor_Percent = Valor / sum(Valor, na.rm = T) * 100)
    
    df_mapa <- merge(mapa_rf, df_aux, by.x = "OBJECTID", by.y = "CodRF")
    
    if(input$tipo_dado_mapa_valor_rf == "radio_absoluto_rf") {
      gradiente = colorNumeric(c("gray", "lightgray", "yellow", "#00f010", "#149800"), domain = df_mapa$Valor)
      
      leaflet(data = df_mapa) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addPolygons(weight = 0.5, 
                    fillColor = ~gradiente(df_mapa$Valor), # Weight e a grossura das bordas
                    color = "grey", 
                    fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                    smoothFactor = 0.25,
                    popup = paste0(paste("<strong>Região Funcional", df_mapa$OBJECTID, "</strong>"), "<br>",
                                   "Valor (R$): ", format(df_mapa$Valor, big.mark = ".", decimal.mark = ","), "<br>",
                                   "Participação no RS (%): ", format(round(df_mapa$Valor_Percent, 2), decimal.mark = ","))) %>% 
        addLegend(position = "bottomright", pal = gradiente, values = ~Valor)
    }
    
    else
      
      if(input$tipo_dado_mapa_valor_rf == "radio_percent_rf") {
        gradiente = colorNumeric(c("gray", "lightgray", "yellow", "#00f010", "#149800"), domain = df_mapa$Valor_Percent)
        
        leaflet(data = df_mapa) %>% 
          addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addPolygons(weight = 0.5, 
                      fillColor = ~gradiente(df_mapa$Valor_Percent), # Weight e a grossura das bordas
                      color = "grey", 
                      fillOpacity = 0.75, # color e a cor das bordas e o fillopacity e tranparencia
                      smoothFactor = 0.25,
                      popup = paste0(paste("<strong>Região Funcional", df_mapa$OBJECTID, "</strong>"), "<br>",
                                     "Valor (R$): ", format(df_mapa$Valor, big.mark = ".", decimal.mark = ","), "<br>",
                                     "Participação no RS (%): ", format(round(df_mapa$Valor_Percent, 2), decimal.mark = ","))) %>% 
          addLegend(position = "bottomright", pal = gradiente, values = ~Valor_Percent)
      }
    
  })
  
  
  
  output$mapa_mun_minichart <- renderLeaflet({
    
    input_ano <- input$ano_pib_mun
    
    df_aux_mun <- filter(base_pib_mun, Ano == input_ano)
	
    if(input_ano >= 2013) {df_mapa_mun <- merge(mapa_mun, df_aux_mun, by.x = "GEOCODIGO", by.y = "CodIBGE")}
	if(input_ano < 2013)  {df_mapa_mun <- merge(mapa_RS_old, df_aux_mun, by.x = "GEOCODIG_M", by.y = "CodIBGE", all.x = FALSE)} # all.x = FALSE para tirar as lagos do merge que são NA
	
    #df_mapa_mun <- merge(mapa_mun, df_aux_mun, by.x = "GEOCODIGO", by.y = "CodIBGE")
	
    coord_mun <- coordinates(df_mapa_mun)
    
    if(input$tipo_representacao_mun == "vab_imp_mun") {
      leaflet(df_mapa_mun) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addMinicharts(
          lng = coord_mun[,1], lat = coord_mun[,2],
          type = "pie",
          width = 16,
          legendPosition = "bottomright",
          chartdata = df_mapa_mun@data[, c("VAB", "Imp")],
          popup = list(labels = c("Valor Adicionado Bruto", "Impostos"), showTitle = T)
        ) %>%
        addPolygons(color = "transparent", # gray
                    fillOpacity = 0,
                    weight = 1,
                    popup = paste0(paste0("<strong>", df_mapa_mun$Municipio, "</strong>"),"<br>",
                                   "VAB (%): ", format(round(df_mapa_mun$VAB / df_mapa_mun$PIB * 100, 2), decimal.mark = ","), " (R$",format(df_mapa_mun$VAB, big.mark = ".", decimal.mark = ","),")<br>",
                                   "Impostos (%): ", format(round(df_mapa_mun$Imp / df_mapa_mun$PIB * 100, 2), decimal.mark = ","), " (R$",format(df_mapa_mun$Imp, big.mark = ".", decimal.mark = ","),")"
                    )
        )
    } 
    
    else 
      
      if(input$tipo_representacao_mun == "agro_ind_serv_mun") {
        leaflet(df_mapa_mun) %>% 
          addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addMinicharts(
            lng = coord_mun[,1], lat = coord_mun[,2],
            type = "pie",
            width = 16,
            legendPosition = "bottomright",
            chartdata = df_mapa_mun@data[, c("VAB_Agro", "VAB_Ind", "VAB_Serv")],
			colorPalette = c("green", "blue", "orange"),
            popup = list(labels = c("Agropecuária", "Indústria", "Serviços"), showTitle = T)
          ) %>%
          addPolygons(color = "transparent", # gray
                      fillOpacity = 0,
                      weight = 1,
                      popup = paste0(paste("<strong>", df_mapa_mun$Cidade, "</strong>"),"<br>",
                                     "Agropecuária (%): ", format(round(df_mapa_mun$VAB_Agro / df_mapa_mun$VAB * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_mun$VAB_Agro, big.mark = ".", decimal.mark = ","),")<br>",
                                     "Indústria (%): ", format(round(df_mapa_mun$VAB_Ind / df_mapa_mun$VAB * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_mun$VAB_Ind, big.mark = ".", decimal.mark = ","),")<br>",
                                     "Serviços (%): ", format(round(df_mapa_mun$VAB_Serv / df_mapa_mun$VAB * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_mun$VAB_Serv, big.mark = ".", decimal.mark = ","),")"
                      )
          )
      }
    
    else
      
      if(input$tipo_representacao_mun == "apub_out_mun") {
        leaflet(df_mapa_mun) %>% 
          addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addMinicharts(
            lng = coord_mun[,1], lat = coord_mun[,2],
            type = "pie",
            width = 16,
            legendPosition = "bottomright",
            chartdata = df_mapa_mun@data[, c("VAB_Apub", "VAB_OutSer")],
			colorPalette = c("purple", "lightgray"),
            popup = list(labels = c("Administração Pública", "Outros Serviços"), showTitle = T)
          ) %>%
          addPolygons(color = "transparent", # gray
                      fillOpacity = 0,
                      weight = 1,
                      popup = paste0(paste("<strong>", df_mapa_mun$Cidade, "</strong>"),"<br>",
                                     "Administração Pública (%): ", format(round(df_mapa_mun$VAB_Apub / df_mapa_mun$VAB_Serv * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_mun$VAB_Apub, big.mark = ".", decimal.mark = ","),")<br>",
                                     "Outros Serviços (%): ", format(round(df_mapa_mun$VAB_OutSer / df_mapa_mun$VAB_Serv * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_mun$VAB_OutSer, big.mark = ".", decimal.mark = ","),")"
                      )
          )
      }
    
    
  })
  
  output$mapa_cor_minichart <- renderLeaflet({
    
    input_ano <- input$ano_pib_cor
    
    df_aux_cor <- filter(base_pib_cor, Ano == input_ano)
    df_mapa_cor <- merge(mapa_cor, df_aux_cor, by.x = "CodCorede", by.y = "CodCorede")
    coord_cor <- coordinates(df_mapa_cor)
    
    if(input$tipo_representacao_cor == "vab_imp_cor") {
      leaflet(df_mapa_cor) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addMinicharts(
          lng = coord_cor[,1], lat = coord_cor[,2],
          type = "pie",
          legendPosition = "bottomright",
          chartdata = df_mapa_cor@data[, c("VAB", "Imp")],
          popup = list(labels = c("Valor Adicionado Bruto", "Impostos"), showTitle = T)
        ) %>%
        addPolygons(color = "gray", # transparent
                    fillOpacity = 0,
                    weight = 1,
                    popup = paste0(paste("<strong>", df_mapa_cor$Corede.x, "</strong>"),"<br>",
                                   "VAB (%): ", format(round(df_mapa_cor$VAB / df_mapa_cor$PIB * 100, 2), decimal.mark = ","), " (R$",format(df_mapa_cor$VAB, big.mark = ".", decimal.mark = ","),")<br>",
                                   "Impostos (%): ", format(round(df_mapa_cor$Imp / df_mapa_cor$PIB * 100, 2), decimal.mark = ","), " (R$",format(df_mapa_cor$Imp, big.mark = ".", decimal.mark = ","),")"
                    )
        )
    } 
    
    else 
      
      if(input$tipo_representacao_cor == "agro_ind_serv_cor") {
        leaflet(df_mapa_cor) %>% 
          addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addMinicharts(
            lng = coord_cor[,1], lat = coord_cor[,2],
            type = "pie",
            legendPosition = "bottomright",
            chartdata = df_mapa_cor@data[, c("VAB_Agro", "VAB_Ind", "VAB_Serv")],
			colorPalette = c("green", "blue", "orange"),
            popup = list(labels = c("Agropecuária", "Indústria", "Serviços"), showTitle = T)
          ) %>%
          addPolygons(color = "gray", # transparent
                      fillOpacity = 0,
                      weight = 1,
                      popup = paste0(paste("<strong>", df_mapa_cor$Corede.x, "</strong>"),"<br>",
                                     "Agropecuária (%): ", format(round(df_mapa_cor$VAB_Agro / df_mapa_cor$VAB * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_cor$VAB_Agro, big.mark = ".", decimal.mark = ","),")<br>",
                                     "Indústria (%): ", format(round(df_mapa_cor$VAB_Ind / df_mapa_cor$VAB * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_cor$VAB_Ind, big.mark = ".", decimal.mark = ","),")<br>",
                                     "Serviços (%): ", format(round(df_mapa_cor$VAB_Serv / df_mapa_cor$VAB * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_cor$VAB_Serv, big.mark = ".", decimal.mark = ","),")"
                      )
          )
      }
    
    else
      
      if(input$tipo_representacao_cor == "apub_out_cor") {
        leaflet(df_mapa_cor) %>% 
          addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addMinicharts(
            lng = coord_cor[,1], lat = coord_cor[,2],
            type = "pie",
            legendPosition = "bottomright",
            chartdata = df_mapa_cor@data[, c("VAB_Apub", "VAB_OutSer")],
			colorPalette = c("purple", "lightgray"),
            popup = list(labels = c("Administração Pública", "Outros Serviços"), showTitle = T)
          ) %>%
          addPolygons(color = "gray", # transparent
                      fillOpacity = 0,
                      weight = 1,
                      popup = paste0(paste("<strong>", df_mapa_cor$Corede.x, "</strong>"),"<br>",
                                     "Administração Pública (%): ", format(round(df_mapa_cor$VAB_Apub / df_mapa_cor$VAB_Serv * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_cor$VAB_Apub, big.mark = ".", decimal.mark = ","),")<br>",
                                     "Outros Serviços (%): ", format(round(df_mapa_cor$VAB_OutSer / df_mapa_cor$VAB_Serv * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_cor$VAB_OutSer, big.mark = ".", decimal.mark = ","),")"
                      )
          )
      }
    
    
  })
  
  output$mapa_rf_minichart <- renderLeaflet({
    
    input_ano <- input$ano_pib_rf
    
    df_aux_rf <- filter(base_pib_rf, Ano == input_ano)
    df_mapa_rf <- merge(mapa_rf, df_aux_rf, by.x = "OBJECTID", by.y = "CodRF")
    coord_rf <- coordinates(df_mapa_rf)
    
    if(input$tipo_representacao_rf == "vab_imp_rf") {
      leaflet(df_mapa_rf) %>% 
        addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
        addMinicharts(
          lng = coord_rf[,1], lat = coord_rf[,2],
          type = "pie",
          legendPosition = "bottomright",
          chartdata = df_mapa_rf@data[, c("VAB", "Imp")],
          popup = list(labels = c("Valor Adicionado Bruto", "Impostos"), showTitle = T)
        ) %>%
        addPolygons(color = "gray", # transparent
                    fillOpacity = 0,
                    weight = 1,
                    popup = paste0(paste("<strong> RF", df_mapa_rf$OBJECTID, "</strong>"),"<br>",
                                   "VAB (%): ", format(round(df_mapa_rf$VAB / df_mapa_rf$PIB * 100, 2), decimal.mark = ","), " (R$",format(df_mapa_rf$VAB, big.mark = ".", decimal.mark = ","),")<br>",
                                   "Impostos (%): ", format(round(df_mapa_rf$Imp / df_mapa_rf$PIB * 100, 2), decimal.mark = ","), " (R$",format(df_mapa_rf$Imp, big.mark = ".", decimal.mark = ","),")"
                    )
        )
    } 
    
    else 
      
      if(input$tipo_representacao_rf == "agro_ind_serv_rf") {
        leaflet(df_mapa_rf) %>% 
          addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addMinicharts(
            lng = coord_rf[,1], lat = coord_rf[,2],
            type = "pie",
            legendPosition = "bottomright",
            chartdata = df_mapa_rf@data[, c("VAB_Agro", "VAB_Ind", "VAB_Serv")],
			colorPalette = c("green", "blue", "orange"),
            popup = list(labels = c("Agropecuária", "Indústria", "Serviços"), showTitle = T)
          ) %>%
          addPolygons(color = "gray", # transparent
                      fillOpacity = 0,
                      weight = 1,
                      popup = paste0(paste("<strong> RF", df_mapa_rf$OBJECTID, "</strong>"),"<br>",
                                     "Agropecuária (%): ", format(round(df_mapa_rf$VAB_Agro / df_mapa_rf$VAB * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_rf$VAB_Agro, big.mark = ".", decimal.mark = ","),")<br>",
                                     "Indústria (%): ", format(round(df_mapa_rf$VAB_Ind / df_mapa_rf$VAB * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_rf$VAB_Ind, big.mark = ".", decimal.mark = ","),")<br>",
                                     "Serviços (%): ", format(round(df_mapa_rf$VAB_Serv / df_mapa_rf$VAB * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_rf$VAB_Serv, big.mark = ".", decimal.mark = ","),")"
                      )
          )
      }
    
    else
      
      if(input$tipo_representacao_rf == "apub_out_rf") {
        leaflet(df_mapa_rf) %>% 
          addTiles('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png') %>%
          addMinicharts(
            lng = coord_rf[,1], lat = coord_rf[,2],
            type = "pie",
            legendPosition = "bottomright",
            chartdata = df_mapa_rf@data[, c("VAB_Apub", "VAB_OutSer")],
			colorPalette = c("purple", "lightgray"),
            popup = list(labels = c("Administração Pública", "Outros Serviços"), showTitle = T)
          ) %>%
          addPolygons(color = "gray", # transparent
                      fillOpacity = 0,
                      weight = 1,
                      popup = paste0(paste("<strong> RF", df_mapa_rf$OBJECTID, "</strong>"),"<br>",
                                     "Administração Pública (%): ", format(round(df_mapa_rf$VAB_Apub / df_mapa_rf$VAB_Serv * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_rf$VAB_Apub, big.mark = ".", decimal.mark = ","),")<br>",
                                     "Outros Serviços (%): ", format(round(df_mapa_rf$VAB_OutSer / df_mapa_rf$VAB_Serv * 100, 2), decimal.mark = ","), " (R$", format(df_mapa_rf$VAB_OutSer, big.mark = ".", decimal.mark = ","),")"
                      )
          )
      }
    
    
  })
  
  
  
  
  
  output$tree_mun_regioes_pre <- renderD3plus({
    
    input_ano <- input$ano_pib_mun_regioes_treemap
    input_setor <- input$setor_mun_regioes_treemap
    
    df_aux <- base_pib_mun %>% 
      gather(Setor, Valor, VAB_Agro, VAB_Ind, VAB_Serv, VAB_Apub, VAB_OutSer, VAB, Imp, PIB, PIB_pp) %>%
      mutate(DescSetor = retorna_setor(Setor)) %>%
      filter(Ano == input_ano, DescSetor == input_setor)
    
    d3plus(data = df_aux,
           type = "tree_map",
           id = c("Municipio"),
		   clean_previous = TRUE) %>%
      d3plusSize(value = "Valor") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top")
    
  })
  
  output$tree_mun_regioes <- renderUI({
    d3plusOutput("tree_mun_regioes_pre")
  })
  
  
  output$tree_cor_regioes_pre <- renderD3plus({
    
    input_ano <- input$ano_pib_cor_regioes_treemap
    input_setor <- input$setor_cor_regioes_treemap
    
    df_aux <- base_pib_cor %>% 
      gather(Setor, Valor, VAB_Agro, VAB_Ind, VAB_Serv, VAB_Apub, VAB_OutSer, VAB, Imp, PIB, PIB_pp) %>%
      mutate(DescSetor = retorna_setor(Setor)) %>%
      filter(Ano == input_ano, DescSetor == input_setor)
    
    d3plus(data = df_aux,
           type = "tree_map",
           id = c("Corede"),
		   clean_previous = TRUE) %>%
      d3plusSize(value = "Valor") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top")
    
  })
  
  output$tree_cor_regioes <- renderUI({
    d3plusOutput("tree_cor_regioes_pre")
  })
  
  
  output$tree_rf_regioes_pre <- renderD3plus({
    
    input_ano <- input$ano_pib_rf_regioes_treemap
    input_setor <- input$setor_rf_regioes_treemap
    
    df_aux <- base_pib_rf %>% 
      gather(Setor, Valor, VAB_Agro, VAB_Ind, VAB_Serv, VAB_Apub, VAB_OutSer, VAB, Imp, PIB, PIB_pp) %>%
      mutate(DescSetor = retorna_setor(Setor)) %>%
      filter(Ano == input_ano, DescSetor == input_setor) %>%
      mutate(Regiao = paste("Região Funcional", CodRF))
    
    d3plus(data = df_aux,
           type = "tree_map",
           id = c("Regiao"),
		   clean_previous = TRUE) %>%
      d3plusSize(value = "Valor") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top")
    
  })
  
  output$tree_rf_regioes <- renderUI({
    d3plusOutput("tree_rf_regioes_pre")
  })
  
  
  output$tree_hier_regioes_pre <- renderD3plus({
    
    input_ano <- input$ano_pib_hier_regioes_treemap
    input_setor <- input$setor_hier_regioes_treemap
    
    df_aux <- base_pib_aux %>% 
      gather(Setor, Valor, VAB_Agro, VAB_Ind, VAB_Serv, VAB_Apub, VAB_OutSer, VAB, Imp, PIB, PIB_pp) %>%
      mutate(DescSetor = retorna_setor(Setor)) %>%
      filter(Ano == input_ano, DescSetor == input_setor) %>%
      mutate(Regiao = paste("Região Funcional", CodRF))
	  
	  df_aux$Corede <- stri_conv(as.character(df_aux$Corede), "latin1", "UTF-8")
    
    d3plus(data = df_aux,
           type = "tree_map",
           id = c("Regiao", "Corede", "Municipio"),
		   clean_previous = TRUE) %>%
      d3plusSize(value = "Valor") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top")
    
  })
  
  output$tree_hier_regioes <- renderUI({
    d3plusOutput("tree_hier_regioes_pre")
  })
  
  
  output$tree_rs_setorial_pre <- renderD3plus({
    
    input_ano <- input$ano_pib_rs_setorial_treemap
    input_rs <- input$rs_setorial_treemap
    
    df_aux <- base_pib_mun %>% 
      filter(Ano == input_ano) %>%
      summarize(VAB_Agro = sum(VAB_Agro, na.rm = T),
                VAB_Ind = sum(VAB_Ind, na.rm = T),
                VAB_Apub = sum(VAB_Apub, na.rm = T),
                VAB_OutSer = sum(VAB_OutSer, na.rm = T),
                Imp = sum(Imp, na.rm = T))
    
    pib_struct_aux <- cbind(pib_mun_struct, Valor = c(df_aux$VAB_Agro,
                                                      df_aux$VAB_Ind,
                                                      df_aux$VAB_Apub,
                                                      df_aux$VAB_OutSer,
                                                      df_aux$Imp))
    
    d3plus(data = pib_struct_aux,
           type = "tree_map",
           id = c("H1","H2","H3","H4"),
		   clean_previous = TRUE) %>%
      d3plusSize(value = "Valor") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top")
    
  })
  
  output$tree_rs_setorial <- renderUI({
    d3plusOutput("tree_rs_setorial_pre")
  })
  
  
  output$tree_mun_setorial_pre <- renderD3plus({
    
    input_ano <- input$ano_pib_mun_setorial_treemap
    input_mun <- input$mun_setorial_treemap
    
    df_aux <- base_pib_mun %>% 
      filter(Ano == input_ano, Municipio == input_mun)
    
    pib_struct_aux <- cbind(pib_mun_struct, Valor = c(df_aux$VAB_Agro,
                                                      df_aux$VAB_Ind,
                                                      df_aux$VAB_Apub,
                                                      df_aux$VAB_OutSer,
                                                      df_aux$Imp))
    
    d3plus(data = pib_struct_aux,
           type = "tree_map",
           id = c("H1","H2","H3","H4"),
		   clean_previous = TRUE) %>%
      d3plusSize(value = "Valor") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top")
    
  })
  
  output$tree_mun_setorial <- renderUI({
    d3plusOutput("tree_mun_setorial_pre")
  })
  
  
  output$tree_cor_setorial_pre <- renderD3plus({
    
    input_ano <- input$ano_pib_cor_setorial_treemap
    input_cor <- input$cor_setorial_treemap
    
    df_aux <- base_pib_cor %>% 
      filter(Ano == input_ano, Corede == input_cor)
    
    pib_struct_aux <- cbind(pib_mun_struct, Valor = c(df_aux$VAB_Agro,
                                                      df_aux$VAB_Ind,
                                                      df_aux$VAB_Apub,
                                                      df_aux$VAB_OutSer,
                                                      df_aux$Imp))
    
    d3plus(data = pib_struct_aux,
           type = "tree_map",
           id = c("H1","H2","H3","H4"),
		   clean_previous = TRUE) %>%
      d3plusSize(value = "Valor") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top")
    
  })
  
  output$tree_cor_setorial <- renderUI({
    d3plusOutput("tree_cor_setorial_pre")
  })
  
  
  output$tree_rf_setorial_pre <- renderD3plus({
    
    input_ano <- input$ano_pib_rf_setorial_treemap
    input_rf <- input$rf_setorial_treemap
    
    df_aux <- base_pib_rf %>% 
      filter(Ano == input_ano, CodRF == input_rf)
    
    pib_struct_aux <- cbind(pib_mun_struct, Valor = c(df_aux$VAB_Agro,
                                                      df_aux$VAB_Ind,
                                                      df_aux$VAB_Apub,
                                                      df_aux$VAB_OutSer,
                                                      df_aux$Imp))
    
    d3plus(data = pib_struct_aux,
           type = "tree_map",
           id = c("H1","H2","H3","H4"),
		   clean_previous = TRUE) %>%
      d3plusSize(value = "Valor") %>%
      d3plusLegend(value = TRUE, order = list(sort = "desc", value = "size")) %>%
      d3plusDepth(0) %>%
      d3plusLabels(valign = "top")
    
  })
  
  output$tree_rf_setorial <- renderUI({
    d3plusOutput("tree_rf_setorial_pre")
  })
  
  # Mapinhas Municipios
  selected_city <- reactive({
    
    b <- mapa_mun[mapa_mun$Cidade == input$mun_setorial_treemap & !is.na(mapa_mun$Cidade), ]
    
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
    
    updateSelectInput(session, 'mun_setorial_treemap', selected = muni_click())
    
  })
  
  # Mapinhas Coredes
  selected_corede <- reactive({
    
    b <- mapa_cor[mapa_cor$Corede == input$cor_setorial_treemap & !is.na(mapa_cor$Corede), ]
    
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
  core_click <- eventReactive(input$mapinha_corede_shape_click, {
    
    x <- input$mapinha_corede_shape_click # O "_shape_click" é criado internamente pelo leaflet.
    
    y <- x$id
    
    return(y)
    
  })
  observe({
    
    updateSelectInput(session, 'cor_setorial_treemap', selected = core_click())
    
  })
  
  # Mapinhas RF
  selected_rf <- reactive({
    
    b <- mapa_rf[mapa_rf$OBJECTID == input$rf_setorial_treemap & !is.na(mapa_rf$OBJECTID), ]
    
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
    
    x <- input$mapinha_rf_shape_click # O "_shape_click" é criado internamente pelo leaflet.
    
    y <- x$id
    
    return(y)
    
  })
  observe({
    
    updateSelectInput(session, 'rf_setorial_treemap', selected = rf_click())
    
  })
  
  
    output$pib_trim_tree_struc <- renderCollapsibleTree({
    
    collapsibleTree(
      pib_trim_struct,
      hierarchy = c("H2", "H3", "H4"),
      root = "PIB",
      fontSize = 15
      # fill = c("lightgrey",
      #          "Yellow", "Blue", "Green",
      #          "Yellow","Yellow","Yellow","Yellow","Blue", "Blue","Green","Green", "Green",
      #          "Yellow", "Yellow","Green","Green","Green","Green")
    )
    
  })
  
  
  
  
  output$comparacao_trimestral <- renderPlotly({
  
    #base_trimestral$Valor <- round(base_trimestral$Valor, 1) # Para tooltip ficar só com x casas
  
    
    if(input$ajuste_var_compara == "radio_ajuste_compara_sem") {input_ajuste <- "Sem"; input_tipo_dado <- input$tipo_dado_var_compara1}
    if(input$ajuste_var_compara == "radio_ajuste_compara_com") {input_ajuste <- "Com"; input_tipo_dado <- input$tipo_dado_var_compara2}
    
    input_ano_trim <- input$ano_trim_var_compara
    #input_ajuste <- input$ajuste_var_compara
    input_setor <- input$setores_var_compara
    #input_tipo_dado <- input$tipo_dado_var_compara
    
    base_aux <- base_trimestral %>%
				  filter(AnoTrim == input_ano_trim, 
						 Ajuste == input_ajuste, 
						 SeRefere == input_setor,
						 TipoDado == input_tipo_dado) %>%
				  arrange(Ordem)

    base_aux$DescVarShort <- factor(base_aux$DescVarShort, levels = unique(base_aux$DescVarShort)) # Para ordenar no eixo "x" (o plotly só ordena no eixo x quem é factor. Caso contrário, ele pega o character e usa a ordem alfabética)	  
    
    if(input$checkbox_retira_rotulo_compara) {
	
	base_aux$Valor <- round(base_aux$Valor, 7)
	rotulo <- ""
	
	
	  base_aux %>%
      plot_ly(x = ~DescVarShort, 
              y = ~Valor, 
              type = 'bar', 
              color = ~Local, 
              hoverinfo = "text", # "closest" ou "text"
			  hovermode = FALSE,
              text = rotulo,
              textposition = 'outside') %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "", zeroline = FALSE),
             title = paste0(input_tipo_dado, " em ", input_ano_trim, " (", input_ajuste, " Ajuste Sazonal)"),
			 titlefont = list(size = 15), # Para caber na imagem de download o título
             legend = list(orientation = 'h')) %>%
			 config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
	
	
	} else {
	  
	  base_aux$Valor <- round(base_aux$Valor, 7)
	  rotulo <- formatC(ifelse(round(base_aux$Valor, 1) == 0, abs(round(base_aux$Valor, 1)), round(base_aux$Valor, 1)), digits = 1, big.mark = ".", format = "f", decimal.mark = ",") # Se o valor arredondado de uma casa decimal for 0, coloca o zero em absoluto no rótulo 
	  #rotulo <- formatC(round(base_aux$Valor, 1), digits = 1, big.mark = ".", format = "f", decimal.mark = ",")
	
	  base_aux %>%
      plot_ly(x = ~DescVarShort, 
              y = ~Valor, 
              type = 'bar', 
              color = ~Local, 
			  #hovermode = FALSE,
			  #hoverlabel = list(bgcolor = "white", font = list(size = 1)),
              text = rotulo,
			  hoverinfo = "none", # Tira a informação do Hover quando os rótulos aparecem! Funcionou!
              textposition = 'outside') %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "", zeroline = FALSE),
             title = paste0(input_tipo_dado, " em ", input_ano_trim, " (", input_ajuste, " Ajuste Sazonal)"),
			 titlefont = list(size = 15), # Para caber na imagem de download o título
             legend = list(orientation = 'h')) %>%
			 config(
			 displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))#%>%
	  #layout(annotations = list(yref = "paper", 
		#					    xref = "paper",
		#					    yanchor = "middle", 
		#					    x = "center",
		#					    text = "Fonte", 
		#					    showarrow = FALSE))
	
	}
	
	
	
    
    #base_aux %>%
    #  plot_ly(x = ~DescVarShort, 
    #          y = ~Valor, 
    #          type = 'bar', 
    #          color = ~Local, 
    #          #text = rotulo,
    #          hoverinfo = FALSE,
#			  hovermode = "none",
#              text = rotulo,
#              textposition = 'outside') %>%
#      layout(hovermode = "none",
#	         xaxis = list(title = ""),
#             yaxis = list(title = "", zeroline = FALSE),
#             title = paste0(input_tipo_dado, " em ", input_ano_trim, " (", input_ajuste, " Ajuste Sazonal)"),
#             legend = list(orientation = 'h')) %>%
#			 config(
#			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
#			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
#			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
    
    
  })
  
  output$fonte_ambos <- renderText({
  "Fonte: FEE/CIES/NCR e IBGE/DPE/CONAC"
  })
  
  output$fonte_ambos2 <- renderText({
  "Fonte: FEE/CIES/NCR e IBGE/DPE/CONAC"
  })
  
  output$fonte_ambos3 <- renderText({
  "Fonte: FEE/CIES/NCR e IBGE/DPE/CONAC"
  })
  
  output$fonte_comparacao_setores <- renderText({
  if(input$regiao_evolu_setores == "Brasil") paste0("Fonte: IBGE/DPE/CONAC") else paste0("Fonte: FEE/CIES/NCR")
  })
  
  output$fonte_comparacao_ajuste <- renderText({
  if(input$regiao_evolu_ajuste == "Brasil") paste0("Fonte: IBGE/DPE/CONAC") else paste0("Fonte: FEE/CIES/NCR")
  })
  
  output$fonte_tabela_compara_variacoes <- renderText({
  if(input$tabela_local_seletor == "Brasil") paste0("Fonte: IBGE/DPE/CONAC") else paste0("Fonte: FEE/CIES/NCR")
  })
  
  output$tabela_descricao <- renderTable({
    unique(select(base_trimestral, DescVarShort, DescVarTabela)) %>% rename(Variável = DescVarShort, Descrição = DescVarTabela)
  })
  
  output$evolucao_trimestral <- renderPlotly({
    
    if(input$ajuste_evolu == "radio_ajuste_evolu_sem") {input_ajuste <- "Sem"; input_tipo_dado <- input$tipo_dado_evolu1}
    if(input$ajuste_evolu == "radio_ajuste_evolu_com") {input_ajuste <- "Com"; input_tipo_dado <- input$tipo_dado_evolu2}
    
    #input_ajuste <- input$ajuste_evolu_temp
    #input_setor <- input$setor_evolu_temp
    #input_tipo_dado <- input$tipo_dado_evolu_temp
    input_setor <- input$setor_evolu_rs_br
    
    base_aux <- base_trimestral %>%
      filter(Ajuste == input_ajuste, 
             #DescVar == input_setor,
             DescVar == input_setor,
             TipoDado == input_tipo_dado) %>%
      arrange(AnoTrim) # Ordenar! 
	  
	ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = TRUE,
	  tickfont = list(size = 8.5),
	  showtickprefix = "first",
      showgrid = FALSE
    )
    
    base_aux %>%
      plot_ly(x = ~AnoTrim, y = ~Valor, type = 'scatter', mode = 'lines', color = ~Local, 
              hoverinfo="text",
              text = ~paste0(Local, " em ", AnoTrim, "<br>",
                             DescVarShort, ": ", formatC(round(Valor,1), digits = 1, big.mark = ".", format = "f", decimal.mark = ","))) %>%  # round(Valor,2)
      layout(xaxis = ax,
             yaxis = list(title = ""),
             title = paste0(input_tipo_dado, "<br>", input_setor," (", input_ajuste, " Ajuste Sazonal)"),
			 titlefont = list(size = 15), # Para caber na imagem de download o título
             legend = list(orientation = 'r')
			 #plot_bgcolor = "transparent",
	         #paper_bgcolor = "transparent"
			 ) %>%
             config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
    
    
  })
  
  
  
  
  output$evolucao_trimestral_setores <- renderPlotly({
    
    if(input$ajuste_evolu_setores == "radio_ajuste_evolu_sem_setores") {input_ajuste <- "Sem"; input_tipo_dado <- input$tipo_dado_evolu1_setores}
    if(input$ajuste_evolu_setores == "radio_ajuste_evolu_com_setores") {input_ajuste <- "Com"; input_tipo_dado <- input$tipo_dado_evolu2_setores}
    
    input_setores <- input$setor_evolu_setores
    input_local <- input$regiao_evolu_setores
    
    base_aux <- base_trimestral %>%
      filter(Ajuste == input_ajuste, 
             #DescVar == input_setor,
             Local == input_local,
             DescVar %in% input_setores,
             TipoDado == input_tipo_dado) %>%
      arrange(AnoTrim) # Ordenar!
    
    ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = TRUE,
	  tickfont = list(size = 8.5),
      showgrid = FALSE
    )
    
    base_aux %>%
      plot_ly(x = ~AnoTrim, y = ~Valor, type = 'scatter', mode = 'lines', color = ~DescVar,
              hoverinfo="text",
              text = ~paste0(Local, " em ", AnoTrim, "<br>",
                             DescVarShort, ": ", formatC(round(Valor,1), digits = 1, big.mark = ".", format = "f", decimal.mark = ","))) %>%
      layout(xaxis = ax,
             yaxis = list(title = ""),
             title = paste0(input_tipo_dado, " no ", input_local, "<br>"," (", input_ajuste, " Ajuste Sazonal)"),
			 titlefont = list(size = 15), # Para caber na imagem de download o título
             legend = list(orientation = 'r')#,
             #hovermode = 'closest' # Esse closest deixa só uma caixa quando o mouse passa em cima
			 ) %>%
			 config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
    
    
  })
  
  
  
  output$evolucao_trimestral_ajuste <- renderPlotly({
    
    input_local <- input$regiao_evolu_ajuste
    input_tipo_dado <- unique(base_trimestral$TipoDado)[1] # Série Encadeada #input$tipo_dado_evolu1_ajuste
    input_setor <- input$setor_evolu_rs_br_ajuste
    
    base_aux <- base_trimestral %>%
      filter(#Ajuste == input_ajuste, 
             DescVar == input_setor,
             TipoDado == input_tipo_dado,
             Local == input_local) %>%
      arrange(AnoTrim) # Ordenar! 
	  
	ax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = TRUE,
	  tickfont = list(size = 8.5),
      showgrid = FALSE
    )
    
    base_aux %>%
      plot_ly(x = ~AnoTrim, y = ~Valor, type = 'scatter', mode = 'lines', color = ~Ajuste,
              hoverinfo="text",
              text = ~paste0(Local, " em ", AnoTrim, "<br>",
                             DescVarShort, ": ", formatC(round(Valor,1), digits = 1, big.mark = ".", format = "f", decimal.mark = ","))) %>%
      layout(xaxis = ax,
             yaxis = list(title = ""),
             title = paste0(input_tipo_dado, "<br>", input_setor),
			 titlefont = list(size = 15), # Para caber na imagem de download o título
             legend = list(orientation = 'r')) %>%
			 
	         config(
			 #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
			 #displaylogo = FALSE, # o displaylogo é o logo do Plotly
			 modeBarButtonsToRemove = list('pan2d', 'resetScale2d', 'autoScale2d', 'zoomIn2d','zoomOut2d', 'select2d', 'zoom2d', 'hoverClosestCartesian', 'lasso2d', 'toggleSpikelines', 'sendDataToCloud'))
    
    
  })

  output$tabela_brazuca_pib <- renderFormattable({

      input_ano_trim_tabela <- input$ano_tabela_var

      input_ajuste_tabela <- input$ajuste_tabela

      input_local_tabela <- input$tabela_local_seletor

      if(input_ajuste_tabela == "radio_tabela_sem_ajuste"){
        tabela_brasileirao_pib_trim_aux <- tabela_brasileirao_pib_trim %>%
                          filter(AnoTrim == input_ano_trim_tabela & 
                               Local == input_local_tabela &
                               Ajuste == "Sem") %>%
                          arrange(Ordem) %>%
                          select(NivelHier, DescVar, `Taxa acumulada em 4 trimestres (%)`,
                               `Taxa acumulada no ano (%)`, `Taxa contra mesmo trimestre do ano anterior (%)`) %>%
                          rename(Setor = DescVar)



        fixedWidth = 100
        casas_decimais = 1
        
        formattable(tabela_brasileirao_pib_trim_aux,
          align = c("l", "r", "r", "r"),
          list(

            NivelHier = FALSE,

            Setor = formatter(.tag = "span", style = function(x) style(
                                      display = "inline-block",
                                    "text-align" = "left",
                                    "font-weight" = ifelse(tabela_brasileirao_pib_trim_aux$NivelHier == "H1", "700", "400"),
                                                                    #direction = "rtl", `border-radius` = "4px", 
                                                                      `padding-left` = ifelse(tabela_brasileirao_pib_trim_aux$NivelHier == "H1", "0px", "50px"), 
                                                                      `background-color` = "transparent")),

            "Taxa acumulada em 4 trimestres (%)" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_brasileirao_pib_trim_aux$"Taxa acumulada em 4 trimestres (%)" > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(tabela_brasileirao_pib_trim_aux$"Taxa acumulada em 4 trimestres (%)", casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_brasileirao_pib_trim_aux$"Taxa acumulada em 4 trimestres (%)" > 0, "arrow-up", "arrow-down"))
						   ),

            "Taxa acumulada no ano (%)" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_brasileirao_pib_trim_aux$"Taxa acumulada no ano (%)" > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(tabela_brasileirao_pib_trim_aux$"Taxa acumulada no ano (%)", casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_brasileirao_pib_trim_aux$"Taxa acumulada no ano (%)" > 0, "arrow-up", "arrow-down"))
						   ),

            "Taxa contra mesmo trimestre do ano anterior (%)" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_brasileirao_pib_trim_aux$"Taxa contra mesmo trimestre do ano anterior (%)" > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(tabela_brasileirao_pib_trim_aux$"Taxa contra mesmo trimestre do ano anterior (%)", casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_brasileirao_pib_trim_aux$"Taxa contra mesmo trimestre do ano anterior (%)" > 0, "arrow-up", "arrow-down"))
						   )





            ))

      } else if (input_ajuste_tabela == "radio_tabela_com_ajuste"){
        tabela_brasileirao_pib_trim_aux <- tabela_brasileirao_pib_trim %>%
                          filter(AnoTrim == input_ano_trim_tabela & 
                               Local == input_local_tabela &
                               Ajuste == "Com") %>%
                          arrange(Ordem) %>%
                          select(NivelHier, DescVar, `Taxa contra trimestre imediatamente anterior (%)`) %>%
                          rename(Setor = DescVar)

        fixedWidth = 100
        casas_decimais = 1
        
        formattable(tabela_brasileirao_pib_trim_aux,
          align = c("l", "r"),
          list(

            NivelHier = FALSE,

            Setor = formatter(.tag = "span", style = function(x) style(
                                      display = "inline-block",
                                    "text-align" = "left",
                                    "font-weight" = ifelse(tabela_brasileirao_pib_trim_aux$NivelHier == "H1", "700", "400"),
                                                                    #direction = "rtl", `border-radius` = "4px", 
                                                                      `padding-left` = ifelse(tabela_brasileirao_pib_trim_aux$NivelHier == "H1", "0px", "50px"), 
                                                                      `background-color` = "transparent")),

            "Taxa contra trimestre imediatamente anterior (%)" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_brasileirao_pib_trim_aux$"Taxa contra trimestre imediatamente anterior (%)" > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(tabela_brasileirao_pib_trim_aux$"Taxa contra trimestre imediatamente anterior (%)", casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_brasileirao_pib_trim_aux$"Taxa contra trimestre imediatamente anterior (%)" > 0, "arrow-up", "arrow-down"))
						   )

          ))
                          
      }

    })

  output$tabela_brazuca_pib_brrs <- renderFormattable({

    input_indicador_escolhido <- input$tabela_indi_seletor_brrs

    input_ano_trim_tabela_brrs <- input$ano_tabela_var_brrs

    #index_var <- grep(^input_indicador_escolhido$, colnames(tabela_brasileirao_pib_trim))

    tabela_brasileirao_pib_trim_br <- base_trimestral %>%
                        filter(Local == "Brasil",
                             AnoTrim == input_ano_trim_tabela_brrs,
                             TipoDado == input_indicador_escolhido) %>%
                        arrange(Ordem) %>%
                        select(Ordem, NivelHier, DescVar, Valor) %>%
                        rename(Setor = DescVar)

    colnames(tabela_brasileirao_pib_trim_br) <- c("Ordem", "NivelHier", "Setor", "Brasil")

    tabela_brasileirao_pib_trim_rs <- base_trimestral %>%
                        filter(Local == "Rio Grande do Sul",
                             AnoTrim == input_ano_trim_tabela_brrs,
                             TipoDado == input_indicador_escolhido) %>%
                        arrange(Ordem) %>%
                        select(Ordem, NivelHier, DescVar, Valor) %>%
                        rename(Setor = DescVar)

    colnames(tabela_brasileirao_pib_trim_rs) <- c("Ordem", "NivelHier", "Setor", "Rio Grande do Sul")

    tabela_brasileirao_pib_trim_brrs <- merge(x = tabela_brasileirao_pib_trim_br,
                          y = tabela_brasileirao_pib_trim_rs,
                          by.x = c("NivelHier", "Setor", "Ordem"),
                          by.y = c("NivelHier", "Setor", "Ordem"))

    #formattable(tabela_brasileirao_pib_trim_brrs)

    tabela_brasileirao_pib_trim_brrs <- tabela_brasileirao_pib_trim_brrs %>%
                        arrange(Ordem)

    fixedWidth = 100
      casas_decimais = 1

    formattable(tabela_brasileirao_pib_trim_brrs,
          align = c("l", "r", "r"),
          list(

            Ordem = FALSE,

            NivelHier = FALSE,

            Setor = formatter(.tag = "span", style = function(x) style(
                                      display = "inline-block",
                                    "text-align" = "left",
                                    "font-weight" = ifelse(tabela_brasileirao_pib_trim_brrs$NivelHier == "H1", "700", "400"),
                                                                    #direction = "rtl", `border-radius` = "4px", 
                                                                      `padding-left` = ifelse(tabela_brasileirao_pib_trim_brrs$NivelHier == "H1", "0px", "50px"), 
                                                                      `background-color` = "transparent")),

            "Brasil" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_brasileirao_pib_trim_brrs$"Brasil" > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(tabela_brasileirao_pib_trim_brrs$"Brasil", casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_brasileirao_pib_trim_brrs$"Brasil" > 0, "arrow-up", "arrow-down"))
						   ),

          "Rio Grande do Sul" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_brasileirao_pib_trim_brrs$"Rio Grande do Sul" > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(tabela_brasileirao_pib_trim_brrs$"Rio Grande do Sul", casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_brasileirao_pib_trim_brrs$"Rio Grande do Sul" > 0, "arrow-up", "arrow-down"))
						   )


          ))

    })

  	output$titulo_tabela_pib <- renderText({


  	input_ano_trim_tabela <- input$ano_tabela_var

    input_local_tabela <- input$tabela_local_seletor

    paste0("Taxas de Crescimento dos Setores do PIB Trimestral, por atividades, do ", input_local_tabela, " em ", input_ano_trim_tabela)


  	})

  	output$titulo_tabela_pib_brrs <- renderText({

		input_indicador_escolhido <- input$tabela_indi_seletor_brrs

    	input_ano_trim_tabela_brrs <- input$ano_tabela_var_brrs

    	if(input_indicador_escolhido == "Taxa contra trimestre imediatamente anterior (%)"){
    			paste0(input_indicador_escolhido, " dos setores do PIB Trimestral, por atividades, em ", input_ano_trim_tabela_brrs, " - Com ajuste sazonal")
    	} else{
    			paste0(input_indicador_escolhido, " dos setores do PIB Trimestral, por atividades, em ", input_ano_trim_tabela_brrs, " - Sem ajuste sazonal")	
    	}



  		})
		
		
		
		
		
    output$tabela_temporal_pib_trim <- renderFormattable({

    input_setor <- input$setor_tabela_temporal
    input_local <- input$tabela_temporal_local

    k <- 4 # Número de períodos (colunas) da tabela

	tabela_trans_aux <- tabela_tempo_transposto_pib_trim %>%
						filter(DescVar == input_setor,
							   Local == input_local,
							   TipoDado != "Série Encadeada (2002 = 100)") %>%
						select(c(`TipoDado`, (ncol(.) - k):(ncol(.)))) # CUIDADO! Mantém as primeiras colunas! Se modificar no tradutor, pode ser que o número de colunas iniciais a se manter pode mudar.


    colnames(tabela_trans_aux)[1] <- "Taxas"
	#colnames(tabela_trans_aux)[2] <- "Teste"
	#teste <- colnames(tabela_trans_aux)[2]


    #formattable(tabela_trans_aux)
	casas_decimais = 1
	fixedWidth = 350
	
    formattable(tabela_trans_aux,
          align = c("l", "r", "r", "r", "r", "r"),
          list(

            "Taxas" = formatter(.tag = "span", style = function(x) style(
                                      display = "inline-block",
                                    "text-align" = "left",
									`background-color` = "transparent")),
									
			"2016.II" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_trans_aux[,2] > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(unlist(tabela_trans_aux[,2]), casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_trans_aux[,2] > 0, "arrow-up", "arrow-down"))
						   ),
						   
			"2016.III" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_trans_aux[,3] > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(unlist(tabela_trans_aux[,3]), casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_trans_aux[,3] > 0, "arrow-up", "arrow-down"))
						   ),
						   
			"2016.IV" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_trans_aux[,4] > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(unlist(tabela_trans_aux[,4]), casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_trans_aux[,4] > 0, "arrow-up", "arrow-down"))
						   ),
						   
			"2017.I" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_trans_aux[,5] > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(unlist(tabela_trans_aux[,5]), casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_trans_aux[,5] > 0, "arrow-up", "arrow-down"))
						   ),
						   
			"2017.II" = formatter("span",
                           style = x ~ style(color = ifelse(tabela_trans_aux[,6] > 0, "green", "red"),
                                    width = paste0(fixedWidth, "px", sep = "")),
                           format(round(unlist(tabela_trans_aux[,6]), casas_decimais), format = "f", decimal.mark = ",")#,
                           #x ~ icontext(ifelse(tabela_trans_aux[,6] > 0, "arrow-up", "arrow-down"))
						   )


          ))
    




    })
	
	
	output$titulo_tabela_temporal <- renderText({

    input_setor <- input$setor_tabela_temporal
    input_local <- input$tabela_temporal_local

	paste0("Taxas de Crescimento do ", input_local, " - ", input_setor)



	})
	
	output$fonte_tabela_temporal <- renderText({
	if(input$tabela_temporal_local == "Brasil") paste0("Fonte: IBGE/DPE/CONAC") else paste0("Fonte: FEE/CIES/NCR")
	})
		
		
  
  
  
  
})
