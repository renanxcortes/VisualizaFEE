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
  })
  
  
  
  output$plot_series <- renderPlotly({
    
	if(stri_conv(as.character(input$series_compara_hierarquia), "UTF-8", "latin1") == "Comércio Confiança") series_escolhidas <- "Comércio - Intenção de Consumo" else series_escolhidas <- input$series_compara_hierarquia # Hardcodeado porque ele estava quebrando o server no input 'Comércio - Confiança'
	
	if (!input$check_series_sazonal) {base_analise <- base_transformada; subtitulo_serie <- "Sem ajuste sazonal"} else
	                                 {base_analise <- base_transformada_SA; subtitulo_serie <- "Com ajuste sazonal"}
	
	base_aux <- base_analise %>% 
                filter(NomeVar == series_escolhidas) %>%
	            select(Indice, Data, NomeVar, NomeVarTabela, UnidadeMedida, DescVar_Tooltips, date_my) # Pega só o índice pra não dar problema no na.omit posterior das recessões
	
	if (!input$check_recessoes) {
	  
	  
	  base_aux %>%
	    plot_ly(
	      x = ~ Data,
	      y = ~ Indice,
	      type = 'scatter',
	      mode = 'lines',
	      color = ~ NomeVar,
	      line = list(width = 3),
	      hoverinfo = "text",
	      text = paste(
	        "",
	        base_aux$DescVar_Tooltips,
	        "<br>",
	        "Valor: ",
	        round(base_aux$Indice, casas_decimais),
	        "<br>",
	        "Data: ",
	        base_aux$date_my
	      )
	    ) %>%
	    layout(
	      title = paste0(unique(base_aux$NomeVar), "<br>", subtitulo_serie),
	      xaxis = list(title = ""),
	      yaxis = list(title = unique(base_aux$UnidadeMedida)),
	      plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
	      paper_bgcolor = "transparent"
	    ) %>%
	    config(
	      modeBarButtonsToRemove = list(
	        'pan2d',
	        'resetScale2d',
	        'autoScale2d',
	        'zoomIn2d',
	        'zoomOut2d',
	        'select2d',
	        'zoom2d',
	        'hoverClosestCartesian',
	        'lasso2d',
	        'toggleSpikelines',
	        'sendDataToCloud'
	      )
	    )
	  
	}
	
	else {
	  
	  # Vetor sem períodos anteriores a existencia de dados
	  picos<-c("1989-06-01","1994-12-01","1997-10-01","2000-12-01","2002-10-01","2008-07-01","2014-02-01")
	  vales<-c("1991-12-01","1995-09-01","1999-02-01","2001-09-01","2003-06-01","2009-01-01","2016-12-01") # Se uma recessão ainda está em vigor o ponto máximo deve ser: as.character(max(aux_datas$Data)))
	  
	  opa_rec <- 0.25 # Opacidade recessão
	  opa_rec_final <- 0.1 # Opacidade recessão ainda vigente
	  col_rec <- "gray" # Cor recessão
	  
	  x_min_serie <- min(na.omit(base_aux)$Data) # Controle do eixo x, pois os retangulos de todas as recessões são incluídos
	  x_max_serie <- max(na.omit(base_aux)$Data)
	  
	  base_aux %>%
	    plot_ly(
	      x = ~ Data,
	      y = ~ Indice,
	      type = 'scatter',
	      mode = 'lines',
	      color = ~ NomeVar,
	      line = list(width = 3),
	      hoverinfo = "text",
	      text = paste(
	        "",
	        base_aux$DescVar_Tooltips,
	        "<br>",
	        "Valor: ",
	        round(base_aux$Indice, casas_decimais),
	        "<br>",
	        "Data: ",
	        base_aux$date_my
	      )
	    ) %>% 
	    layout(title = paste0(unique(base_aux$NomeVar), "<br>", subtitulo_serie),
	           xaxis = list(title = "", range = c(x_min_serie, x_max_serie)),
	           yaxis = list(title = unique(base_aux$UnidadeMedida)),
	           shapes = list(
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[1], x1 = vales[1], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[2], x1 = vales[2], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[3], x1 = vales[3], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[4], x1 = vales[4], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[5], x1 = vales[5], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[6], x1 = vales[6], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below"),
	             
	             list(type = "rect",
	                  fillcolor = col_rec, line = list(color = col_rec), opacity = opa_rec,
	                  x0 = picos[7], x1 = vales[7], xref = "x",
	                  y0 = min(base_aux$Indice, na.rm = T), y1 = max(base_aux$Indice, na.rm = T), yref = "y",
	                  layer = "below")
	             
	           ),
	           
	    		 plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
	         paper_bgcolor = "transparent"
		) %>%
	    config(
	      #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
	      #displaylogo = FALSE, # o displaylogo é o logo do Plotly
	      modeBarButtonsToRemove = list(
	        'pan2d',
	        'resetScale2d',
	        'autoScale2d',
	        'zoomIn2d',
	        'zoomOut2d',
	        'select2d',
	        'zoom2d',
	        'hoverClosestCartesian',
	        'lasso2d',
	        'toggleSpikelines',
	        'sendDataToCloud'
	      )
	    )
	  
	  
	}
	  
  
    })

  output$fonte_series <- renderText({

  if(input$series_compara_hierarquia != "Comércio Confiança") {
  series_escolhidas <- input$series_compara_hierarquia
  aux <- base_transformada %>% filter(NomeVar == series_escolhidas)
  paste0("Fonte: ", unique(aux$Fonte))} else {paste0("Fonte: Fecomércio")}
  
  })
  
  
  output$explicacao_series <- renderText({
  if(input$series_compara_hierarquia != "Comércio Confiança") {
  series_escolhidas <- input$series_compara_hierarquia
  aux <- base_transformada %>% filter(NomeVar == series_escolhidas)
  paste0("Descrição: ", unique(aux$Explicacao))} else {paste0("Descrição: Intenção de Consumo das Famílias (ICF) no Rio Grande do Sul.")}
  })
  
  

  output$plot_monitor <- renderPlotly({
  
  if(input$check_antecedente)  {a <- "Antecedente"} else {a <- NA}
	if(input$check_coincidente)  {b <- "Coincidente"} else {b <- NA}
	if(input$check_defasada)     {d <- "Defasada"} else {d <- NA}

  input_ano <- lubridate::year(ymd(input$data_slider_monitor))
  input_mes <- lubridate::month(ymd(input$data_slider_monitor))
	input_categoria <- c(a,b,d)
	
	aux_eixos_fixos <- base_transformada

  base_aux <- base_transformada %>%
                  filter(year == input_ano, 
                         month == input_mes, 
                         CategoriaPIB %in% input_categoria, 
                         Categoria != "Índices de Preços") %>%
                  na.omit()
	
																			   
  if (input$check_incluir_PIB_monitor) {
    base_pib_aux <- base_pib %>%
                    filter(Ano == input_ano, month == input_mes) %>%
                    mutate(PIB_X = Acelera_Acum_4T, PIB_Y = Cresc_Acum_4T) %>%
                    select(PIB_X, PIB_Y)
    
    if (is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {
      texto_flecha_pib <- ""
      booleano_flecha <- F
      base_pib_aux[1, ] <- NA
    } else {
      texto_flecha_pib <- "<b>PIB</b>"
      booleano_flecha <- T
    }
  }
  
  if (!input$check_incluir_PIB_monitor) {
    base_pib_aux <- base_pib %>%
      filter(Ano == input_ano, month == input_mes) %>%
      mutate(PIB_X = NA, PIB_Y = NA)
    if (is.na(base_pib_aux$PIB_X) || length(base_pib_aux$PIB_X) == 0) {
      texto_flecha_pib <- ""
      booleano_flecha <- F
      base_pib_aux[1, ] <- NA
    } else {
      texto_flecha_pib <- "<b>PIB</b>"
      booleano_flecha <- T
    }
  }
	

	# Parâmetros Gráficos
	opacidade_cor <- 0.3
	cores_estagios <- c("red", "yellow", "green", "orange") # Sentido horário

	f <- list(size = 14, color = "black")
	
  base_aux <- rename(base_aux, 
	                   Variavel_X = Acelera_Acum_12M, 
	                   Variavel_Y = Cresci_Acum_12M)
	tipo_variacao <- "Variação acum. em 12 meses"

	
    if (!input$check_monitor_eixos_fixos) {
    limite_cores_x <- 1.05 * max(abs(c(base_aux$Variavel_X, base_pib_aux$PIB_X)), na.rm = T) # Percentual a mais do que o máximo filtrado (e o PIB)
    limite_cores_y <- 1.05 * max(abs(c(base_aux$Variavel_Y, base_pib_aux$PIB_Y)), na.rm = T)
	}
	
	
	if (input$check_monitor_eixos_fixos) {
	  
	limite_cores_x <- 1.05 * max(c(abs(aux_eixos_fixos$Acelera_Acum_12M), abs(base_pib_aux$PIB_X)), na.rm = T) # Percentual a mais do que o máximo filtrado
  limite_cores_y <- 1.05 * max(c(abs(aux_eixos_fixos$Cresci_Acum_12M), abs(base_pib_aux$PIB_Y)), na.rm = T)
	
  }

    # Gráfico da Dinâmica Macroeconômica
    base_aux %>%
      plot_ly(x = ~Variavel_X, 
              y = ~Variavel_Y, 
              type = 'scatter', 
              mode = 'markers', 
              color = ~NomeVar,
              marker = list(size = 13, sizeref = .15, color = ifelse(base_aux$CategoriaPIB == "Coincidente", "#004B82", 
															  ifelse(base_aux$CategoriaPIB == "Antecedente", "#004B82", "#004B82")),
															  symbol = 1:length(base_aux$NomeVar) + 12), # Controla o tamanho do marker # color = "#004B82" color = ifelse(base_aux$CategoriaPIB == "Coincidente", "#004B82", ifelse(base_aux$CategoriaPIB == "Antecedente", "#FF0000", "#00FF00"))
              hoverinfo = "text",
              text = paste("", base_aux$DescVar_Tooltips, "<br>",
                           "Crescimento: ", round(base_aux$Variavel_Y, casas_decimais), "<br>",
                           "Aceleração: ", round(base_aux$Variavel_X, casas_decimais)),
              showlegend = TRUE) %>%

      layout(title = paste0("Dinâmica em ", retorna_mes(input_mes) , " de ", input_ano, " — ", tipo_variacao),
             shapes = list(
               list(type = "rect",
                    fillcolor = cores_estagios[1], line = list(color = cores_estagios[1]), opacity = opacidade_cor,
                    x0 = -limite_cores_x, x1 = 0, xref = "x",
                    y0 = -limite_cores_y, y1 = 0, yref = "y",
                    layer = "below"),
               list(type = "rect",
                    fillcolor = cores_estagios[2], line = list(color = cores_estagios[2]), opacity = opacidade_cor,
                    x0 = -limite_cores_x, x1 = 0, xref = "x",
                    y0 = 0, y1 = limite_cores_y, yref = "y",
                    layer = "below"),
               list(type = "rect",
                    fillcolor = cores_estagios[3], line = list(color = cores_estagios[3]), opacity = opacidade_cor,
                    x0 = 0, x1 = limite_cores_x, xref = "x",
                    y0 = 0, y1 = limite_cores_y, yref = "y",
                    layer = "below"),
               list(type = "rect",
                    fillcolor = cores_estagios[4], line = list(color = cores_estagios[4]), opacity = opacidade_cor,
                    x0 = 0, x1 = limite_cores_x, xref = "x",
                    y0 = -limite_cores_y, y1 = 0, yref = "y",
                    layer = "below")),

             xaxis = list(
               zeroline = FALSE,
               showline = FALSE,
               showgrid = FALSE,
               range = c(-limite_cores_x, limite_cores_x),
               title = "Aceleração (p.p.)",
               titlefont = f
             ),

             yaxis = list(
               zeroline = FALSE,
               showline = FALSE,
               showgrid = FALSE,
               range = c(-limite_cores_y, limite_cores_y),
               title = "Crescimento (%)",
               titlefont = f
             ),
			 
      			 plot_bgcolor = "transparent", # Plot_bg serve para descolorir o fundo do plot apenas
      	     paper_bgcolor = "transparent"
      			 
      			 )%>%
			 
      add_annotations(
        x = base_pib_aux$PIB_X,
        y = base_pib_aux$PIB_Y,
        text = texto_flecha_pib,
        showarrow = booleano_flecha
      ) %>%
  
      add_markers(
        x = base_pib_aux$PIB_X,
        y = base_pib_aux$PIB_Y,
        text = paste0(
          "PIB",
          "<br>",
          "Crescimento:",
          round(base_pib_aux$PIB_Y, casas_decimais),
          "<br>",
          "Aceleração:",
          round(base_pib_aux$PIB_X, casas_decimais)
        ),
        xanchor = 'center',
        marker = list(color = "black", symbol = 500),
        showlegend = FALSE
      ) %>%
			 

	
      config(
        #displayModeBar = T, # Mostra SEMPRE a barra de ferramentas se TRUE
        #displaylogo = FALSE, # o displaylogo é o logo do Plotly
        modeBarButtonsToRemove = list(
          'pan2d',
          'resetScale2d',
          'autoScale2d',
          'zoomIn2d',
          'zoomOut2d',
          'select2d',
          'zoom2d',
          'hoverClosestCartesian',
          'lasso2d',
          'toggleSpikelines',
          'sendDataToCloud'
        )
      )
    
  })
  
	
	output$titulo_tabela <- renderText({
	
	ano_data_tabela <- lubridate::year(ymd(input$data_jogo))
	mes_data_tabela <- lubridate::month(ymd(input$data_jogo))
	
	paste0("Tabela Sintética de Variações em ", retorna_mes(mes_data_tabela), " de ", ano_data_tabela)
	
	})
	
	output$tabela_brazuca <- renderFormattable ({
	
	# Séries contracíclicas
  series_contra_ci <- c("s_1005", "s_1006", "s_1016")
	
	casas_decimais <- 1
	
	data_analise <- input$data_jogo
	
	input_ano <- lubridate::year(ymd(data_analise))
	input_mes <- lubridate::month(ymd(data_analise))
	input_grupo <- input$grupo_tabela
	
	if(!input$check_grupo_tabela) {
	tabela_brasileirao <- tabela_variacoes_pre %>%
          							filter(year == input_ano, month == input_mes) %>%
          							arrange(Ordem) %>% 
          							select(NomeVarTabela, 
          							       Indice, 
          							       Cresci_Mensal_12M, 
          							       Cresci_Acum_12M, 
          							       Cresci_Mensal_1M, 
          							       Cresci_Acum_3M_JM, 
          							       Serie, 
          							       Categoria)}
	
	if(input$check_grupo_tabela) {
	tabela_brasileirao <- tabela_variacoes_pre %>%
          							filter(year == input_ano, month == input_mes, Categoria == input_grupo) %>%
          							arrange(Ordem) %>% 
          							select(NomeVarTabela, 
          							       Indice, 
          							       Cresci_Mensal_12M, 
          							       Cresci_Acum_12M, 
          							       Cresci_Mensal_1M, 
          							       Cresci_Acum_3M_JM, 
          							       Serie, 
          							       Categoria)}

	fixedWidth = 295

	colnames(tabela_brasileirao) <- c("Nome da Série", "Valor Bruto", "Mês contra mesmo mês do ano anterior*", "Média em 12 meses*", "Mês contra mês imediatamente anterior**", "Média Móvel 3 Meses**", "Cod Série", "Categoria")
	
	
	formattable(tabela_brasileirao, 
		align = c("l", "c", "r", "r", "r", "r"),
		list(

		"Nome da Série" = formatter(.tag = "span", style = function(x) style(
																display = "inline-block",
																"text-align" = "left",
                                `background-color` = "transparent",
                                width = paste0(fixedWidth, "px", sep = ""))),


		"Valor Bruto" = formatter("span",
						 ifelse(tabela_brasileirao$"Nome da Série" == "Agronegócio (pessoas ocupadas)", 
						 formatC(tabela_brasileirao$"Valor Bruto", digits = 0, big.mark = ".", format = "f", decimal.mark = ","), 
						 formatC(tabela_brasileirao$"Valor Bruto", digits = 1, big.mark = ".", format = "f", decimal.mark = ","))
						 ),

		"Mês contra mesmo mês do ano anterior*" = formatter("span",
                         style = x ~ style(color = ifelse(tabela_brasileirao$"Categoria" == "Índices de Preços", NA, ifelse(tabela_brasileirao$"Mês contra mesmo mês do ano anterior*" > 0, ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "green", "red"))), width = paste0(200, "px", sep = "")),
                         format(round(tabela_brasileirao$"Mês contra mesmo mês do ano anterior*", casas_decimais), format = "f", decimal.mark = ","),
                         x ~ icontext(ifelse(tabela_brasileirao$"Mês contra mesmo mês do ano anterior*" > 0, "arrow-up", "arrow-down"))),
		
		"Média em 12 meses*" = formatter("span",
                                style = x ~ style(color = ifelse(tabela_brasileirao$"Categoria" == "Índices de Preços", NA, ifelse(tabela_brasileirao$"Média em 12 meses*" > 0, ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "green", "red")))),
                                format(round(tabela_brasileirao$"Média em 12 meses*", casas_decimais), format = "f", decimal.mark = ","),
                                x ~ icontext(ifelse(tabela_brasileirao$"Média em 12 meses*" > 0, "arrow-up", "arrow-down"))),
		
		"Mês contra mês imediatamente anterior**" = formatter("span",
                                style = x ~ style(color = ifelse(tabela_brasileirao$"Categoria" == "Índices de Preços", NA, ifelse(tabela_brasileirao$"Mês contra mês imediatamente anterior**" > 0, ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "green", "red")))),
                                format(round(tabela_brasileirao$"Mês contra mês imediatamente anterior**", casas_decimais), format = "f", decimal.mark = ","),
                                x ~ icontext(ifelse(tabela_brasileirao$"Mês contra mês imediatamente anterior**" > 0, "arrow-up", "arrow-down"))),
		
		"Média Móvel 3 Meses**" = formatter("span",
                                style = x ~ style(color = ifelse(tabela_brasileirao$"Categoria" == "Índices de Preços", NA, ifelse(tabela_brasileirao$"Média Móvel 3 Meses**" > 0, ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "red", "green"), ifelse(tabela_brasileirao$"Cod Série" %in% series_contra_ci, "green", "red")))),
                                format(round(tabela_brasileirao$"Média Móvel 3 Meses**", casas_decimais), format = "f", decimal.mark = ","),
                                x ~ icontext(ifelse(tabela_brasileirao$"Média Móvel 3 Meses**" > 0, "arrow-up", "arrow-down"))),
								
		"Cod Série" = FALSE,
		"Categoria" = FALSE
))
	
	})
	
	
	
	
	output$tabela_download_sem_ajuste = renderDataTable({
    datatable(base_transformada_download, options(list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
      pageLength = 15)))
  })
  
  
  datasetInput_sem <- reactive({
  base_transformada_download
  })
  
  
  output$downloadData_sem <- downloadHandler(
    filename = function() { paste('base_sem_ajuste', '.csv', sep = '') },
    content = function(file) {
      write.csv(datasetInput_sem(), file, fileEncoding = "latin1")
  })
  
  	
	output$tabela_download_com_ajuste = renderDataTable({
    datatable(base_transformada_SA_download, options(list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'), 
      pageLength = 15)))
  })
  
  
  datasetInput_com <- reactive({
  base_transformada_SA_download
  })
  
  
  output$downloadData_com <- downloadHandler(
    filename = function() { paste('base_com_ajuste', '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput_com(), file, fileEncoding = "latin1")
    }
  )

  output$collapsible_meg <- renderCollapsibleTree({

  	tradutor_aux <- tradutor %>%
  					        filter(CodVar %in% unique(base_transformada$Serie))
  	
  	collapsibleTree(tradutor_aux,
  		hierarchy = c("Categoria", "NomeVar"),
  		root = "MEG",
  		fontSize = 15)
  	})
	
  output$downloadMetodologia <- downloadHandler(
    filename = "Metodologia_MEG.pdf",
    content = function(file) {
      file.copy("www/meg_metodologia.pdf", file)
    }
  )
  

})


