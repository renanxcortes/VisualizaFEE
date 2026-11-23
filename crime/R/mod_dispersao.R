# Módulo "Relação entre crimes": dispersão entre dois crimes por município,
# com opção de agrupar municípios por k-means.
# Outputs: dispersao

dispersaoUI <- function(id) {
  ns <- NS(id)
  tabPanel("Relação entre crimes",
                                   sidebarLayout(
                                     sidebarPanel(
                                       
                                       sliderInput(ns('ano_disp'), 'Ano a ser escolhido', 
                                                   min = min(base_crime$Ano),
                                                   max = max(base_crime$Ano),
                                                   value = max(base_crime$Ano),
                                                   step=1,
                                                   animate = animationOptions(interval = 1750, loop = TRUE), sep=""), # 1500
                                       
								       sliderInput(ns('dispersao_mun_pop_control'), "População:",
						                           min = 0,
						                           max = 1500000,
						                           value = range(c(0, 1500000)),
						                           step = 2500),
									   
                                       selectInput(ns('crimex'), 'Selecione o crime do eixo horizontal', 
                                                   as.character(unique(base_crime$Crime)),
                                                   selected = "Homicídio Doloso"),
                                       
                                       selectInput(ns('crimey'), 'Selecione o crime do eixo vertical', 
                                                   as.character(unique(base_crime$Crime)),
                                                   selected = "Roubo"),
                                       
                                       radioButtons(ns("tipo_dado_disp"), "Tipo de Informação:",
                                                    c("Número de Ocorrências" = "ocorre_radio_disp",
                                                      "Taxa por 100.000" = "taxa_radio_disp")),

                                       fluidRow(
                                         column(8, checkboxInput(ns("checkbox_kmeans"), label = "Criar grupos de municípios?", 
                                                                 value = FALSE)),
                                        column(4, bsButton(ns("q_kmeans"), label = "Ajuda", icon = icon("info"),
                                                           style = "info", size = "medium")),
                                        bsPopover(id = ns("q_kmeans"), title = "K-Means",
                                                 content = paste0("Os grupos de municípios a serem criados seguem o algoritmo de clusterização denominado ", strong("K-Means"), ". Este algoritmo agrupa cidades similares entre si de acordo com as suas distâncias entre os crimes escolhidos. Isto é, municípios que tem ocorrências ou taxas próximas, estarão no mesmo grupo. Para maiores informações veja este ", 
                                                                  a("link metodológico", 
                                                                    href = "https://www.coursera.org/learn/machine-learning/lecture/93VPG/k-means-algorithm",
                                                                    target="_blank"),"."),
                                                 placement = "right", 
                                                 trigger = "focus", # O 'click' só fechava quando clicava denovo 
                                                 options = list(container = "body")
                                       )),
                                       
                                       conditionalPanel(condition = 'input.checkbox_kmeans', ns = ns,
                                                        numericInput(ns("n_grupos_kmeans"), "Número de Grupos:", 3, min = 2, max = 496)
                                                        )
                                       
                                     ),
                                     
                                     # Show the plots
                                     mainPanel(
                                       plotlyOutput(ns("dispersao"))
                                     )
                                   ))
}

dispersaoServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$dispersao <- renderPlotly({
      
      ano <- input$ano_disp
      slider_pop_min <-input$dispersao_mun_pop_control[1]
      slider_pop_max <- input$dispersao_mun_pop_control[2]
      
      crime_x <- input$crimex
      crime_y <- input$crimey

      crimes_com_dados <- base_crime %>% filter(Ano == ano, Crime %in% c(crime_x, crime_y)) %>% pull(Crime) %>% unique()
      shiny::validate(shiny::need(
        crime_x %in% crimes_com_dados && crime_y %in% crimes_com_dados,
        "Um dos crimes selecionados não possui dados para o ano escolhido."
      ))

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
  })
}
