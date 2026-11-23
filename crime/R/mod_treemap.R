# Módulo "Representação no estado": treemap da participação de cada município
# no total do estado (ocorrências ou população).
# Outputs: tree_map

treemapUI <- function(id) {
  ns <- NS(id)
  tabPanel("Representação no estado",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput(ns('ano_tree'), 'Ano a ser escolhido', 
                                      min = min(base_crime$Ano),
                                      max = max(base_crime$Ano),
                                      value = max(base_crime$Ano),
                                      step = 1,
                                      animate = animationOptions(interval = 1750, loop = TRUE), sep=""),
                          
                          conditionalPanel(condition = ("input.tipo_dado_tree != 'pop_radio_tree'"), ns = ns,
                          selectInput(ns('crime_tree'), 'Crime a ser escolhido', 
                                      as.character(unique(base_crime$Crime)),
                                      selected = "Homicídio Doloso")),
                          
                          radioButtons(ns("tipo_dado_tree"), "Tipo de Informação:",
                                       c("Número de Ocorrências" = "ocorre_radio_tree",
                                         "População" = "pop_radio_tree"))
                          
                        ),
                        
                        # Show the plots
                        mainPanel(
                          d3plusOutput(ns("tree_map"))
                        )
                      ))
}

treemapServer <- function(id) {
  moduleServer(id, function(input, output, session) {

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
      
    })
  })
}
