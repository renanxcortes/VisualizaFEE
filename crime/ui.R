##################
# User Interface #
##################

shinyUI(fluidPage(
        includeCSS("estilocrime.css"),
        theme = shinytheme("united"),
        tags$style(type="text/css", # isso é para não mostrar nenhuma mensagem vermelha de erro!!
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
        ),

        # Essa parte de baixo dá uma "empurradinha" pra direita o navbarheader pra aparecer o nome do app: https://stackoverflow.com/questions/9792849/how-to-insert-spaces-tabs-in-text-using-html-css
        tags$style(type = 'text/css',
        
                                   '.navbar-default .navbar-brand {
        							 padding-left: 35px;
                                   }'
        
        ),  

  # Cada aba é um módulo definido em R/mod_*.R (o Shiny carrega a pasta R/ sozinho).
  # O argumento de cada xxxUI() é o id do módulo: ele vira prefixo dos inputs/outputs
  # (ex.: "mapa-ano_mapa") e precisa ser o mesmo usado no xxxServer() do server.R.
  navbarPage("VisualizaCrimeRS", # Navegação
    tabPanel("Mapas",
      mainPanel(width = 12,
        tabsetPanel(type = 'tabs',
          mapaUI("mapa"),
          markovUI("markov")
        )
      )
    ),
    navbarMenu(title = "Séries Temporais",
      comparaCrimesUI("compara_crimes"),
      comparaMunicipiosUI("compara_municipios")
    ),
    dispersaoUI("dispersao"),
    treemapUI("treemap"),
    dadosUI("dados"),
    sobreUI("sobre")
  )

  # Fim do User Interface
))
