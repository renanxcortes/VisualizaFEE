# Módulo "Download dos Dados": tabela pesquisável da base e botão de download.
# Outputs: tabela, downloadData

dadosUI <- function(id) {
  ns <- NS(id)
  tabPanel("Download dos Dados",
                      
                      sidebarLayout(
                        sidebarPanel(
                          h2("Tabela de Dados"),
                          p("Pesquise as informações de seu interesse ao lado."),
						              br(),
                          p("Se você deseja fazer o ", strong("download"), "da base completa, clique no ícone abaixo."),
                          downloadButton(ns('downloadData'), 'Baixar .csv')
                        ),
                        
                        # Show the plots
                        mainPanel(
                          dataTableOutput(ns("tabela"))
                        )
                      ))
}

dadosServer <- function(id) {
  moduleServer(id, function(input, output, session) {

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
  })
}
