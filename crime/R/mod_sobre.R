# Módulo "Sobre": texto institucional do app. Só tem UI (nenhum output).

sobreUI <- function(id) {
  ns <- NS(id)
  tabPanel("Sobre", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          h2("Sobre o VisualizaCrimeRS"),
                          p("O VisualizaCrimeRS é um aplicativo de visualização de dados de criminalidade do Rio Grande do Sul, desenvolvido por Renan Xavier Cortes."),
                          br(),
                          br(),
                          "Shiny é um produto do ",
                          span("RStudio", style = "color:blue"),".", 
                          br(), 
                          br(),
                          img(src = "bigorb.png", height = 82, width = 92)
                        ),
                        mainPanel(
                          h1("Visualização da criminalidade anual no Rio Grande do Sul"),
                          p("O aplicativo VisualizaCrimeRS apresenta,
                            de maneira interativa e dinâmica, os crimes dos municípios do Rio Grande do Sul (RS)
                            com dados anuais disponibilizados pela Secretaria de Segurança Pública do RS (SSP-RS). Seus
                            dados brutos podem ser acessados no ",em("site")," da SSP-RS neste",
                            a("link", href = "http://www.ssp.rs.gov.br/"), "na parte de indicadores
                            criminais."),
                          p("Os dados de população utilizados para o cálculo das taxas são estimativas da ",
                            a("Rede Interagencial de Informações para a Saúde (RIPSA)", href = "https://www.gov.br/saude/pt-br/composicao/seidigi/demas/ripsa", target="_blank"), "."),
                          br(),
                          p("O VisualizaCrimeRS foi desenvolvido com o uso da ferramenta gratuita Shiny. Para uma introdução e outros exemplos, acesse ",
                            a("Shiny homepage.", 
                              href = "http://www.rstudio.com/shiny")),
						              br(),
                          #p(em("Nota: Como os dados de criminalidade de 2016 já estão disponíveis, os dados populacionais de 2016 são as estimativas de 2015 neste aplicativo para o cálculo das taxas. Assim que as estimativas populacionais de 2016 forem calculadas, elas serão atualizadas no VisualizaCrimeRS.")),						  
                          br(),
                          h2("Características do aplicativo"),
                          p("* Visualize séries temporais dos municípios e do Estado por número de ocorrências e taxas."),
                          p("* Relacione crimes em gráficos de dispersão e crie grupos de cidades."),
                          p("* Visualize os dados em mapas, de maneira interativa, e calcule autocorrelações espaciais dos crimes. Adicionalmente, verifique a evolução espaço temporal de presença/ausência de crimes através de Cadeias de Markov."),
                          p("* Obtenha a representação municipal no Estado de maneira rápida e intuitiva."),
                          p("* Realize pesquisas rápidas na base de dados e faça o ",em("download")," dos dados."),
                          br(),
                          h3("Contato para dúvidas, sugestões ou solicitações de código:"),
						              p("Renan Xavier Cortes ",
                            a("(GitHub)",
                              href = "https://github.com/renanxcortes", target="_blank")),
						  
						  br(),
						  br(),
						  div(img(href = "http://creativecommons.org/licenses/by/4.0/", src="https://i.creativecommons.org/l/by/4.0/88x31.png"), align = "center"),
						  div(p("Este obra está licenciada com uma Licença"), align = "center"),
						  div(a("Creative Commons Atribuição 4.0 Internacional",
                              href = "http://creativecommons.org/licenses/by/4.0/", target="_blank"), align = "center")
                      )
                      ))
}
