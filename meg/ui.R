##################
# User Interface #
##################

shinyUI(fluidPage(includeCSS("estilociclo.css"), 
                  htmlOutput("frame"), 
                  theme = shinytheme("cerulean"), 
                  tags$head(tags$script(src="tracking.js")), 
                  tags$head(tags$link(rel="shortcut icon", href="feeicon.ico")),
        tags$div(
                HTML("<script>
                     (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                     (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                     m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                     })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

                     ga('create', 'UA-1506603-5', 'auto');
                     ga('send', 'pageview');
                     </script>")),
					 
		    tags$head(HTML('<meta property="og:type" content="article">')),
				tags$head(HTML('<meta property="og:title" content="MEG - Monitor da Economia Gaúcha">')),
				tags$head(HTML('<meta property="og:url" content="http://visualiza.fee.tche.br/meg/">')),
				tags$head(HTML('<meta property="og:image" content="http://visualiza.fee.tche.br/_imgs/meg.jpg">')),
				tags$head(HTML('<meta property="og:description" content="O MEG apresenta, de maneira interativa e dinâmica, os principais dados conjunturais da economia do Rio Grande do Sul (RS). " />')),
				tags$head(HTML('<meta property="og:site_name" content="VisualizaFEE" />')),


				 
# Essa parte de baixo dá uma "empurradinha" pra direita o navbarheader pra aparecer o nome do app: https://stackoverflow.com/questions/9792849/how-to-insert-spaces-tabs-in-text-using-html-css
tags$style(type = 'text/css',

                           '.navbar-default .navbar-brand {
							 padding-left: 35px;
                           }'

                           ),


					 
tags$style(type="text/css", # isso é para não mostrar nenhuma mensagem vermelha de erro!!
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
	),  
 
  navbarPage("MEG", # Navegação
             tabPanel("Apresentação", #Apresentação
                      
                      sidebarLayout(
                        sidebarPanel(
                          h2("Fundação de Economia e Estatística"),
                          p("A Fundação de Economia e Estatística Siegfried Emanuel Heuser (FEE) é uma instituição de pesquisa, vinculada à Secretaria do Planejamento, Mobilidade e Desenvolvimento Regional do Governo do Estado do Rio Grande do Sul."),
                          br(),
                          br(),
                          img(src = "fee_logo.png", height = 32, width = 132), 
                          img(src = "logoGoverno.png", height = 62, width = 92),
                          br(),
                          br(),
                          br(),
                          "Shiny é um produto do ", 
                          span("RStudio", style = "color:blue"),".", 
                          br(), 
                          br(),
                          img(src = "bigorb.png", height = 82, width = 92),
						  br(),
						  br(),
						  div(h2("Vídeo Tutorial"), align = "center"),
						  div(HTML('<iframe height="315" src="https://www.youtube.com/embed/LKABa4rKo1c" frameborder="0" allowfullscreen></iframe>'), align = "center") # width="450"
                        ),
                        mainPanel(
                          h1("Monitor da Economia Gaúcha"),
                          p("O ", strong("Monitor da Economia Gaúcha (MEG)")," é um produto da Fundação de Economia e Estatística que apresenta, 
                            de maneira interativa e dinâmica, os principais dados conjunturais da economia do Rio Grande do Sul (RS). Os indicadores 
                            possuem origens em múltiplas fontes de dados e abarcam diversas dimensões, tais como Nível de Atividade, Mercado de Trabalho, Indicadores de Confiança, Crédito e Inadimplência e Índices de Preços."),			  
                          br(),
						  div(downloadButton("downloadMetodologia", "Download da Metodologia do MEG"), align = "center"),
						  br(),
						  br(),
              h2("Características do aplicativo"),
              p("* Visualize as séries temporais dos indicadores deste sistema de acordo com a sua dimensão na economia."),
              p("* Compare diferentes tipos de variação em uma tabela sintética com os indicadores."),
              p("* Analise a dinâmica macroeconômica das variáveis do sistema, através do comportamento temporal de indicadores categorizados como antecedentes, coincidentes ou defasados."),
						  p("* Faça o download de todas as bases de dados utilizadas."),
						  br(),
						  div(h2("Dimensões e Indicadores do MEG"), align = "center"),
                          div(collapsibleTreeOutput("collapsible_meg"), align = "center"),
                          br(),
                          h3("Contato para dúvidas, sugestões ou solicitações de código:"),
						  p("Jéfferson Augusto Colombo, Renan Xavier Cortes, Fernando Cruz ou Luis Henrique Zanandréa Paese ",
                            a("(CONTATO)", 
                              href = "http://www.fee.rs.gov.br/contato/", target="_blank")),
						  
						  br(),
						  br(),
						  div(img(href = "http://creativecommons.org/licenses/by/4.0/", src="https://i.creativecommons.org/l/by/4.0/88x31.png"), align = "center"),
						  div(p("Este obra está licenciada com uma Licença"), align = "center"),
						  div(a("Creative Commons Atribuição 4.0 Internacional",
                              href = "http://creativecommons.org/licenses/by/4.0/", target="_blank"), align = "center")
                          )
                      )),
					  
					  
	        tabPanel("Séries Temporais",
                      
            sidebarLayout(
            sidebarPanel(width = 3, # Numero de colunas
			
			       h3("Séries Temporais"),
              p("No gráfico ao lado é possível analisar a evolução temporal de todos os indicadores do monitor."),
						  br(),
						  p("Os indicadores estão agrupados por cinco dimensões: ", strong("Nível de Atividade, Mercado de Trabalho, Indicadores de Confiança, Inadimplência e Crédito e Índices de Preços"), "."), 
						  br(),
                          
			# Árvore Hierárquica: unique(select(base_transformada, NomeVar, Categoria)) %>% arrange(desc(Categoria), desc(NomeVar))							   
			selectInput('series_compara_hierarquia', 'Escolha o indicador:', 
			        choices = list(
						  `Nível de Atividade` = c(`IBCR-RS` = 'IBCR-RS',
						                            `Comércio - Vendas` = 'Comércio - Vendas',
						                            `Serviços` = 'Serviços', 
													`Indústria - Produção` = 'Indústria - Produção', 
													`Indústria - UCI` = 'Indústria - UCI',
													`Indústria - Desempenho` = 'Indústria - Desempenho', 
													`Indústria - Compras` = 'Indústria - Compras'
													),
						  `Mercado de Trabalho` = c(`Tempo de Desemprego` = 'Tempo de Desemprego', 
													`Massa de Rendimentos` = 'Massa de Rendimentos',
													`Taxa de Desemprego` = 'Taxa de Desemprego',
													`Emprego - Agronegócio` = 'Emprego - Agronegócio',
													`Emprego - Total` = 'Emprego - Total'),
						  `Indicadores de Confiança` =  c(`Indústria - Confiança` = 'Indústria - Confiança', 
														  `Mercado Imobiliário - Expectativas` = 'Mercado Imobiliário - Expectativas',
														  `Comércio - Intenção de Consumo` = 'Comércio Confiança'), 
						  `Inadimplência e Crédito` =  c(`Inadimplência` = 'Inadimplência', 
														  `Crédito` = 'Crédito'),
						  `Índices de preços` = c(`IPCA - Índice geral` = 'IPCA - Índice geral',
						                                     `IPCA - Alimentação e bebidas` = 'IPCA - Alimentação e bebidas',
															 `IPCA - Habitação` = 'IPCA - Habitação',
															 `IPCA - Artigos de residência` = 'IPCA - Artigos de residência',
															 `IPCA - Vestuário` = 'IPCA - Vestuário',
															 `IPCA - Transportes` = 'IPCA - Transportes',
															 `IPCA - Saúde e cuidados pessoais` = 'IPCA - Saúde e cuidados pessoais',
															 `IPCA - Despesas pessoais` = 'IPCA - Despesas pessoais',
															 `IPCA - Educação` = 'IPCA - Educação',
															 `IPCA - Comunicação` = 'IPCA - Comunicação',
															 `Preço Imóveis Residenciais - Venda` = 'Preço Imóveis Residenciais - Venda',
															 `Preço Imóveis Residenciais - Locação` = 'Preço Imóveis Residenciais - Locação',
															 `Rental yield - Imóveis Residenciais` = 'Rental yield (%) - Imóveis Residenciais',
															 `Preço Imóveis Comerciais - Venda` = 'Preço Imóveis Comerciais - Venda',
															 `Preço Imóveis Comerciais - Locação` = 'Preço Imóveis Comerciais - Locação',
															 `Rental yield - Imóveis Comerciais` = 'Rental yield (%) - Imóveis Comerciais')
														  
						), selected = "IBCR-RS", selectize = FALSE),
										   
			checkboxInput('check_series_sazonal', "Series Temporais com Ajuste Sazonal", value=F),
            
      checkboxInput('check_recessoes', "Visualizar períodos recessivos nacionais?", value=F),
			
			conditionalPanel(condition = 'input.check_recessoes',
			div(bsButton("info_recessao", label = "Informações", icon = icon("info"), style = "info", size = "medium"), align = "center")),
						bsPopover(id = "info_recessao", title = "Informações das Recessões",
                                                 content = paste0("Os períodos recessivos nacionais referem-se às datações do " ,a("CODACE - FGV/IBRE", 
                                                       href = "http://portalibre.fgv.br/main.jsp?lumChannelId=4028808126B9BC4C0126BEA1755C6C93",
                                                       target="_blank"), ". Os períodos representados pela cor cinza escura são já finalizados de entrada e de saída de uma recessão. O cinza mais claro representa um período de entrada de uma recessão que ainda não teve a sua datação final definida pelo CODACE."),
                                                 placement = "right", 
                                                 trigger = "focus", # O 'click' só fechava quando clicava denovo 
                                                 options = list(container = "body")
                                       )),
                        
            # Construindo o gráfico principal
            mainPanel(width = 9, # Complementar ao Sidebar

            # Algumas séries não tem ajuste sazonal, então não aparecerá e dará um aviso
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Mercado Imobiliário - Expectativas"', p("Devido ao tamanho reduzido da série, ela não apresenta decomposição de ajuste sazonal.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Preço Imóveis Residenciais - Locação"', p("Devido ao tamanho reduzido da série, ela não apresenta decomposição de ajuste sazonal.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Rental yield (%) - Imóveis Residenciais"', p("Devido ao tamanho reduzido da série, ela não apresenta decomposição de ajuste sazonal.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Preço Imóveis Comerciais - Venda"', p("Devido ao tamanho reduzido da série, ela não apresenta decomposição de ajuste sazonal.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Preço Imóveis Comerciais - Locação"', p("Devido ao tamanho reduzido da série, ela não apresenta decomposição de ajuste sazonal.")),
						conditionalPanel(condition = 'input.check_series_sazonal && input.series_compara_hierarquia == "Rental yield (%) - Imóveis Comerciais"', p("Devido ao tamanho reduzido da série, ela não apresenta decomposição de ajuste sazonal.")),
            
						div(plotlyOutput("plot_series", height = "500px", width = "85%"), align = "center"), # height = "600px", width = "1000px"
						div(textOutput('fonte_series'), align = "center"),
						br(),
						div(textOutput('explicacao_series'), align = "center")
                      )
                      )
			                ),
					  
			tabPanel("Tabela Sintética de Variações",
                      
      sidebarLayout(
			
			sidebarPanel(width = 3,
                    h3("Tabela Sintética de Variações"),
                    p("A tabela ao lado apresenta os indicadores do MEG de uma maneira sintética dando destaque para as variações."),
                    br(),
                    div(bsButton("info_tabela", 
                                 label = "Informações", 
                                 icon = icon("info"), 
                                 style = "info", 
                                 size = "medium"), align = "center"),
			             
                    bsPopover(id = "info_tabela", 
                              title = "Informações da Tabela",
                              content = paste0("Ela apresenta o valor bruto do indicador sem ajuste sazonal e suas  variações mensais em 12 meses, bem como acumuladas em 12 meses. A tabela apresenta também variações da série com ajuste sazonal sendo mês contra mês anterior e variação da janela móvel de 3 meses. As setas ilustram se o indicador está aumentando ou decaindo, enquanto que as cores indicam se o desempenho é bom (verde) ou ruim (vermelho). As variações das séries que são medidas em percentual estão em pontos percentuais absolutos."),
                              placement = "right", 
                              trigger = "focus", # O 'click' só fechava quando clicava denovo 
                    options = list(container = "body")),
			             
                    br(),		   
									   
						  h4("Filtrar Tabela"),
						  checkboxInput('check_grupo_tabela', 
						                "Visualizar somente um grupo de indicadores?", 
						                value = F),
						  
						  conditionalPanel(condition = 'input.check_grupo_tabela',
						  selectInput('grupo_tabela', 'Selecione o grupo:', 
                                      as.character(unique(base_transformada$Categoria)),
                                      selected = "Nível de Atividade"))),
			
              mainPanel(width = 9, # Complementar do width do sidebar
                          div(sliderInput("data_jogo",
                          "Escolha o mês e ano da Tabela de Variações:",
  						min = min(aux_datas$Data) + years(2), # Adiciona dois anos, pois não tem como fazer o monitor nos primeiros dois anos
  						max = max(aux_datas$Data),
                          value = max(aux_datas$Data),
  						timeFormat = "%m-%Y",
  						width = "90%",
  						step = 31, # Número de dias em cada variação do slider
  						animate = animationOptions(interval = 2500, loop = FALSE)), align = "center"),
					
						br(),
						div(h3(textOutput("titulo_tabela")), align = "center"),
						br(),
						column(12, formattableOutput("tabela_brazuca")),
						p("* Sem Ajuste Sazonal"),
						p("** Com Ajuste Sazonal"),
						p("NA - Não se aplica")))),
					  
            tabPanel("Dinâmica Macroeconômica",
                      
						sidebarLayout(
            sidebarPanel(width = 3, # Numero de colunas
						
						h4("Propriedades do Gráfico"),
						
										   
					  checkboxInput('check_monitor_eixos_fixos', "Eixos fixos ao longo do tempo", value=F),
						checkboxInput('check_incluir_PIB_monitor', "Incluir PIB no monitor", value=F),
            br(),						
						p(h5("Categorias dos indicadores do monitor:")),
						checkboxInput('check_antecedente', "Antecedente", value=T),
						checkboxInput('check_coincidente', "Coincidente", value=T),
						checkboxInput('check_defasada', "Defasada", value=T),

						p("OBS.: Se o período escolhido não tiver dados, o gráfico não aparecerá.", style = "color:gray", align = "left"),
										 
						div(bsButton("info_monitor", 
						             label = "Informações", 
						             icon = icon("info"), 
						             style = "info", 
						             size = "medium"), align = "center"),
						
						bsPopover(id = "info_monitor", 
						          title = "Informações do Monitor",
                      content = paste0("As cores do monitor representam o estágio do indicador em um dado momento da economia. A cor verde, do quadrante superior direito, significa que o indicador está tendo um desempenho ", 
												 span("bom", style = "color:green"), " enquanto que a cor vermelha, do quadrante inferior esquerdo, signfica que o indicador está tendo um desempenho ", span("ruim", style = "color:red"), 
												 ". Além disso, as séries que se movimentam no sentido oposto ao PIB (contracíclicas) como Taxa de Desemprego, Tempo de Desemprego e Inadimplência foram invertidas para manter a consistência de interpretação das cores do monitor."),
                       placement = "right", 
                       trigger = "focus", # O 'click' só fechava quando clicava denovo 
                       options = list(container = "body")
                                       ),
						
						br(),
						br()
            ),
										 
				 
						mainPanel(width = 9,
						conditionalPanel(condition = '!input.check_destaque_indicadores',
						div(plotlyOutput("plot_monitor", height = "473px", width = "750px"), align = "center")), # 
						
						conditionalPanel(condition = 'input.check_destaque_indicadores',
						div(plotlyOutput("plot_monitor_cores_no_scatter", height = "600px", width = "950px"), align = "center")),
						
						br(),
						br(),
						
						div(sliderInput("data_slider_monitor",
                        "Escolha o mês e ano do gráfico:",
						min = min(aux_datas$Data) + years(2), # Adiciona dois anos, pois não tem como fazer o monitor nos primeiros dois anos
						max = max(aux_datas$Data),
                        value = max(aux_datas$Data),
						timeFormat="%m-%Y",
						width = "90%",
						step = 30.5, # Número de dias em cada variação do slider
						animate = animationOptions(interval = 2500, loop = FALSE)), align = "center"),
						span("Crescimento: representa a taxa de crescimento do indicador no acumulado de 12 meses.", style = "color:gray"),
						br(),
						span("Aceleração: representa a variação absoluta mensal do crescimento.", style = "color:gray")
						
						)
            )
						),
			
			navbarMenu(
			  title = "Download dos Dados",
			  tabPanel("Sem Ajuste Sazonal",
			           
			           sidebarLayout(
			             sidebarPanel(
			               h3("Tabela de Dados"),
			               p("Pesquise as informações de seu interesse ao lado."),
			               br(),
			               p(
			                 "Se você deseja fazer o ",
			                 strong("download"),
			                 "da base completa, clique no ícone abaixo."
			               ),
			               downloadButton('downloadData_sem', 'Baixar .csv')
			             ),
			             
			             # Mostrar a tabela
			             mainPanel(dataTableOutput("tabela_download_sem_ajuste"))
			           )),
			  
			  tabPanel("Com Ajuste Sazonal",
			           
			           sidebarLayout(
			             sidebarPanel(
			               h3("Tabela de Dados"),
			               p("Pesquise as informações de seu interesse ao lado."),
			               br(),
			               p(
			                 "Se você deseja fazer o ",
			                 strong("download"),
			                 "da base completa, clique no ícone abaixo."
			               ),
			               downloadButton('downloadData_com', 'Baixar .csv')
			             ),
			             
			             # Mostrar a tabela
			             mainPanel(dataTableOutput("tabela_download_com_ajuste"))
			           ))
			  
			)
					  
)
)
)

