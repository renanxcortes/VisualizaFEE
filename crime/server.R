##########
# Server #
##########

# Cada linha liga o server de um módulo (R/mod_*.R) ao id usado na UI (ui.R).
shinyServer(function(input, output, session) {

  mapaServer("mapa")
  markovServer("markov")
  comparaCrimesServer("compara_crimes")
  comparaMunicipiosServer("compara_municipios")
  dispersaoServer("dispersao")
  treemapServer("treemap")
  dadosServer("dados")
  # "sobre" não tem server: é só texto.

})
