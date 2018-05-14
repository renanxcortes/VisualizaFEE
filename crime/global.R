###########
# Pacotes #
###########
library(shiny)
library(plotly)
library(leaflet)
library(D3plusR) # devtools::install_github('paulofelipe/D3plusR')
library(DT)      # Data Tables
library(C3)      # devtools::install_github("FrissAnalytics/shinyJsTutorials/widgets/C3")
library(spdep)
library(flexdashboard) # install.packages("flexdashboard", type = "source")
library(tidyverse)
library(stringi)
library(shinyBS) # Pelos botoes de popover e tooltip
library(shinythemes)
###########
###########
###########

base_crime <- readRDS("base_crimevis_2016_pop_ok.rds") # Tibble

# Conversão de strings para Shiny server
#base_crime$Mun <- stri_conv(as.character(base_crime$Mun), "latin1", "UTF-8")
#base_crime$Crime <- stri_conv(as.character(base_crime$Crime), "latin1", "UTF-8")

mapa_rs <- readRDS("MapaRS.rds")

# Conversão de strings para Shiny server
#mapa_rs@data$Nome_Munic <- stri_conv(as.character(mapa_rs@data$Nome_Munic), "latin1", "UTF-8")

# CodIBGE dos Municípios da RMPA
cods_rmpa <- c(4300604,4300877,4301107,4303103,4303905,4304606,4304689,4305355,4306403,4306767,4307609,4307708,4309050,4309209,4309308,4310108,4310801,4312401,4313060,4313375,4313409,4314050,4314803,4314902,4316006,4317608,4318408,4318705,4319505,4319901,4320008,4321204,4322004,4323002)

options(shiny.sanitize.errors = FALSE)