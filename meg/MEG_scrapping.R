###########
# PACOTES #
###########
library(data.table)
library(tidyverse)
library(sidrar)
library(XML)
library(RCurl)
library(rbcb) #devtools::install_github('wilsonfreitas/rbcb') # Baita pacote para minerar dados do Banco Central
library(readxl)
library(gdata) # read.xls
library(stringr)
library(zoo)
library(seasonal)
###########
###########
###########


pim_rs <- get_sidra(3653,
                    variable = c(3135,3134), # Índice de base fixa sem ajuste sazonal (Base: média de 2012 = 100) (Número-índice) & Índice de base fixa com ajuste sazonal (Base: média de 2012 = 100) (Número-índice)
                    period = "all",
                    classific = "c544", # No info_sidra(3653, wb = TRUE) está presente o código $classific_category$`c544 = Seções e atividades industriais (CNAE 2.0) (28):`
                    category = list(129314), # "Seções e atividades industriais (CNAE 2.0)"
                    geo = "State",
                    geo.filter = list("State" = 43))

# Varejista ampliado
pmc_rs <- get_sidra(3417,
                    variable = 1186, # Índice de volume de vendas no comércio varejista (unidade de medida: vide classificação 'Tipos de índice')
                    period = "all",
                    classific = c("c11046"), # $classific_category$`c11046 = Tipos de índice (6):`
                    category = list(c(40311)), # NÃO TEM ajuste!!!
                    geo = "State",
                    geo.filter = list("State" = 43))

pms_rs <- get_sidra(6442,
                    variable = 8677, #  Índice de volume de serviços (unidade de medida: vide classificação 'Tipos de índice')
                    period = "all",
                    classific = c("c11046"), # $classific_category$`c11046 = Tipos de índice (6):`
                    category = list(c(40311, 40312)), 
                    geo = "State",
                    geo.filter = list("State" = 43))

ipca_rs <- get_sidra(1419,
                    variable = 63, #  IPCA - Variação mensal (Percentual) - casas decimais: padrão = 2, máximo = 2
                    period = "all",
                    classific = c("c315"), # Geral, grupo, subgrupo, item e subitem
                    category = list(c(7169, 7170, 7445, 7486, 7558, 7625, 7660, 7712, 7766, 7786)), 
                    geo = "MetroRegion",
                    geo.filter = list("State" = 43))

ipca_sa_rs <- get_sidra(1420,
                     variable = 306, # IPCA dessazonalizado - Variação mensal (Percentual) - casas decimais: padrão = 2, máximo = 2
                     period = "all",
                     classific = c("c315"), # Geral, grupo, subgrupo, item e subitem
                     category = list(c(7169, 7170, 7445, 7486, 7558, 7625, 7660, 7712, 7766, 7786)), 
                     geo = "MetroRegion",
                     geo.filter = list("State" = 43))



ibcr_rs <- rbcb::get_series(25401)

ibcr_sa_rs <- rbcb::get_series(25404)



####################
### DADOS DA PED ###
####################
url_ped <- getURL("https://www.fee.rs.gov.br/publicacoes/ped-rmpa/serie-historica-mensal/")
doc <- htmlTreeParse(url_ped, useInternal=T)
aux1<-xpathSApply(doc, "//a[@href]", xmlGetAttr, "href") #Coleta todas as urls da página


# Encontrar a url da tabela que tem padrão "tabela-ped-mensal-sh-04"
# Coletando os links
link_taxa_des <- aux1[which(grepl("tabela-ped-mensal-sh-02", aux1))] #Encontrar a url da tabela que tem padrão "tabela-ped-mensal-sh-04"
link_tempo_des <- aux1[which(grepl("tabela-ped-mensal-sh-04", aux1))] #Encontrar a url da tabela que tem padrão "tabela-ped-mensal-sh-04"
link_massa_ren <- aux1[which(grepl("tabela-ped-mensal-sh-19", aux1))] #Encontrar a url da tabela que tem padrão "tabela-ped-mensal-sh-19"


# Lendo os arquivos em excel
library(readxl)
temp = tempfile(fileext = ".xlsx")
res1 <- link_taxa_des
download.file(res1, destfile=temp, mode = 'wb')
taxa_des <- readxl::read_excel(temp, 
                               sheet ="tabela-ped-mensal-sh-02",
                               col_names = T)

temp = tempfile(fileext = ".xlsx")
res2 <- link_tempo_des
download.file(res2, destfile=temp, mode = 'wb')
tempo_des <- readxl::read_excel(temp, 
                                sheet ="tabela-ped-mensal-sh-04",
                                col_names = T)

temp = tempfile(fileext = ".xlsx")
res3 <- link_massa_ren
download.file(res3, destfile=temp, mode = 'wb')
massa_ren <- readxl::read_excel(temp, 
                                sheet ="tabela-ped-mensal-sh-19",
                                col_names = T)

##################################################################################################

# COLETANDO OS DADOS DAS TABELAS


####Taxa de desemprego####


#Pegando os dados de taxa de desemprego total
taxa_des <- taxa_des[10:nrow(taxa_des),c(1,2)]

#Encontrando a linha em que a primeira coluna é igual a "∆% mensal". Meu ponto de "corte" é a observação anterior.
corte1 <- which(taxa_des[,1]=="∆% mensal") - 1

#Deletando os NA's
taxa_des <-data.frame(na.omit(taxa_des[1:corte1,]))

taxa_des.valores <-c(as.numeric(taxa_des[,2]))

#Criando a coluna de meses
meses<-seq(from=as.Date("01-06-1992",format="%d-%m-%Y"),
           by="month",
           length.out=length(taxa_des.valores))

#Dados finais
taxa_des_pre <- cbind.data.frame(meses,taxa_des.valores)


####Tempo de desemprego###


tempo_des<-tempo_des[10:nrow(tempo_des),c(2:14)]

#Encontrando a linha do primeiro NA
prim.NA<-which(is.na(tempo_des[,1]))[1]

#Cortando o dataframe até o último ano em que não é NA
anos<-seq(from=1992,by=1,length.out=prim.NA-1)


tempo_des.valores <- as.numeric(c(t(tempo_des[1:(prim.NA-1),-1])))
meses<-seq(from=as.Date("01-01-1992",format="%d-%m-%Y"),
           by="month",
           length.out=length(tempo_des.valores))

tempo_des_pre <- cbind.data.frame(meses,tempo_des.valores)



####Massa de Rendimentos####

#Pegando os dados de taxa de desemprego total
massa_ren <- massa_ren[10:nrow(massa_ren),c(1,6)]

#Encontrando a linha em que a primeira coluna é igual a "∆% mensal". Meu ponto de "corte" é a observação anterior.
corte2 <- which(massa_ren[,1]=="∆% mensal") - 1

#Deletando os NA's
massa_ren <-data.frame(na.omit(massa_ren[1:corte2,]))

massa_ren.valores <-c(as.numeric(massa_ren[,2]))

#Criando a coluna de meses
meses<-seq(from=as.Date("01-06-1992",format="%d-%m-%Y"),
           by="month",
           length.out=length(massa_ren.valores))

#Dados finais
massa_ren_pre <- cbind.data.frame(meses,massa_ren.valores)

############################
### FIM DOS DADOS DA PED ###
############################







credito_rs_pre <- rbcb::get_series(14076)

inadimplencia_rs <- rbcb::get_series(15945)

igp_di_rs <- rbcb::get_series(190) %>% filter(date > credito_rs_pre$date[1])

transforma_indice <- function(x) {y <- rep(0,length(x)); y[1] <- 100; for(i in 2:length(x)) y[i] <- y[i-1]*(100+x[i])/100; return(y)}

fatores <- rev(transforma_indice(igp_di_rs$`190`) / 100)

credito_rs <- credito_rs_pre %>% mutate(`14076` = `14076` * fatores)


fiergs <-  read.xls("http://www.fiergs.org.br/sites/default/files/series/indicadores_industriais_do_rs.xlsx", perl = "C:/Perl64/bin/perl.exe", skip = 5)
#fiergs <-  read.xls("http://www.fiergs.org.br/sites/default/files/series/indicadores_industriais_do_rs_0.xlsx", perl = "C:/Perl64/bin/perl.exe", skip = 5)

fiergs_icei <- read.xls("http://www.fiergs.org.br/sites/default/files/series/indice_de_confianca_do_empresario_industrial_do_rs.xlsx", perl = "C:/Perl64/bin/perl.exe", skip = 2)
#fiergs_icei <- read.xls("http://www.fiergs.org.br/sites/default/files/series/indice_de_confianca_do_empresario_industrial_do_rs_0.xlsx", perl = "C:/Perl64/bin/perl.exe", skip = 2)



### INÍCIO DO AGRONEGÓCIO ###
url_agro <- getURL("https://www.fee.rs.gov.br/indicadores/agronegocio/emprego-formal-celetista/serie-historica/")
doc <- htmlTreeParse(url_agro, useInternal=T)
aux1<-xpathSApply(doc, "//a[@href]", xmlGetAttr, "href") # Coleta todas as urls da página


# Encontrar a url da tabela que tem padr?o "tabela-ped-mensal-sh-04"
# Coletando os links
link_agronegocio <- aux1[which(grepl("tabela-agronegocio-sh-05", aux1))] #Encontrar a url da tabela que tem padr?o "tabela-ped-mensal-sh-04"


# Lendo os arquivos em excel
library(readxl)
temp = tempfile(fileext = ".xlsx")
res1 <- link_agronegocio
download.file(res1, destfile=temp, mode='wb')
agronegocio <- readxl::read_excel(temp, 
                                  sheet ="5. Estoque Mensal")

agronegocio.valores <- as.numeric(agronegocio[which(agronegocio[,1]=="TOTAL"),-1])

meses <- seq(from=as.Date("01-01-2007",format="%d-%m-%Y"),
             by="month",
             length.out=length(agronegocio.valores))

agronegocio_pre <- cbind.data.frame(meses,agronegocio.valores)
### FIM DO AGRONEGÓCIO ###


# Data Cleaning and Wrangling

# Função Auxiliares #
retorna_quarter <-   Vectorize(function(x) {switch(as.character(x),"NA" = NA, "1" = 1,"2" = 1,"3" = 1,"4" = 2,"5" = 2,"6" = 2,"7" = 3,"8" = 3,"9" = 3,"10" = 4,"11" = 4,"12" = 4)})
extrai_mes_short <-  Vectorize(function(x) {switch(x,"1" = "jan","2" = "fev","3" = "mar","4" = "abr","5" = "mai","6" = "jun","7" = "jul","8" = "ago","9" = "set","10" = "out","11" = "nov","12" = "dez")})
mes_ingles_p_numero <- Vectorize(function(x) {switch(x,"Jan" = 1,"Feb" = 2,"Mar" = 3,"Apr" = 4,"May" = 5,"Jun" = 6,"Jul" = 7,"Aug" = 8,"Sep" = 9,"Oct" = 10,"Nov" = 11,"Dec" = 12)})
mes_portu_para_ingles <- Vectorize(function(x) {switch(x,"jan" = "Jan","fev" = "Feb","mar" = "Mar","abr" = "Apr","mai" = "May","jun" = "Jun","jul" = "Jul","ago" = "Aug","set" = "Sep","out" = "Oct","nov" = "Nov","dez" = "Dec")})
transforma_YY_para_YYYY <- Vectorize(function(x) {switch(x, "91" = 1991, "92" = 1992, "93" = 1993, "94" = 1994, "95" = 1995, "96" = 1996, "97" = 1997, "98" = 1998, "99" = 1999, "00" = 2000, "01" = 2001, "02" = 2002, "03" = 2003, "04" = 2004, "05" = 2005, "06" = 2006, "07" = 2007, "08" = 2008, "09" = 2009, "10" = 2010, "11" = 2011, "12" = 2012, "13" = 2013, "14" = 2014, "15" = 2015, "16" = 2016, "17" = 2017, "18" = 2018, "19" = 2019, "20" = 2020, "21" = 2021)})
transforma_YYYYMM <- function(x)  {x %>% 
                                   separate(Data_Original, into = c("year", "month"), sep = 4) %>%   
                                   mutate(year = as.integer(year), 
                                          month = as.integer(month), 
                                          quarter = as.integer(retorna_quarter(month)),
                                          date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3))))}

trata_fiergs <- function(x) {x %>% separate(X, into = c("Mes","Ano"), sep = "-") %>%
                                   mutate(month = mes_ingles_p_numero(Mes), 
                                          quarter = as.integer(retorna_quarter(month)), 
                                          year = transforma_YY_para_YYYY(Ano),
                                          date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
                                   dplyr::select(-Mes, -Ano)}

trata_fipe <- function(x) {x %>% mutate(date = lubridate::ymd(date),
                                  year = year(date), 
                                  month = month(date), 
                                  quarter = as.integer(retorna_quarter(month)),
                                  date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
                                  ordena_colunas()}


refcols <- c("date", "month", "quarter", "year")
ordena_colunas <- function(x) {return(x[,c(refcols, setdiff(names(x), refcols))])}





  
pim_pre <- pim_rs %>%
       dplyr::select(`Mês (Código)`, Variável, Valor) %>%
       spread(Variável, Valor) %>%
       rename(Com = `Índice de base fixa com ajuste sazonal (Base: média de 2012 = 100)`,
              Sem = `Índice de base fixa sem ajuste sazonal (Base: média de 2012 = 100)`,
              Data_Original = `Mês (Código)`) %>%
       transforma_YYYYMM()




pmc_pre <- pmc_rs %>%
           dplyr::select(`Mês (Código)`, `Tipos de índice`, Valor) %>%
           spread(`Tipos de índice`, Valor) %>%
           rename(#Com = `Índice base fixa com ajuste sazonal (2014=100)`, 
                  Sem = `Índice base fixa (2014=100)`, 
                  Data_Original = `Mês (Código)`) %>%
           transforma_YYYYMM()



pms_pre <- pms_rs %>%
           dplyr::select(`Mês (Código)`, `Tipos de índice`, Valor) %>%
           spread(`Tipos de índice`, Valor) %>%
           rename(Com = `Índice base fixa com ajuste sazonal (2014=100)`,
                  Sem = `Índice base fixa (2014=100)`,
                  Data_Original = `Mês (Código)`) %>%
           transforma_YYYYMM()






ipca_pre <- ipca_rs %>%
            dplyr::select(`Mês (Código)`, `Geral, grupo, subgrupo, item e subitem`, Valor) %>%
            spread(`Geral, grupo, subgrupo, item e subitem`, Valor) %>%
            mutate_at(.vars = vars(`1.Alimentação e bebidas`:`Índice geral`), .funs = funs(transforma_indice(.))) %>%
            rename(Data_Original = `Mês (Código)`) %>%
            transforma_YYYYMM()



ipca_sa_pre <- ipca_sa_rs %>%
                dplyr::select(`Mês (Código)`, `Geral, grupo, subgrupo, item e subitem`, Valor) %>%
                spread(`Geral, grupo, subgrupo, item e subitem`, Valor) %>%
                mutate_at(.vars = vars(`1.Alimentação e bebidas`:`Índice geral`), .funs = funs(transforma_indice(.))) %>%
                rename(Data_Original = `Mês (Código)`) %>%
                transforma_YYYYMM()



       

uci_pre    <- fiergs %>% 
              dplyr::select(X, Sem.ajuste.sazonal.2) %>%
              rename(Sem = Sem.ajuste.sazonal.2) %>%
              trata_fiergs()

  
uci_sa_pre <- fiergs %>% 
          dplyr::select(X, Com.ajuste.sazonal.2) %>%
          rename(Com = Com.ajuste.sazonal.2) %>%
          trata_fiergs()

compras_industriais_pre <- fiergs %>% 
                           dplyr::select(X, Sem.ajuste.sazonal.5) %>%
                           rename(Sem = Sem.ajuste.sazonal.5) %>%
                           trata_fiergs()


compras_industriais_sa_pre <- fiergs %>% 
                              dplyr::select(X, Com.ajuste.sazonal.5) %>%
                              rename(Com = Com.ajuste.sazonal.5) %>%
                              trata_fiergs()

idi_pre <-    fiergs %>% 
              dplyr::select(X, Sem.ajuste.sazonal.7) %>%
              rename(Sem = Sem.ajuste.sazonal.7) %>%
              trata_fiergs()
         
idi_sa_pre <- fiergs %>% 
              dplyr::select(X, Com.ajuste.sazonal.7) %>%
              rename(Com = Com.ajuste.sazonal.7) %>%
              trata_fiergs()

icei <- fiergs_icei %>% 
        filter(X == "ICEI/RS") %>% 
        dplyr::select(-X) %>% 
        gather(Data, Valor)
icei <- icei[-c(1:19),] # Deixa de ser trimestral a partir de 2010!
icei$Valor <- as.numeric(icei$Valor)

icei_pre   <- icei %>% 
              separate(Data, into = c("Mes","Ano")) %>%
              mutate(month = mes_ingles_p_numero(Mes), 
                     quarter = as.integer(retorna_quarter(month)), 
                     year = transforma_YY_para_YYYY(Ano),
                     date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
              dplyr::select(-Mes, -Ano)



### INÍCIO DA COLETA DO FIPEZAP ###

url_fipe <- getURL("http://www.fipe.org.br/pt-br/indices/fipezap/#indice-mensal")
doc <- htmlTreeParse(url_fipe, useInternal=T)
aux1<-xpathSApply(doc, "//a[@href]", xmlGetAttr, "href") #Coleta todas as urls da página

#Coletando os links
#link_fipezap_residencial<- aux1[which(grepl("fipezap-consolidado", aux1))] # procura a url que contém "fipezap-consolidado" no link 
link_fipezap_residencial<- aux1[which(grepl("fipezap-residencial", aux1))] # procura a url que contém "fipezap-consolidado" no link 

#link_fipezap_comercial <- aux1[which(grepl("fipezap-comercial", aux1))] # procura a url que contém "fipezap-comercial-consolidado" no link 
link_fipezap_comercial <- aux1[which(grepl("comercial", aux1))] # fipezap-201711-comercial


#Lendo os arquivos em excel
library(readxl)
temp = tempfile(fileext = ".xlsx")
res1 <- link_fipezap_residencial
download.file(res1, destfile=temp, mode='wb')
preco_residencial_venda <- readxl::read_excel(temp, 
                                              #sheet ="Venda (Número Índice)",
                                              sheet ="Venda (R$ por m2)",
                                              col_names=T)
preco_residencial_locacao <- readxl::read_excel(temp, 
                                                #sheet = "Locação (Número-índice)",
                                                sheet = "Locação (R$ por m2)",
                                                col_names=T)
rental_residencial <- readxl::read_excel(temp, 
                                         sheet ="Rental Yield (Rentab. aluguel)",
                                         col_names=T)

temp = tempfile(fileext = ".xlsx")
res2 <- link_fipezap_comercial
download.file(res2, destfile=temp, mode='wb')
preco_comercial_venda <- readxl::read_excel(temp, 
                                            sheet ="Venda (Número-índice)",
                                            col_names=T)
preco_comercial_locacao <- readxl::read_excel(temp, 
                                              sheet ="Locação (Número-índice)",
                                              col_names=T)
rental_comercial <- readxl::read_excel(temp, 
                                       sheet ="Rental Yield (Rentab. aluguel)",
                                       col_names=T)

# Coletando so dados de porto alegre
preco_residencial_venda <- preco_residencial_venda[3:nrow(preco_residencial_venda),c("X__1","Porto Alegre")] %>% na.omit() %>% mutate(`Porto Alegre` = as.numeric(`Porto Alegre`)) %>% filter(`Porto Alegre` > 0)
preco_residencial_locacao <- preco_residencial_locacao[3:nrow(preco_residencial_locacao),c("X__1","Porto Alegre")] %>% na.omit() %>% mutate(`Porto Alegre` = as.numeric(`Porto Alegre`)) %>% filter(`Porto Alegre` > 0)
rental_residencial <- rental_residencial[3:nrow(rental_residencial),c("X__1","Porto Alegre")] %>% na.omit() %>% mutate(`Porto Alegre` = 100 * as.numeric(`Porto Alegre`)) %>% filter(`Porto Alegre` > 0)


preco_comercial_venda <- preco_comercial_venda[3:nrow(preco_comercial_venda),c("X__1","Porto Alegre")] %>% na.omit() %>% mutate(`Porto Alegre` = as.numeric(`Porto Alegre`)) %>% filter(`Porto Alegre` > 0)
preco_comercial_locacao <- preco_comercial_locacao[3:nrow(preco_comercial_locacao),c("X__1","Porto Alegre")] %>% na.omit() %>% mutate(`Porto Alegre` = as.numeric(`Porto Alegre`)) %>% filter(`Porto Alegre` > 0)
rental_comercial <- rental_comercial[3:nrow(rental_comercial),c("X__1","Porto Alegre")] %>% na.omit() %>% mutate(`Porto Alegre` = 100 * as.numeric(`Porto Alegre`)) %>% filter(`Porto Alegre` > 0)

### FINAL DA FIPEZAP ###






# Inclui os códigos do MEG

pim     <- dplyr::select(ordena_colunas(pim_pre), -Com) %>% rename(s_1001 = Sem)
pmc     <-        ordena_colunas(pmc_pre)               %>% rename(s_1002 = Sem)
pms     <- dplyr::select(ordena_colunas(pms_pre), -Com) %>% rename(s_1003 = Sem)
ibcr <- ibcr_rs %>%
        mutate(year = year(date), 
               month = month(date), 
               quarter = as.integer(retorna_quarter(month)),
               date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
        rename(s_1004 = `25401`) %>%
        ordena_colunas()



taxa_des <- taxa_des_pre %>%
            mutate(year = year(meses), 
                   month = month(meses), 
                   quarter = as.integer(retorna_quarter(month)),
                   date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
            rename(s_1005 = taxa_des.valores) %>%
            dplyr::select(-meses) %>%
            ordena_colunas()


tempo_des <- tempo_des_pre %>%
             mutate(year = year(meses), 
                    month = month(meses), 
                    quarter = as.integer(retorna_quarter(month)),
                    date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
             rename(s_1006 = tempo_des.valores) %>%
             dplyr::select(-meses) %>%
             ordena_colunas()


massa_ren <- massa_ren_pre %>%
             mutate(year = year(meses), 
                    month = month(meses), 
                    quarter = as.integer(retorna_quarter(month)),
                    date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
             rename(s_1007 = massa_ren.valores) %>%
             dplyr::select(-meses) %>%
             ordena_colunas()



credito <- credito_rs %>%
            mutate(year = year(date), 
                   month = month(date), 
                   quarter = as.integer(retorna_quarter(month)),
                   date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
            rename(s_1015 = `14076`) %>%
            ordena_colunas()


inadimplencia <- inadimplencia_rs %>%
                 mutate(year = year(date), 
                        month = month(date), 
                        quarter = as.integer(retorna_quarter(month)),
                        date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
                 rename(s_1016 = `15945`) %>%
                 ordena_colunas()


agronegocio <-  agronegocio_pre %>%
                mutate(year = year(meses), 
                       month = month(meses), 
                       quarter = as.integer(retorna_quarter(month)),
                       date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
                rename(s_1020 = agronegocio.valores) %>%
                dplyr::select(-meses) %>%
                ordena_colunas()



ipca    <- ordena_colunas(ipca_pre) %>% rename(s_1028 = `1.Alimentação e bebidas`,
                                               s_1029 = `2.Habitação`,
                                               s_1030 = `3.Artigos de residência`,
                                               s_1031 = `4.Vestuário`,
                                               s_1032 = `5.Transportes`,
                                               s_1033 = `6.Saúde e cuidados pessoais`,
                                               s_1034 = `7.Despesas pessoais`,
                                               s_1035 = `8.Educação`,
                                               s_1036 = `9.Comunicação`,
                                               s_1037 = `Índice geral`)
uci <- ordena_colunas(uci_pre) %>% rename(s_1009 = Sem)
compras_industriais <- ordena_colunas(compras_industriais_pre) %>% rename(s_1010 = Sem)
idi     <- ordena_colunas(idi_pre) %>% rename(s_1011 = Sem)
icei <- ordena_colunas(icei_pre) %>% rename(s_1008 = Valor)






pim_sa     <- dplyr::select(ordena_colunas(pim_pre), -Sem) %>% rename(s_1001 = Com)
#pmc_sa     <- select(ordena_colunas(pmc_pre), -Sem) %>% rename(s_1002 = Com)
pms_sa     <- dplyr::select(ordena_colunas(pms_pre), -Sem) %>% rename(s_1003 = Com)
ibcr_sa <- ibcr_sa_rs %>%
            mutate(year = year(date), 
                   month = month(date), 
                   quarter = as.integer(retorna_quarter(month)),
                   date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
            rename(s_1004 = `25404`) %>%
            ordena_colunas()




ipca_sa <- ordena_colunas(ipca_sa_pre) %>% rename(s_1028 = `1.Alimentação e bebidas`,
                                                  s_1029 = `2.Habitação`,
                                                  s_1030 = `3.Artigos de residência`,
                                                  s_1031 = `4.Vestuário`,
                                                  s_1032 = `5.Transportes`,
                                                  s_1033 = `6.Saúde e cuidados pessoais`,
                                                  s_1034 = `7.Despesas pessoais`,
                                                  s_1035 = `8.Educação`,
                                                  s_1036 = `9.Comunicação`,
                                                  s_1037 = `Índice geral`)
uci_sa <- ordena_colunas(uci_sa_pre) %>% rename(s_1009 = Com)
compras_industriais_sa <- ordena_colunas(compras_industriais_sa_pre) %>% rename(s_1010 = Com)
idi_sa     <- ordena_colunas(idi_sa_pre) %>% rename(s_1011 = Com)




pr_res_venda <- preco_residencial_venda %>%
                rename(date = X__1, s_1022 = `Porto Alegre`) %>%
                trata_fipe()


pr_res_locacao <- preco_residencial_locacao %>%
                  rename(date = X__1, s_1023 = `Porto Alegre`) %>%
                  trata_fipe()

rental_resid <- rental_residencial %>%
                rename(date = X__1, s_1024 = `Porto Alegre`) %>%
                trata_fipe()

pr_com_venda <- preco_comercial_venda %>%
                rename(date = X__1, s_1025 = `Porto Alegre`) %>%
                trata_fipe()

pr_com_locacao <- preco_comercial_locacao %>%
                  rename(date = X__1, s_1026 = `Porto Alegre`) %>%
                  trata_fipe()

rental_comer <- rental_comercial %>%
                rename(date = X__1, s_1027 = `Porto Alegre`) %>%
                trata_fipe()



# Joining bases Sem
Sem <- Reduce(function(x, y) merge(x, y, by = c("date", "month", "quarter", "year"), all=TRUE), list(pim,                                                        
                                                                                                     pmc, 
                                                                                                     pms, 
                                                                                                     ibcr, 
                                                                                                     taxa_des, 
                                                                                                     tempo_des, 
                                                                                                     massa_ren, 
                                                                                                     icei, 
                                                                                                     uci, 
                                                                                                     compras_industriais, 
                                                                                                     idi, 
                                                                                                     credito, 
                                                                                                     inadimplencia, 
                                                                                                     agronegocio, 
                                                                                                     pr_res_venda, 
                                                                                                     pr_res_locacao, 
                                                                                                     rental_resid, 
                                                                                                     pr_com_venda, 
                                                                                                     pr_com_locacao, 
                                                                                                     rental_comer, 
                                                                                                     ipca)) %>% arrange(year, month)
       



# Joining bases Com ajuste sazonal na fonte original
Com_original <- Reduce(function(x, y) merge(x, y, by = c("date", "month", "quarter", "year"), all=TRUE), list(pim_sa,
                                                                                                              #pmc_sa,
                                                                                                              pms_sa, 
                                                                                                              ibcr_sa, 
                                                                                                              uci_sa, 
                                                                                                              compras_industriais_sa, 
                                                                                                              idi_sa, 
                                                                                                              ipca_sa)) %>% arrange(year, month)
  

  


# Aplica algoritmo da sazonalidade! #



###################################################################
#######FUNÇÃO AUXILIAR DE DESSAZONALIZAÇÃO DO BANCO DE DADOS#######
###################################################################

AS <- function(bancots,exporta=T){
  
  agregts=bancots
  
  lista<-list()
  
  for(i in 1:ncol(agregts)){
    lista[[i]]<-agregts[,i]
  }
  
  agreg_SA<-lapply(lista, function(x) try(seas(ts(x,start=start(agregts),freq=12),
                                               na.action=na.omit,
                                               pickmdl.method="best",
                                               pickmdl.identify="all",
                                               transform.function = "auto",
                                               regression.aictest = c("td", "easter"),
                                               outlier.types="all",
                                               #outlier.method="addall"
                                               outlier.method="addone",
                                               x11="", forecast.maxlead=18,
                                               forecast.maxback=0)))
  
  names(agreg_SA)<-colnames(agregts)
  
  
  # Coletando as séries que deram erro
  is.err <- sapply(agreg_SA, class) == "try-error"
  agreg_SA[is.err]
  
  
  # Pegando as séries que deram erro e usando outro método pra dessazonalizar (outliers sem level shift)
  
  erros1<-names(agreg_SA[is.err])
  
  if(length(erros1)==1){
    #lista2<-list(agregts[,erros1])
    lista2<-list()
    for(i in 1:ncol(agregts[,erros1])){
      lista2[[i]]<-agregts[,erros1][,i]
    }
    
    agreg_SA2<-lapply(lista2, function(x) try(seas(ts(x,start=start(agregts[,erros1]),freq=12),
                                                   na.action=na.omit,
                                                   pickmdl.method="best",
                                                   pickmdl.identify="all",
                                                   transform.function = "auto",
                                                   regression.aictest = c("td", "easter"),
                                                   outlier.types=c("ao","tc"),
                                                   #outlier.method="addall"
                                                   outlier.method="addone",
                                                   x11="", forecast.maxlead=18,
                                                   forecast.maxback=0)))
    
    
    names(agreg_SA2)<-erros1
    
    
    #Coletando as séries que deram erro novamente
    is.err2 <- sapply(agreg_SA2, class) == "try-error"
    agreg_SA2[is.err2]
  }else{
    if(length(erros1)>1){
      lista2<-list()
      
      for(i in 1:ncol(agregts[,erros1])){
        lista2[[i]]<-agregts[,erros1][,i]
      }
      
      agreg_SA2<-lapply(lista2, function(x) try(seas(ts(x,start=start(agregts[,erros1]),freq=12),
                                                     na.action=na.omit,
                                                     pickmdl.method="best",
                                                     pickmdl.identify="all",
                                                     transform.function = "auto",
                                                     regression.aictest = c("td", "easter"),
                                                     outlier.types=c("ao","tc"),
                                                     #outlier.method="addall"
                                                     outlier.method="addone",
                                                     x11="", forecast.maxlead=18,
                                                     forecast.maxback=0)))
      
      
      names(agreg_SA2)<-colnames(agregts[,erros1])
      
      
      #Coletando as séries que deram erro novamente
      is.err2 <- sapply(agreg_SA2, class) == "try-error"
      agreg_SA2[is.err2]
    }
  }
  
  if(length(agreg_SA[is.err])==0){
    series_SA<-zoo(do.call(cbind, lapply(agreg_SA[!is.err], final)))
  }else{
    lista_SA<-lapply(list(agreg_SA[!is.err],agreg_SA2[!is.err2]),
                     lapply,final)
    lista_SA_1<-do.call(cbind,lista_SA[[1]])
    lista_SA_2<-do.call(cbind,lista_SA[[2]])
    series_SA<-zoo(do.call(cbind,list(lista_SA_1,lista_SA_2)))
    colnames(series_SA)<-c(colnames(lista_SA_1),ifelse(length(agreg_SA2[!is.err2])>1,
                                                       colnames(lista_SA_2),
                                                       names(agreg_SA2[!is.err2])))
    series_SA<-series_SA[,order(match(names(series_SA),colnames(agregts)))]
  }
  
  
  
  #Exportando
  
  
  if(exporta==T){
    write.table(cbind.data.frame(Data=yearmon(index(series_SA)),series_SA),"Series_SA.csv",sep=";",dec=",",
                row.names=F)
  }
  
  return(series_SA)
}


#### FIM DA FUNÇÃO AUXILIAR


# Aplicando a dessazonalização:

dfts<-ts(Sem[,-c(1:4)],start=c(1992,1),freq=12)
dfts2<-dfts[,-which(colnames(dfts)=="s_1017"| colnames(dfts)=="s_1023"| colnames(dfts)=="s_1024"| colnames(dfts)=="s_1025" |colnames(dfts)=="s_1026" | colnames(dfts)=="s_1027")] #GAMBIARRA PRA EXCLUIR A S?RIE QUE T? DANDO PROBLEMA ("s_1017")
#dfzoo<-zoo(dfts2)

# Dessazonalizando o banco de dados
series_SA<-AS(bancots=dfts2,exporta=T)
datas <- yearmon(index(series_SA)) # Aki tava dando pau! Começa em 1992 a ts lá em cima
Com <- tbl_df(series_SA) %>% 
       mutate(Datas = as.character(datas)) %>% 
       separate(Datas, into = c("mes", "year"), sep = 3) %>%
       mutate(ano = str_sub(year, start = 4, end = 5), 
              dia = "1", 
              mes_ing = mes_portu_para_ingles(mes),
              month = as.character(mes_ingles_p_numero(mes_ing)), 
              quarter = as.integer(retorna_quarter(month)), 
              year = transforma_YY_para_YYYY(ano),
              date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
       dplyr::select(-mes, -dia, -ano, -mes_ing) %>%
       ordena_colunas()

series_que_tem_na_fonte <- Com_original %>% dplyr::select(contains("s_")) %>% colnames()

# Séries que tiveram ajustes sazonais que não tem dessazonalização na fonte
Com_que_fazem_sentido <- Com[,c(setdiff(names(Com), series_que_tem_na_fonte))]

Sem_Final_pre_pre <- Sem
Com_Final_pre_pre <- merge(Com_que_fazem_sentido, Com_original, by = c("date", "month", "quarter", "year"), all = TRUE) %>% mutate(month = as.integer(month), date = as.character(date), year = as.integer(year)) %>% arrange(year, month)


  

# Somente rodar abaixo de quiser mergear índices que estão na base original (Fecomércio, alphaplan, etc.)
#setwd("C:\\Users\\renan\\Desktop\\ciclovis-rs")
setwd("C:\\Users\\renan\\Dropbox\\Indicadores FEE\\Business Cycle Tracer\\Shiny - Business Cycle")

#base_jeff_sem <- readRDS("base_ciclo_V10.rds")
#base_jeff_com <- readRDS("base_ciclo_SA_V10.rds")

base_jeff_sem <- read_csv2("base_dados - credito emprego.csv")
base_jeff_com <- read_csv2("base_dados_SA - credito emprego.csv") %>%
                 mutate(Datas = as.character(Data)) %>% 
                 separate(Datas, into = c("mes", "year"), sep = 3) %>%
                 mutate(ano = str_sub(year, start = 4, end = 5), 
                        dia = "1", 
                        mes_ing = mes_portu_para_ingles(mes),
                        month = as.character(mes_ingles_p_numero(mes_ing)), 
                        quarter = as.integer(retorna_quarter(month)), 
                        year = transforma_YY_para_YYYY(ano),
                        date = as.factor(paste0("1/",extrai_mes_short(month),"/",str_sub(year,3)))) %>%
                 dplyr::select(-mes, -dia, -ano, -mes_ing, -Data) %>%
                 ordena_colunas() %>%
                 mutate(month = as.integer(month),
                        year = as.integer(year),
                        date = as.character(date)) %>%
                 arrange(year, month)

setwd("C:\\Users\\renan\\Desktop\\ciclovis-rs")

Sem_Final_pre <- Sem_Final_pre_pre #%>% select(-s_1015)
Com_Final_pre <- Com_Final_pre_pre #%>% select(-s_1015)

series_handmade_1 <- c("s_1014", "s_1017", "s_1021") # , "s_1015"
series_handmade_2 <- c("s_1014", "s_1021") # , "s_1015"

jeff_sem_importa <- base_jeff_sem[,c("month", "year", series_handmade_1)]
jeff_com_importa <- base_jeff_com[,c("month", "year", series_handmade_2)]

Sem_Final <- merge(Sem_Final_pre, jeff_sem_importa, by = c("month", "year"), all = TRUE) %>% arrange(year, month) # all = TRUE pq na base original do jefferson, podem ter meses que não tinham na outra
Com_Final <- merge(Com_Final_pre, jeff_com_importa, by = c("month", "year"), all = TRUE) %>% arrange(year, month)

saveRDS(Sem_Final, "base_ciclo_VX.rds")
saveRDS(Com_Final, "base_ciclo_SA_VX.rds")
  



