require(data.table)
require(tidyverse)
setwd("C:/Users/renan/Desktop")

# Os nomes dos crimes tem que serem iguais! 
# Principais Problemas: 
# "Delitos Relacionados à Corrupção"
# "Entorpecentes - Posse"
# "Entorpecentes - Tráfico"
# "Furto de Veículo"
# "Furtos"
# "Roubo de Veículo"
# "Roubos"


base_crime <- readRDS("BaseCrime.rds")

base_pop <- base_crime %>% 
            filter(Ano == 2015, Crime == "Roubo") %>% 
            select(CodIBGE, Populacao)


base_pre <- tbl_df(fread("Crimes2016RS.csv", header = T, sep = ";")) %>%
            gather(Crime, Qtd, -Mun, -CodIBGE) %>%
            mutate(Ano = 2016) %>%
            inner_join(base_pop, by = "CodIBGE") %>%
            select(Mun, CodIBGE, Ano, Crime, Qtd, Populacao) # Reordenar as colunas para o bind


base_crime_2016 <- bind_rows(base_crime, base_pre)
base_crime_2016$Crime <- as.factor(base_crime_2016$Crime)


saveRDS(base_crime_2016, "base_crime_2016.rds")