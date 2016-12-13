# limpando ambiente -------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))

# carregando pacotes ------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(betapart)
library(metacom)

# carregando dados --------------------------------------------------------------------------------------------------------------------

seminarios <- read_tsv("data/tidy data/lista_de_presenca.xls")

# dados das palestras -----------------------------------------------------------------------------------------------------------------

dados <- seminarios %>% 
  mutate(palestra = paste0(data,mes,ano)) %>% 
  select(palestra, palestra:`tema #1`) %>% 
  distinct %>% 
  mutate(ordem = c(4, 11, 7, 1, 8, 5, 2, 9, 6, 3, 10)) %>% 
  arrange(ordem)

# criando matriz de presenca ausencia -------------------------------------------------------------------------------------------------

presencas <- seminarios %>% 
  filter(id != 5) %>% 
  mutate(palestra = paste0(data,mes,ano)) %>% 
  select(id, palestra) %>% 
  mutate(presenca = rep(1, nrow(.))) %>% 
  spread(key = id, value = presenca, fill = 0) %>% 
  left_join(select(dados, palestra, ordem), by = "palestra") %>% 
  arrange(ordem) %>% 
  select(-ordem)

# particionando beta ------------------------------------------------------------------------------------------------------------------



