# limpando ambiente -------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))

# carregando pacotes ------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)

# carregando dados --------------------------------------------------------------------------------------------------------------------

arquivos <- list.files("data/raw data/")
arquivos <- arquivos[1:(length(arquivos)-1)]

lista_de_tabelas <- list()

for(i in 1:length(arquivos)) {
  lista_de_tabelas[[i]] <- read_excel(paste0("data/raw data/",arquivos[i]))
}

tabela <- do.call(rbind.data.frame, lista_de_tabelas)

# criando ids dos nomes ---------------------------------------------------------------------------------------------------------------

nomes_id <- tabela %>% 
  select(Nome) %>% 
  distinct(Nome) %>% 
  arrange(Nome) %>% 
  mutate(id = seq_along(Nome),
         Nome = gsub(pattern = "Mayara Vescossi Assis", replacement = "Mayara Assis", x = Nome),
         Nome = gsub(pattern = "Nathalia Rezende", replacement = "Natalia Resende de Souza", x = Nome),
         Nome = gsub(pattern = "Raquel Mattos G da Costa", replacement = "Raquel Matos G da Costa", x = Nome),
         Nome = gsub(pattern = "Sorana Karenina Antonia Francesquini de Lima", replacement = "Sorana Karenina A F Lima", x = Nome),
         Nome = gsub(pattern = "Vinicius F Farjalla", replacement = "Vinicius Fortes Farjalla", x = Nome))

# carregando tabela dos seminarios ----------------------------------------------------------------------------------------------------

seminarios <- read_excel("data/raw data/Lista de Palestras.xlsx") %>% 
  mutate(id_seminario = paste0(Dia,Mes,Ano)) %>% 
  select(-Dia,-Mes,-Ano)

# limpando tabela ---------------------------------------------------------------------------------------------------------------------

tabela <- tabela %>% 
  mutate(Formacao = ifelse(Formacao == "Graduanda", "Graduacao", Formacao), 
         id_seminario = paste0(Data,Mes,Ano),
         Nome = gsub(pattern = "Mayara Vescossi Assis", replacement = "Mayara Assis", x = Nome),
         Laboratorio = gsub(pattern = "Plantas", replacement = "LEV", x = Laboratorio),
         Nome = gsub(pattern = "Nathalia Rezende", replacement = "Natalia Resende de Souza", x = Nome),
         Nome = gsub(pattern = "Raquel Mattos G da Costa", replacement = "Raquel Matos G da Costa", x = Nome),
         Nome = gsub(pattern = "Sorana Karenina Antonia Francesquini de Lima", replacement = "Sorana Karenina A F Lima", x = Nome),
         Formacao = ifelse(Nome == "Sorana Karenina A F Lima", "Licenciatura", Formacao),
         Nome = gsub(pattern = "Vinicius F Farjalla", replacement = "Vinicius Fortes Farjalla", x = Nome)) %>% 
  left_join(nomes_id, by = "Nome") %>% 
  select(-Nome) %>% 
  select(id, everything()) %>% 
  left_join(seminarios, by = "id_seminario") %>% 
  select(-id_seminario)

names(tabela) <- gsub(pattern = "\\.x", replacement = "_ouvinte", x = names(tabela))
names(tabela) <- gsub(pattern = "\\.y", replacement = "_palestrante", x = names(tabela))
names(tabela) <- tolower(names(tabela))

# salvando tabela ---------------------------------------------------------------------------------------------------------------------

write.table(x = tabela, file = "data/tidy data/lista_de_presenca.xls", sep = "\t", row.names = FALSE)
write.table(x = nomes_id, file = "data/tidy data/chave_id.xls", sep = "\t", row.names = FALSE)
