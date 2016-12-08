# limpando ambiente -------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))

# carregando pacotes ------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(readxl)

# carregando dados --------------------------------------------------------------------------------------------------------------------

arquivos <- list.files("/Users/Nicholas/Dropbox/Laboratório/Outros/Palestras/")
arquivos <- arquivos[1:10]

lista_de_tabelas <- list()

for(i in 1:length(arquivos)) {
  lista_de_tabelas[[i]] <- read_excel(paste0("/Users/Nicholas/Dropbox/Laboratório/Outros/Palestras/",arquivos[i]))
}

tabela <- do.call(rbind.data.frame, lista_de_tabelas)

# salvando tabela ---------------------------------------------------------------------------------------------------------------------


