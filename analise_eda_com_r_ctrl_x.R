# -*- coding: utf-8
# Abraji (https://www.abraji.org.br)
# Reinaldo Chaves (reinaldo@abraji.org.br)
# Programa exemplo de como usar a base do Ctrl+X (http://www.ctrlx.org.br) com R
# Exploratory Data Analysis
# 

# Marca o diretório de trabalho
setwd("/Users/abraji/Code/abraji/curso_congresso2019/repo/ferramentas_abraji")

#Mostra
getwd()

#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("DT")

library(readxl)
library(tidyverse)
library(DT)

# Exemplo com o download da base completa no dia 24/6/2019
df_processos <- read_excel("dados/abraji-ctrlx_24_jun_2019.xlsx", sheet=1, skip=3)

# O símbolo "*" não é bem aceito em comandos do R quando está no título de colunas
# Vamos mudar os nomes das colunas
names(df_processos)[names(df_processos) == "Nome_Político_Autor*"] <- "Nome_Político_Autor"
names(df_processos)[names(df_processos) == "CPF_Político_Autor*"] <- "CPF_Político_Autor"
names(df_processos)[names(df_processos) == "Partido_Político_Autor*"] <- "Partido_Político_Autor"

# Nomes das colunas
colnames(df_processos)

# Quantas linhas
nrow(df_processos)

# Cinco primeiras linhas
head(df_processos, 5)

# Resumo e tipos das colunas
str(df_processos)

# Quantos são autores no dataframe principal
how_many <- unique(df_processos$Nome_Político_Autor)
length(how_many)

# Outra forma de olhar um resumo do dataframe - essa função é do dplyr
glimpse(df_processos)

# Quantos são processos apenas vindos de políticos e soma por partido - dez primeiros
filter(df_processos, Quem_é_o_Autor=="Político") %>% 
  group_by(Partido_Político_Autor) %>% 
  summarize(total=n()) %>%    
  arrange(desc(total)) %>% 
  head(10)

# Prepara nova coluna para trabalhar com datas
require("lubridate")
# Mostra o local
Sys.getlocale("LC_TIME")

# Cria nova coluna com formato de data e uma com o Ano
df_processos$Data_da_Propositura_uso_final <- ymd(df_processos$Data_da_Propositura)
df_processos$Ano_da_Propositura <- year(df_processos$Data_da_Propositura_uso_final)

glimpse(df_processos)

# Quem foi o maior político autor de processos em 2018 
filter(df_processos, Quem_é_o_Autor=="Político" & Ano_da_Propositura == 2018) %>% 
  group_by(Nome_Político_Autor) %>% 
  summarize(total=n()) %>%    
  arrange(desc(total)) %>% 
  head(10)

# Quem foi o maior político autor de processos em 2018 e com Retirada_deferida_em_algum_momento como Sim
filter(df_processos, Quem_é_o_Autor=="Político" & Ano_da_Propositura == 2018  & Retirada_deferida_em_algum_momento == "Sim") %>% 
  group_by(Nome_Político_Autor) %>% 
  summarize(total=n()) %>%    
  arrange(desc(total)) %>% 
  head(10)

# Evolução dos processos no Estado da BA por ano
#lag() - Calcula uma versão defasada de uma série temporal, deslocando a base de tempo de volta por um determinado número de observações

#No caso aqui, podemos calcular a diferença no número de ações ano após ano
anos <- filter(df_processos, UF=="BA" & Quem_é_o_Autor=="Político") %>% 
  group_by(Ano_da_Propositura) %>% 
  summarize(total=n()) %>%
  mutate(ano_anterior=lag(total), variacao=total-ano_anterior) 

anos

# Ações de políticos, com Retirada_deferida_em_algum_momento como Sim
# group_by pelo nome da Empresa_Ré e pelo tipo de Alegação na ação
filter(df_processos, Quem_é_o_Autor=="Político"  & Retirada_deferida_em_algum_momento == "Sim") %>% 
  group_by(Empresa_Ré, Alegação) %>% 
  summarize(total=n()) %>% 
  arrange(desc(total)) %>% 
  head(20)

# Em SP, qual a porcentagem de ações de políticos em relação ao total
percent_processos_sp <- df_processos %>% 
  group_by(UF, Nome_Político_Autor) %>% 
  filter(UF=="SP" & Quem_é_o_Autor=="Político") %>% 
  summarize(total=n()) %>% 
  mutate(percent=total/sum(total, na.rm=T)*100) %>% 
  arrange(-percent)

# Usando a biblioteca DT (DataTables) que nos permite criar tabelas pesquisáveis com o plug-in para jQuery
datatable(percent_processos_sp)
