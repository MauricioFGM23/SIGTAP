library(dplyr)
library(janitor)
library(readxl)

procedimentos <- read_excel("dados/procedimentos.xlsx")%>%
  clean_names()

lista_proc <- procedimentos %>%
  select(codigos) %>%
  rename(
    procedimento = codigos
  )%>%
  mutate(
    procedimento = as.numeric(procedimento)
  )

base <- read.csv2("dados/base_ambulatorial.csv", nrows = 10)
  clean_names() %>%
  select(ano, procedimento, quantidade_corrigida) %>%
  mutate(
    procedimento = as.numeric(procedimento)
  ) %>%
  filter(
    ano == 2025,
    procedimento %in% lista_proc$procedimento
  ) %>%
  group_by(procedimento, ano) %>%
  summarise(
    qtd = sum(quantidade_corrigida)
  )

write.csv2(base, "saida/dados_exames.csv", fileEncoding = "latin1")
