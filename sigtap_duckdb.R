library(duckdb)
library(janitor)
library(readxl)
library(dplyr)

head_dados <- read.csv2("dados/base_ambulatorial.csv", nrows = 1)

#CARREGAR DADOS COM DUCKDB E PLANILHAS AUXILIARES ####
con <- dbConnect(duckdb())

dados_duck <- dbGetQuery(con, "
  SELECT ANO, Procedimento, Quantidade_corrigida
  FROM   read_csv_auto('dados/base_ambulatorial.csv')
  WHERE  ANO = 2025
")

dbDisconnect(con, shutdown = TRUE)

procedimentos <- read_excel("dados/Cópia de Rol de procedimento _IMAGEM_ com incremento.xlsx")%>%
  clean_names()

lista_proc <- procedimentos %>%
  select(codigos) %>%
  rename(
    procedimento = codigos
  )%>%
  mutate(
    procedimento = as.numeric(procedimento)
  )

#FORMATAR DADOS ####
dados_formatados <- dados_duck %>%
  clean_names()%>%
  mutate(
    procedimento = as.numeric(procedimento)
  )

dados_group <- dados_formatados %>%
  group_by(procedimento)%>%
  summarise(
    qtd_procedimento_2025 = sum(quantidade_corrigida)
  )

#FORTAR MATRIZ E REALIZAR O MERGE ####

procedimentos_format <- procedimentos %>%
  rename(
    co_procedimento = codigos,
    no_procedimento = nome,
    vlr_sigtap_unit = valor_digtap_r,
    perc_incremento = percent_incremento,
    vlr_final       = valor_final,
    obs             = observacao
  )%>%
  left_join(
    dados_group,
    by = c("co_procedimento" = "procedimento" )
  )%>%
  mutate(
    vlr_final = as.numeric(vlr_final),
    vlr_sigtap_unit = as.numeric(vlr_sigtap_unit)
  )


procedimentos_calc <- procedimentos_format%>%
  mutate(
    exec_financeira_2025 = vlr_sigtap_unit * qtd_procedimento_2025,
    projecao_financeira_novo_calc = vlr_final * qtd_procedimento_2025,
    perc_cresc_financeiro = projecao_financeira_novo_calc / exec_financeira_2025
  )


