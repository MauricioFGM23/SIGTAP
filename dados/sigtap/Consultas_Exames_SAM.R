# =========================================================
# 📦 PACOTES
# =========================================================
library(data.table)
library(stringr)

# =========================================================
# 📁 0) ARQUIVO
# =========================================================
arq <- "/Users/felipecotrim/Downloads/Pan. Ambul. e Hosp. Clinico_versao15_03_2026.csv"

# =========================================================
# 🔎 1) CABEÇALHO (ver variáveis disponíveis)
# =========================================================
nomes <- names(fread(arq, nrows = 0, showProgress = FALSE))
cat("✅ Total de variáveis:", length(nomes), "\n")
print(nomes)

# (opcional) ver 5 primeiras linhas completas
print(fread(arq, nrows = 5, showProgress = FALSE))

# =========================================================
# 🎯 2) COLUNAS NECESSÁRIAS
# =========================================================
cols_need <- c(
  "MES",
  "ANO",
  "UF",
  "Municipio",
  "Valor",
  "Nome_C.Atendimento",
  "Procedimento",
  "Secundario de OCI",
  "Rol OCI",
  "Nome do procedimento",
  "categorias tabela",
  "Quantidade_corrigida"
)

cols_missing <- setdiff(cols_need, nomes)
if (length(cols_missing) > 0) {
  stop(paste("⚠️ Colunas não encontradas:", paste(cols_missing, collapse = ", ")))
}

cat("✅ Colunas selecionadas:\n")
print(cols_need)

# =========================================================
# 📥 3) LER SÓ AS COLUNAS NECESSÁRIAS (UMA ÚNICA VEZ)
# =========================================================
dt <- fread(
  arq,
  select = cols_need,
  showProgress = TRUE
)
# =========================================================
# 🔄 RENOMEAR PARA PADRÃO INTERNO
# =========================================================
setnames(
  dt,
  old = c(
    "MES",
    "ANO",
    "UF",
    "Municipio",
    "Valor",
    "Nome_C.Atendimento",
    "Procedimento",
    "Secundario de OCI",
    "Rol OCI",
    "Nome do procedimento",
    "categorias tabela",
    "Quantidade_corrigida"
  ),
  new = c(
    "mes",
    "ano",
    "uf",
    "municipio",
    "valor",
    "tipo_atendimento",
    "procedimento",
    "sec_oci",
    "rol_oci",
    "nome_procedimento",
    "categoria",
    "qtd"
  )
)

cat("✅ Base carregada:", nrow(dt), "linhas |", ncol(dt), "colunas\n")

#### Filtro par aano se precisar ####
dt_2025 <- dt[ano == 2025]

cat("✅ Base 2025:", nrow(dt_2025), "linhas\n")

# =========================================================
# 💾 5) SALVAR EM RDS
# =========================================================
saveRDS(
  dt_2025,
  file = "/Users/felipecotrim/Downloads/baseCONS_EXA_panorama_2025.rds"
)

cat("💾 Arquivo salvo com sucesso!\n")

# =========================================================
# 🩺 6) CRIAR BASE DE CONSULTAS E TELECONSULTAS
#    (mantendo TODAS as colunas, inclusive valor)
# =========================================================
cod_consultas <- c(301010072, 301010307)

dt_consultas <- copy(
  dt[procedimento %in% cod_consultas]
)

cat("✅ Base de consultas/teleconsultas criada com",
    nrow(dt_consultas), "linhas e", ncol(dt_consultas), "colunas\n")

# conferir estrutura
str(dt_consultas)

# conferir distribuição dos procedimentos
dt_consultas[
  ,
  .(
    qtd_registros = .N,
    qtd_produzida = sum(qtd, na.rm = TRUE)
  ),
  by = .(procedimento, nome_procedimento)
][order(procedimento)]

# =========================================================
# 💰 6.1) TRATAR VALOR MONETÁRIO NA BASE DE CONSULTAS
# =========================================================
dt_consultas[
  ,
  valor_num := as.numeric(
    gsub(",", ".", gsub("\\.", "", trimws(valor)))
  )
]

summary(dt_consultas$valor_num)

# =========================================================
# 🏷️ 7) MARCAR O QUE ESTÁ DENTRO DA OCI PELA CATEGORIA
# =========================================================
cat_oci_sec <- "Consultas / Atendimentos / Acompanhamentos | FAEC | Secund. de OCI"

dt_consultas[, dentro_oci := categoria == cat_oci_sec]

# opcional: conferir categorias existentes
unique(dt_consultas$categoria)

# =========================================================
# 📊 8) TABELA ANUAL - TOTAL E DENTRO DA OCI
# =========================================================
tab_anual_consultas <- dt_consultas[
  ,
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = ano
][order(ano)]

# percentual dentro da OCI
tab_anual_consultas[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_anual_consultas)


# =========================================================
# 🩺 9) CRIAR RÓTULO DO TIPO DE PROCEDIMENTO
# =========================================================
dt_consultas[
  ,
  tipo_proc := fifelse(
    procedimento == 301010072,
    "Consulta médica especializada",
    "Teleconsulta em atenção especializada"
  )
]

# =========================================================
# 📊 10) TABELA ANUAL POR TIPO DE PROCEDIMENTO
# =========================================================
tab_anual_tipo <- dt_consultas[
  ,
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = .(ano, tipo_proc)
][order(ano, tipo_proc)]

tab_anual_tipo[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_anual_tipo)

# =========================================================
# 📅 TABELA MENSAL DE 2025 - TOTAL E DENTRO DA OCI
# =========================================================
tab_mensal_2025_consultas <- dt_consultas[
  ano %in% c(2025, 2026),
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = mes
][order(mes)]

# percentual da OCI no mês
tab_mensal_2025_consultas[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_mensal_2025_consultas)


#### FINANCEIRO - CONSULTAS E TELE ####
# =========================================================
# 📊 TABELA ANUAL - VALOR TOTAL
# =========================================================
tab_anual_valor <- dt_consultas[
  ,
  .(
    vl_total = sum(valor_num, na.rm = TRUE)
  ),
  by = ano
][order(ano)]

print(tab_anual_valor)

# =========================================================
# 📅 TABELA MENSAL 2025 - VALOR TOTAL
# =========================================================
tab_mensal_valor_2025 <- dt_consultas[
  ano %in% c(2025, 2026),
  .(
    vl_total = sum(valor_num, na.rm = TRUE)
  ),
  by = mes
][order(mes)]

print(tab_mensal_valor_2025)


# =========================================================
# 🩺 6A) CRIAR BASE DE CONSULTAS E TELECONSULTAS - SOMENTE ELETIVAS
# =========================================================
dt_consultas_eletivas <- copy(
  dt_consultas[grepl("ELETIVO", tipo_atendimento, ignore.case = TRUE)]
)

cat("✅ Base de consultas/teleconsultas ELETIVAS criada com",
    nrow(dt_consultas_eletivas), "linhas e", ncol(dt_consultas_eletivas), "colunas\n")

# conferir distribuição
dt_consultas_eletivas[
  ,
  .(
    qtd_registros = .N,
    qtd_produzida = sum(qtd, na.rm = TRUE),
    vl_total = sum(valor_num, na.rm = TRUE)
  ),
  by = .(procedimento, nome_procedimento)
][order(procedimento)]

# =========================================================
# 📊 TABELA ANUAL - TOTAL E DENTRO DA OCI (SOMENTE ELETIVAS)
# =========================================================
tab_anual_consultas_eletivas <- dt_consultas_eletivas[
  ,
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = ano
][order(ano)]

tab_anual_consultas_eletivas[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_anual_consultas_eletivas)

# =========================================================
# 📊 TABELA ANUAL POR TIPO DE PROCEDIMENTO (SOMENTE ELETIVAS)
# =========================================================
tab_anual_tipo_eletivas <- dt_consultas_eletivas[
  ,
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = .(ano, tipo_proc)
][order(ano, tipo_proc)]

tab_anual_tipo_eletivas[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_anual_tipo_eletivas)

# =========================================================
# 📅 TABELA MENSAL DE 2025 - TOTAL E DENTRO DA OCI (SOMENTE ELETIVAS)
# =========================================================
tab_mensal_2025_consultas_eletivas <- dt_consultas_eletivas[
  ano %in% c(2025, 2026),
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = mes
][order(mes)]

tab_mensal_2025_consultas_eletivas[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_mensal_2025_consultas_eletivas)

# =========================================================
# 💰 TABELA ANUAL - VALOR TOTAL (SOMENTE ELETIVAS)
# =========================================================
tab_anual_valor_eletivas <- dt_consultas_eletivas[
  ,
  .(
    vl_total = sum(valor_num, na.rm = TRUE)
  ),
  by = ano
][order(ano)]

print(tab_anual_valor_eletivas)

# =========================================================
# 💰 TABELA MENSAL 2025 - VALOR TOTAL (SOMENTE ELETIVAS)
# =========================================================
tab_mensal_valor_2025_eletivas <- dt_consultas_eletivas[
  ano %in% c(2025, 2026),
  .(
    vl_total = sum(valor_num, na.rm = TRUE)
  ),
  by = mes
][order(mes)]

print(tab_mensal_valor_2025_eletivas)




#### EXAMES ####
# =========================================================
# 🧾 1) LISTA FECHADA DE CÓDIGOS (10 dígitos)
# =========================================================
codigos_exames_raw <- c(
  # Cardiologia
  "02.05.01.001-6",
  "02.11.02.003-6",
  
  # Ginecologia
  "02.03.02.003-0",
  "02.07.03.002-2",
  "02.05.02.016-0",
  "02.05.02.018-6",
  
  # Oftalmologia
  "02.11.06.002-0",
  "02.11.06.017-8",
  
  # Oncologia
  "02.09.01.002-9",
  "02.09.01.003-7",
  "02.03.02.006-5",
  "02.03.02.008-1",
  "02.03.02.002-2",
  "02.03.02.003-0",
  "02.04.03.003-0",
  
  # Ortopedia
  "02.05.02.006-2",
  
  # Otorrinolaringologia
  "02.11.07.004-1",
  "02.09.04.004-1",
  
  # Ressonância Magnética
  "02.07.01.003-0",
  "02.07.01.004-8",
  "02.07.01.005-6",
  "02.07.02.002-7",
  "02.07.03.002-2",
  "02.07.03.003-0",
  
  # Tomografia Computadorizada
  "02.06.02.002-3",
  "02.06.03.002-9",
  "02.06.03.003-7",
  "02.06.01.001-0",
  "02.06.01.002-8",
  "02.06.01.003-6",
  "02.06.02.001-5"
)

codigos_ok <- substr(gsub("\\D", "", codigos_exames_raw), 1, 10)
codigos_ok <- unique(codigos_ok)

# =========================================================
# 🧹 2) CRIAR CÓDIGO PADRONIZADO NA BASE ORIGINAL
# =========================================================
dt_exames <- copy(dt)

dt_exames[
  ,
  codigo := stringr::str_pad(as.character(procedimento), 10, "left", "0")
]

dt_exames <- dt_exames[codigo %in% codigos_ok]

# =========================================================
# 🔎 4) CONFERIR QUAIS PROCEDIMENTOS ENTRARAM
# =========================================================
tab_exames_proc <- dt_exames[
  ,
  .(
    qtd_registros = .N,
    qtd_produzida = sum(qtd, na.rm = TRUE),
    vl_total = sum(as.numeric(gsub(",", ".", gsub("\\.", "", trimws(valor)))), na.rm = TRUE)
  ),
  by = .(codigo, nome_procedimento)
][order(codigo)]

print(tab_exames_proc)

dt_exames[
  ,
  valor_num := as.numeric(
    gsub(",", ".", gsub("\\.", "", trimws(valor)))
  )
]

# =========================================================
# 🏷️ MARCAR EXAMES DENTRO DA OCI (REGRA DEFINIDA)
# =========================================================
cat_oci_exames <- "Exames Complementares | FAEC | Secund. de OCI"

dt_exames[
  ,
  dentro_oci := categoria == cat_oci_exames
]

# =========================================================
# 📊 2) TABELA ANUAL DE EXAMES - TOTAL E DENTRO DA OCI
# =========================================================
tab_anual_exames <- dt_exames[
  ,
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = ano
][order(ano)]

# percentual dos exames dentro da OCI
tab_anual_exames[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_anual_exames)

#mensal
tab_mensal_exames <- dt_exames[
  ano %in% c(2025, 2026),
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = .(ano, mes)
][order(ano, mes)]

tab_mensal_exames[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_mensal_exames)

# =========================================================
# 🏆 TOP 20 EXAMES MAIS REALIZADOS - TOTAL (2025)
# =========================================================
top20_exames_total_2025 <- dt_exames[
  ano == 2025,
  .(
    qtd_total = sum(qtd, na.rm = TRUE)
  ),
  by = .(codigo, nome_procedimento)
][
  order(-qtd_total)
][
  1:20
]

print(top20_exames_total_2025)

# =========================================================
# 🏆 TOP 20 EXAMES MAIS REALIZADOS - OCI (2025)
# =========================================================
top20_exames_oci_2025 <- dt_exames[
  ano == 2025 & dentro_oci == TRUE,
  .(
    qtd_oci = sum(qtd, na.rm = TRUE)
  ),
  by = .(codigo, nome_procedimento)
][
  order(-qtd_oci)
][
  1:20
]

print(top20_exames_oci_2025)



# =========================================================
# 💾 SALVAR BASE COMPLETA DE EXAMES (CSV)
# =========================================================
fwrite(
  dt_exames,
  "/Users/felipecotrim/Downloads/base_exames.csv",
  sep = ";",
  dec = ",",
  bom = TRUE
)




#### =========================================================
#### EXAMES ONCOLÓGICOS | BRASIL E MG | TABELAS E GRÁFICOS
#### =========================================================

library(data.table)
library(ggplot2)
library(stringr)
library(scales)

# =========================================================
# 1) CÓDIGOS DE EXAMES ONCOLÓGICOS
# =========================================================
cod_onco <- c(
  "0209010029", # Colonoscopia
  "0209010037", # Esofagogastroduodenoscopia
  "0203020065", # Exame anatomopatológico de mama
  "0203020081", # Exame anatomo-patológico do colo uterino - biópsia
  "0203020022", # Exame anatomopatológico do colo uterino - peça cirúrgica
  "0203020030", # Exame anatomo-patológico para congelamento/parafina
  "0204030030"  # Mamografia
)

# =========================================================
# 2) BASE DE EXAMES ONCOLÓGICOS
# =========================================================
dt_onco <- copy(dt_exames[codigo %in% cod_onco])

cat("✅ Base de exames oncológicos criada com",
    nrow(dt_onco), "linhas e", ncol(dt_onco), "colunas\n")

# padronizar nome do procedimento para uso nos gráficos
dt_onco[, proc_nome := str_squish(as.character(nome_procedimento))]

# manter só 2022 a 2025 para os gráficos de evolução
dt_onco_2225 <- dt_onco[ano %in% 2022:2025]

# =========================================================
# 3) TABELA ANUAL - BRASIL
# =========================================================
tab_onco_br_anual <- dt_onco[
  ,
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = ano
][order(ano)]

tab_onco_br_anual[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_onco_br_anual)

# =========================================================
# 4) TABELA ANUAL - MG
# =========================================================
tab_onco_mg_anual <- dt_onco[
  uf == "MG",
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = ano
][order(ano)]

tab_onco_mg_anual[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

print(tab_onco_mg_anual)

# =========================================================
# 5) TABELA MENSAL POR EXAME - BRASIL (2025 E 2026)
#    usando nome do procedimento
# =========================================================
tab_onco_br_mensal_exame <- dt_onco[
  ano %in% c(2025, 2026),
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = .(ano, mes, proc_nome)
][order(ano, mes, -qtd_total)]

tab_onco_br_mensal_exame[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

tab_onco_br_mensal_exame[
  ,
  competencia := paste0(substr(as.character(mes), 1, 4), "-", substr(as.character(mes), 5, 6))
]

setcolorder(
  tab_onco_br_mensal_exame,
  c("ano", "mes", "competencia", "proc_nome", "qtd_total", "qtd_oci", "perc_oci")
)

print(tab_onco_br_mensal_exame)

# =========================================================
# 6) TABELA MENSAL POR EXAME - MG (2025 E 2026)
#    usando nome do procedimento
# =========================================================
tab_onco_mg_mensal_exame <- dt_onco[
  uf == "MG" & ano %in% c(2025, 2026),
  .(
    qtd_total = sum(qtd, na.rm = TRUE),
    qtd_oci = sum(qtd[dentro_oci == TRUE], na.rm = TRUE)
  ),
  by = .(ano, mes, proc_nome)
][order(ano, mes, -qtd_total)]

tab_onco_mg_mensal_exame[
  ,
  perc_oci := round(100 * qtd_oci / qtd_total, 2)
]

tab_onco_mg_mensal_exame[
  ,
  competencia := paste0(substr(as.character(mes), 1, 4), "-", substr(as.character(mes), 5, 6))
]

setcolorder(
  tab_onco_mg_mensal_exame,
  c("ano", "mes", "competencia", "proc_nome", "qtd_total", "qtd_oci", "perc_oci")
)

print(tab_onco_mg_mensal_exame)

# =========================================================
# 7) BASES ANUAIS PARA GRÁFICOS
# =========================================================

# Brasil - total anual
graf_br_total <- dt_onco_2225[
  ,
  .(qtd_total = sum(qtd, na.rm = TRUE)),
  by = ano
][order(ano)]

# MG - total anual
graf_mg_total <- dt_onco_2225[
  uf == "MG",
  .(qtd_total = sum(qtd, na.rm = TRUE)),
  by = ano
][order(ano)]

# Brasil - anual por exame
graf_br_exame <- dt_onco_2225[
  ,
  .(qtd_total = sum(qtd, na.rm = TRUE)),
  by = .(ano, proc_nome)
][order(proc_nome, ano)]

# MG - anual por exame
graf_mg_exame <- dt_onco_2225[
  uf == "MG",
  .(qtd_total = sum(qtd, na.rm = TRUE)),
  by = .(ano, proc_nome)
][order(proc_nome, ano)]

# =========================================================
# 8) RANKING 2025 - BRASIL E MG
# =========================================================
rank_br_2025 <- dt_onco[
  ano == 2025,
  .(qtd_total = sum(qtd, na.rm = TRUE)),
  by = proc_nome
][order(-qtd_total)]

rank_mg_2025 <- dt_onco[
  uf == "MG" & ano == 2025,
  .(qtd_total = sum(qtd, na.rm = TRUE)),
  by = proc_nome
][order(-qtd_total)]

print(rank_br_2025)
print(rank_mg_2025)

# =========================================================
# 9) GRÁFICOS DE BARRA - EVOLUÇÃO TOTAL 2022 A 2025
# =========================================================

g_br_total <- ggplot(graf_br_total, aes(x = factor(ano), y = qtd_total)) +
  geom_col() +
  geom_text(aes(label = scales::comma(qtd_total, big.mark = ".", decimal.mark = ",")),
            vjust = -0.2, size = 3.5) +
  labs(
    title = "Brasil - Exames oncológicos realizados (2022 a 2025)",
    x = "Ano",
    y = "Quantidade"
  ) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 12)

g_mg_total <- ggplot(graf_mg_total, aes(x = factor(ano), y = qtd_total)) +
  geom_col() +
  geom_text(aes(label = scales::comma(qtd_total, big.mark = ".", decimal.mark = ",")),
            vjust = -0.2, size = 3.5) +
  labs(
    title = "Minas Gerais - Exames oncológicos realizados (2022 a 2025)",
    x = "Ano",
    y = "Quantidade"
  ) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 12)

print(g_br_total)
print(g_mg_total)

# =========================================================
# 10) GRÁFICOS DE BARRA - EVOLUÇÃO 2022 A 2025 POR EXAME
# =========================================================

g_br_exame <- ggplot(graf_br_exame, aes(x = factor(ano), y = qtd_total)) +
  geom_col() +
  geom_text(aes(label = scales::comma(qtd_total, big.mark = ".", decimal.mark = ",")),
            vjust = -0.2, size = 2.8) +
  facet_wrap(~ proc_nome, scales = "free_y") +
  labs(
    title = "Brasil - Evolução anual por exame oncológico (2022 a 2025)",
    x = "Ano",
    y = "Quantidade"
  ) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 9))

g_mg_exame <- ggplot(graf_mg_exame, aes(x = factor(ano), y = qtd_total)) +
  geom_col() +
  geom_text(aes(label = scales::comma(qtd_total, big.mark = ".", decimal.mark = ",")),
            vjust = -0.2, size = 2.8) +
  facet_wrap(~ proc_nome, scales = "free_y") +
  labs(
    title = "Minas Gerais - Evolução anual por exame oncológico (2022 a 2025)",
    x = "Ano",
    y = "Quantidade"
  ) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(size = 9))

print(g_br_exame)
print(g_mg_exame)

# =========================================================
# 11) GRÁFICOS DE RANKING 2025 - BRASIL E MG
# =========================================================

g_rank_br_2025 <- ggplot(
  rank_br_2025,
  aes(x = reorder(proc_nome, qtd_total), y = qtd_total)
) +
  geom_col() +
  geom_text(aes(label = scales::comma(qtd_total, big.mark = ".", decimal.mark = ",")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  labs(
    title = "Brasil - Ranking dos exames oncológicos realizados em 2025",
    x = NULL,
    y = "Quantidade"
  ) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.12))) +
  theme_minimal(base_size = 12)

g_rank_mg_2025 <- ggplot(
  rank_mg_2025,
  aes(x = reorder(proc_nome, qtd_total), y = qtd_total)
) +
  geom_col() +
  geom_text(aes(label = scales::comma(qtd_total, big.mark = ".", decimal.mark = ",")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  labs(
    title = "Minas Gerais - Ranking dos exames oncológicos realizados em 2025",
    x = NULL,
    y = "Quantidade"
  ) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.12))) +
  theme_minimal(base_size = 12)

print(g_rank_br_2025)
print(g_rank_mg_2025)

# =========================================================
# 12) SALVAR TABELAS
# =========================================================
fwrite(tab_onco_br_anual, "/Users/felipecotrim/Downloads/tab_onco_br_anual.csv", sep = ";", dec = ",", bom = TRUE)
fwrite(tab_onco_mg_anual, "/Users/felipecotrim/Downloads/tab_onco_mg_anual.csv", sep = ";", dec = ",", bom = TRUE)

fwrite(tab_onco_br_mensal_exame, "/Users/felipecotrim/Downloads/tab_onco_br_mensal_exame.csv", sep = ";", dec = ",", bom = TRUE)
fwrite(tab_onco_mg_mensal_exame, "/Users/felipecotrim/Downloads/tab_onco_mg_mensal_exame.csv", sep = ";", dec = ",", bom = TRUE)

fwrite(rank_br_2025, "/Users/felipecotrim/Downloads/rank_onco_br_2025.csv", sep = ";", dec = ",", bom = TRUE)
fwrite(rank_mg_2025, "/Users/felipecotrim/Downloads/rank_onco_mg_2025.csv", sep = ";", dec = ",", bom = TRUE)

# =========================================================
# 13) SALVAR GRÁFICOS
# =========================================================
ggsave("/Users/felipecotrim/Downloads/g_br_total_onco_2022_2025.png", g_br_total, width = 10, height = 6, dpi = 300)
ggsave("/Users/felipecotrim/Downloads/g_mg_total_onco_2022_2025.png", g_mg_total, width = 10, height = 6, dpi = 300)

ggsave("/Users/felipecotrim/Downloads/g_br_exame_onco_2022_2025.png", g_br_exame, width = 16, height = 10, dpi = 300)
ggsave("/Users/felipecotrim/Downloads/g_mg_exame_onco_2022_2025.png", g_mg_exame, width = 16, height = 10, dpi = 300)

ggsave("/Users/felipecotrim/Downloads/g_rank_br_onco_2025.png", g_rank_br_2025, width = 12, height = 7, dpi = 300)
ggsave("/Users/felipecotrim/Downloads/g_rank_mg_onco_2025.png", g_rank_mg_2025, width = 12, height = 7, dpi = 300)

cat("✅ Tabelas e gráficos de exames oncológicos salvos em Downloads.\n")
