library(readxl)
library(dplyr)

# Ler e montar planilha de trabalho
vendas <- read_excel('fato_vendas.xlsx')
familia <- read_excel('dim_familia_produtos.xlsx')
vendedor <- read_excel('dim_vendedor.xlsx')
produtos <- read_excel('dim_produtos.xlsx')

dados <- merge(vendas, vendedor, by="codigo_vendedor")
dados <- merge(dados, produtos, by="codigo_produto")
dados <- merge(dados, familia, by="codigo_familia")

dados$lucro <- ((dados$preco_venda_unitario - dados$custo_produto_unitario) * dados$quantidade) - dados$valor_desconto

# Total vendas ao longo do ano:
total_monetario_anual <- sum(dados$valor_monetario_total)
total_produtos_anual <- sum(dados$quantidade)
total_lucro_anual <- sum(dados$lucro)

# Total vendas por mês
dados <- mutate(dados, Mes = format(data_venda, "%Y-%m"))

monetario_por_mes <- dados %>%
  group_by(Mes) %>%
  summarise(Total= sum(valor_monetario_total))

produtos_por_mes <- dados %>%
  group_by(Mes) %>%
  summarise(Total= sum(quantidade))

lucro_por_mes <- dados %>%
  group_by(Mes) %>%
  summarise(Total= sum(dados$lucro))

# Total vendas por semana
dados <- mutate(dados, Semana = format(data_venda, "%Y-%U"))

monetario_por_semana <- dados %>%
  group_by(Semana) %>%
  summarise(Total= sum(valor_monetario_total))

produtos_por_semana <- dados %>%
  group_by(Semana) %>%
  summarise(Total= sum(quantidade))

lucro_por_semana <- dados %>%
  group_by(Semana) %>%
  summarise(Total= sum(lucro))

# Total vendas por filial
monetario_por_filial <- dados %>%
  group_by(filial_venda) %>%
  summarise(Total= sum(valor_monetario_total))

monetario_por_filial_mes <- dados %>%
  group_by(filial_venda, Mes) %>%
  summarise(Total= sum(valor_monetario_total))

monetario_por_filial_semana <- dados %>%
  group_by(filial_venda, Semana) %>%
  summarise(Total= sum(valor_monetario_total))

produtos_por_filial <- dados %>%
  group_by(filial_venda) %>%
  summarise(Total= sum(quantidade))

produtos_por_filial_mes <- dados %>%
  group_by(filial_venda, Mes) %>%
  summarise(Total= sum(quantidade))

produtos_por_filial_semana <- dados %>%
  group_by(filial_venda, Semana) %>%
  summarise(Total= sum(quantidade))

lucro_por_filial <- dados %>%
  group_by(filial_venda) %>%
  summarise(Total= sum(lucro))

lucro_por_filial_mes <- dados %>%
  group_by(filial_venda, Mes) %>%
  summarise(Total= sum(lucro))

lucro_por_filial_semana <- dados %>%
  group_by(filial_venda, Semana) %>%
  summarise(Total= sum(lucro))

# Total vendas por vendedor
monetario_por_vendedor <- dados %>%
  group_by(codigo_vendedor) %>%
  summarise(Total= sum(valor_monetario_total))

produtos_por_vendedor <- dados %>%
  group_by(codigo_vendedor) %>%
  summarise(Total= sum(quantidade))

lucro_por_vendedor <- dados %>%
  group_by(codigo_vendedor) %>%
  summarise(Total= sum(lucro))

monetario_por_vendedor_mes <- dados %>%
  group_by(codigo_vendedor, Mes) %>%
  summarise(Total= sum(valor_monetario_total))

produtos_por_vendedor_mes <- dados %>%
  group_by(codigo_vendedor, Mes) %>%
  summarise(Total= sum(quantidade))

lucro_por_vendedor_mes <- dados %>%
  group_by(codigo_vendedor, Mes) %>%
  summarise(Total= sum(lucro))

monetario_por_vendedor_semana <- dados %>%
  group_by(codigo_vendedor, Semana) %>%
  summarise(Total= sum(valor_monetario_total))

produtos_por_vendedor_semana <- dados %>%
  group_by(codigo_vendedor, Semana) %>%
  summarise(Total= sum(quantidade))

lucro_por_vendedor_semana <- dados %>%
  group_by(codigo_vendedor, Semana) %>%
  summarise(Total= sum(lucro))

# Total vendas por familia de produtos
monetario_por_fam_produto <- dados %>%
  group_by(codigo_familia) %>%
  summarise(Total= sum(valor_monetario_total))

produtos_por_fam_produto <- dados %>%
  group_by(codigo_familia) %>%
  summarise(Total= sum(quantidade))

lucro_por_fam_produto <- dados %>%
  group_by(codigo_familia) %>%
  summarise(Total= sum(lucro))

  monetario_por_fam_produto_mes <- dados %>%
  group_by(codigo_familia, Mes) %>%
  summarise(Total= sum(valor_monetario_total))

produtos_por_fam_produto_mes <- dados %>%
  group_by(codigo_familia, Mes) %>%
  summarise(Total= sum(quantidade))

lucro_por_fam_produto_mes <- dados %>%
  group_by(codigo_familia, Mes) %>%
  summarise(Total= sum(lucro))

monetario_por_fam_produto_semana <- dados %>%
  group_by(codigo_familia, Semana) %>%
  summarise(Total= sum(valor_monetario_total))

produtos_por_fam_produto_semana <- dados %>%
  group_by(codigo_familia, Semana) %>%
  summarise(Total= sum(quantidade))

lucro_por_fam_produto_semana <- dados %>%
  group_by(codigo_familia, Semana) %>%
  summarise(Total= sum(lucro))

# Calculo do ticket médio geral anual 
n_vendas_anuais <- nrow(dados)
ticket_medio_anual_geral <- total_monetario_anual/n_vendas_anuais

# Calculo do ticket médio geral mensal
ticket_medio_mensal_geral <- data.frame(
  Mes = unique(dados$Mes),
  Ticket = numeric(length(unique(dados$Mes)))
)

for (i in seq_along(ticket_medio_mensal_geral$Mes)) {
  mes <- ticket_medio_mensal_geral$Mes[i]
  dados_mes_x <- dados[dados$Mes == mes, ]
  valor_monetario <- monetario_por_mes$Total[monetario_por_mes$Mes == mes] 
  ticket_medio_mensal_geral$Ticket[i] <- valor_monetario / nrow(dados_mes_x)
}

# Calculo do ticket médio geral semanal
ticket_medio_semanal_geral <- data.frame(
  Semana = unique(dados$Semana),
  Ticket = numeric(length(unique(dados$Semana)))
)

for (i in seq_along(ticket_medio_semanal_geral$Semana)) {
  semana <- ticket_medio_semanal_geral$Semana[i]
  dados_semana_x <- dados[dados$Semana == semana, ]
  valor_monetario <- monetario_por_semana$Total[monetario_por_semana$Semana == semana]
  ticket_medio_semanal_geral$Ticket[i] <- valor_monetario / nrow(dados_semana_x)
}

# Para calcular o ticket médio anual por filial
ticket_medio_anual_por_filial <- dados %>%
  group_by(filial_venda) %>%
  summarize(ticket_medio = mean(valor_monetario_total))

# Para calcular o ticket médio mensal por filial
ticket_medio_mensal_por_filial <- dados %>%
  group_by(filial_venda, Mes) %>%
  summarize(ticket_medio = mean(valor_monetario_total))

# Para calcular o ticket médio semanal por filial
ticket_medio_semanal_por_filial <- dados %>%
  group_by(filial_venda, Semana) %>%
  summarize(ticket_medio = mean(valor_monetario_total))

# Para calcular o ticket médio anual por vendedor
ticket_medio_anual_por_vendedor <- dados %>%
  group_by(nome_vendedor) %>%
  summarize(ticket_medio = mean(valor_monetario_total))

# Para calcular o ticket médio mensal por vendedor
ticket_medio_mensal_por_vendedor <- dados %>%
  group_by(nome_vendedor, Mes) %>%
  summarize(ticket_medio = mean(valor_monetario_total))

# Para calcular o ticket médio semanal por vendedor
ticket_medio_semanal_por_vendedor <- dados %>%
  group_by(nome_vendedor, Semana) %>%
  summarize(ticket_medio = mean(valor_monetario_total))

# Margens de margem de lucro geral
margem_de_lucro_geral <- dados %>%
  filter(!is.na(lucro) & !is.na(valor_monetario_total) & valor_monetario_total != 0) %>%
  summarize(margem_lucro = mean(lucro / valor_monetario_total) * 100)

# Calcular as margens de lucro por filial
margens_de_lucro_por_filial <- dados %>%
  filter(!is.na(lucro) & !is.na(valor_monetario_total) & valor_monetario_total != 0) %>%
  group_by(filial_venda) %>%
  summarize(margem_lucro = mean(lucro / valor_monetario_total) * 100)

# Calcular as margens de lucro por vendedor
margens_de_lucro_por_vendedor <- dados %>%
  filter(!is.na(lucro) & !is.na(valor_monetario_total) & valor_monetario_total != 0) %>%
  group_by(nome_vendedor) %>%
  summarize(margem_lucro = mean(lucro / valor_monetario_total) * 100)

# Calcular o LTV dos Clientes Individualmente
lifetime_Value_individual <- dados %>%
  group_by(codigo_cliente) %>%
  summarise(ltv = sum(valor_monetario_total))

# Calcular o LTV Médio dos Clientes
lifetime_Value_medio <- mean(ltv_individual$ltv)

# Número de compras por cliente
numero_de_compras <- dados %>%
  group_by(codigo_cliente) %>%
  summarise(numero_de_compras = n())

