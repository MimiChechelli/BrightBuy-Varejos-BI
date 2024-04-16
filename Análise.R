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

  
#IV. Calculo do ticket médio por filial, Churn Rate por vendedor/filial/geral, Lifetime Value (LTV) dos 
#Cliente individualmente e em média, Taxa de Recorrência e Margens de Lucro.










'''Análise de dados: 
II. Indicação de cinco (5) famílias de produtos para focar nas campanhas de marketing em dezembro, 
apartir do histórico e tendencias de fim de ano.

III. Avaliação de desempenho de vendedores apartir da analise do número de produtos vendidos, 
número de clientes, ticket médio e receita e lucro gerada. 5 melhores vendedores do ano, comparação 
grafica geral e tratativa para melhorar as vendas dos vendedores com desempenho abaixo da média.'''

