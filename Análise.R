# Ler e montar planilha de trabalho
library(readxl)
vendas <- read_excel('fato_vendas.xlsx')
familia <- read_excel('dim_familia_produtos.xlsx')
vendedor <- read_excel('dim_vendedor.xlsx')
produtos <- read_excel('dim_produtos.xlsx')

dados <- merge(vendas, vendedor, by="codigo_vendedor")
dados <- merge(dados, produtos, by="codigo_produto")
dados <- merge(dados, familia, by="codigo_familia")




'''Análise de dados: 
I. Relatório de vendas ao longo do ano, por mês e por semana.
total R$, n e lucro vendas
produtos por filial, vendedor
total R$, n e lucro por vendedor, filial, produtos, familia de produtos
no ano todo, por mes e por semana

II. Indicação de cinco (5) famílias de produtos para focar nas campanhas de marketing em dezembro, 
apartir do histórico e tendencias de fim de ano.

III. Avaliação de desempenho de vendedores apartir da analise do número de produtos vendidos, 
número de clientes, ticket médio e receita e lucro gerada. 5 melhores vendedores do ano, com paração 
grafica geral e tratativa para melhorar as vendas dos vendedores com desempenho abaixo da média.

IV. Calculo do ticket médio por filial, Churn Rate por vendedor/filial/geral, Lifetime Value (LTV) dos 
Cliente individualmente e em média, Taxa de Recorrência e Margens de Lucro.'''














