#### APLICAÇÃO DE R NA ANÁLISE DE DADOS ####

### Análise de dados sobre viagens a serviço para medidas de redução dos gastos

#Qual é o valor gasto por órgão?
#Qual é o valor gasto por cidade?
#Qual é a quantidade de viagens por mês?

setwd("C:/Users/hoppe/Documents/Mestrado/Análise Dados R_ENAP")

viagens <- read.csv(
  file = "C:/Users/hoppe/Documents/Mestrado/Análise Dados R_ENAP/2019_Viagem.csv",
  sep = ';',
  dec = ','
)

head(viagens)
View(viagens)

#Verificar dimensões do dataset
dim(viagens)

#Resumo do dataset - valores min, max, media, mediana...
summary(viagens)

#Summary de uma coluna especifica
summary(viagens$Valor.passagens)

#Verificar tipo dos dados
library(dplyr)

glimpse(viagens)


### TRANSFORMAÇÃO DOS DADOS OBTIDOS ###
#Convertendo o tipo do dato para tipo Date
viagens$data.inicio <- as.Date(viagens$Período...Data.de.início, "%d/%m/%Y")

glimpse(viagens)

#Formatando a data de inicio - para utilizar apenas Ano/Mês
viagens$data.inicio.formatada <- format(viagens$data.inicio, "%Y-%m")
viagens$data.inicio.formatada


### EXPLORAÇÃO DOS DADOS ###

#Gerando histograma da coluna passagens
hist(viagens$Valor.passagens)

#Filtrando os valores das passagens - apenas passagens entre 200 e 5000
passagens_fitro <- viagens %>%
  select(Valor.passagens) %>%
  filter(Valor.passagens >= 200 & Valor.passagens <= 5000)

passagens_fitro
hist(passagens_fitro$Valor.passagens)

#Verificando os valores min, max, média... da coluna valor
summary(viagens$Valor.passagens)

#Visualizando os valores em um boxplot
boxplot(viagens$Valor.passagens)

#Visualizando os valores das passagens - filtro de 200 a 5000
boxplot(passagens_fitro$Valor.passagens)

#Calculando o desvio padrão
sd(viagens$Valor.passagens)


#Verificar se existem valores não preenchidos nas colunas do dataframe
colSums(is.na(viagens))

#Verifcar a quantidade de categorias da coluna Situação (character)
#Converter para factor
viagens$Situação <- factor(viagens$Situação)
str(viagens$Situação)

#Verificar quantidade de registros em cada categoria
table(viagens$Situação)

#Obtendo os valores em percentual de cada categoria
prop.table(table(viagens$Situação))*100


### Visualização dos resultados ###

# 1 - Qual é o valor gasto por órgão em passagens?
#Criando um dataframe com os 15 órgãos que gastam mais

library(dplyr)
p1 <- viagens %>%
  group_by(Nome.do.órgão.superior) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>% #arrange(desc(n)): pra colocar em ordem decrescente de n
  top_n(15)

#Alterando o nome das colunas
names(p1) <- c("orgao", "valor")

# Questão 1 respondida
p1

#Plotando os dados com o ggplot
library(ggplot2)
ggplot(p1, aes(x = reorder(orgao, valor), y = valor))+ #reorder é para ordenar os valores do eixo x
  geom_bar(stat = "identity")+
  coord_flip()+ #muda a orientação do gráfico
  labs(x = "Valor", y = "Órgãos")


# 2 - Qual é o valor gasto por cidade?
#Criando um dataframe com as 15 cidades que gastam mais
p2 <- viagens %>%
  group_by(Destinos) %>%
  summarise(n = sum(Valor.passagens)) %>%
  arrange(desc(n)) %>%
  top_n(15)

p2

#Alterando o nome das colunas
names(p2) <- c("destino", "valor")
p2

#Criando o gráfico
ggplot(p2, aes(x = reorder(destino, valor), y = valor))+
  geom_bar(stat = "identity", fill = "#0ba791")+
  geom_text(aes(label = valor), vjust = 0.3, size = 3)+ #label insere aí retangulos no fundo pra facilitar a compreensão
  coord_flip()+
  labs(x = "Valor", y = "Destino")

options(scipen = 999)

# 3 - Qual é a quantidade de viagens por mês?

#Criando um dataframe com a quantidade de viagens por Ano/mês
p3 <- viagens %>%
  group_by(data.inicio.formatada) %>%
  summarise(qtd = n_distinct(Identificador.do.processo.de.viagem))
# n_distinct é pra contar apenas os identificadores distintos das viagens
# É pra impedir q uma mesma viagem seja contabilizada duas vezes

head(p3)

#Criando o gráfico
ggplot(p3, aes(x = data.inicio.formatada, y = qtd, group = 1))+
  geom_line()+
  geom_point()




#Exemplo de utilização do R markdown

#Na linguagem R é possível usar o Markdown para formatar os seus relatórios


#Instalação do Rmarkdown
install.packages("rmarkdown")
install.packages('tinytex')
library(tinytex)

#esta linha poderá levar alguns minutos para terminar a execução
#acompanhe o progresso na aba console e aguarde a instalação ser finalizada
tinytex::install_tinytex()


# Próximos passos

# Criar um arquivo R Markdown: File > New File RMarkdown
# Criar script
# Gerar Relatório: Knit > Knit to PDF