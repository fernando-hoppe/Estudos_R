## AULA 1 - R INTERMEDIÁRIO

# importando bibliotecas:
library(readxl)
library(corrplot)
library(tidyr)

## indicando pasta raiz:
setwd("C:/Users/hoppe/Documents/Mestrado/R_Intermediário")

# importando o banco de dados
ind_hom <- read_excel("ind_hom.xlsx")


# análise sumária
summary(ind_hom)
# perceba q o indica acabou ficando como character, o que ferra com a analise

# transformando variável em numérico:
ind_hom$indice <- as.numeric(ind_hom$indice)

summary(ind_hom)
# agora sim!

## se você quer simplesmente algumas estatísticas sumárias de uma variável:
mean(ind_hom$indice)
median(ind_hom$indice)
sd(ind_hom$indice)  #desvio padrão
var(ind_hom$indice)  #variância

## Fazendo boxplot
boxplot(ind_hom$indice ~ ind_hom$regiao,
        xlab="Região", ylab="Taxa de Homicídios", main = "Taxa de Homicídios no Mundo")
# boxplot: quadrado de cima: terceiro quartil
# quadrado de baixo: primeiro quartil
# linha do meio: mediana
# bolinhas: outliers, estão fora da variância
# outlier superior = Média + 1,5 x IQR
# outlier inferior = Média - 1,5 x IQR

# pra retirar outliers
outliers <- boxplot(ind_hom$indice)$out

# carregando outras variáveis q eventualmente estejam
# correlacionadas com homicídios

gini <- read_excel("dados.xlsx", sheet="gini")
ind_dem <- read_excel("dados.xlsx", sheet="democracia")
idade <- read_excel("dados.xlsx", sheet="idade")
gpd_pct <- read_excel("dados.xlsx", sheet="gpd_pct")

## variaveis gini e gdp vieram com mais informações do q as necessárias
## vamos portanto selecionar as colunas são uteis

gini <- gini[,c(1,2)]
gpd_pct <- gpd_pct[,c(1,6)]
# selecionamos as colunas 1 e 2; 1 e 6

# indice de homicidios tbm tem variáveis demais
ind_hom <- ind_hom[,c(1,3)]

## fundindo tudo numa tabela só:
final <- merge(ind_hom,gini)
final <- merge(final,gpd_pct)
final <- merge(final,idade)
final <- merge(final,ind_dem)

# lembrando q o nome de uma das colunas tem q ser igual pra haver a junção
# e o nome das unidades tem q ser igual pra haver o match
# Fazendo o merge ele não pega as observações com NA (omissão)
# pra pegar as omissões teria q ser merge(ind_hom,gini, all=TRUE)

# checando se estão numéricos
summary(final)

# algumas estão como caracter, tem q transformar em númerico:
final$gini <- as.numeric(final$gini)
final$idade_media <- as.numeric(final$idade_media)
final$indice_dem <- as.numeric(final$indice_dem)

summary(final)

## criando a matriz de correlação

matriz <- cor(final[2:6], method = "pearson")
## perceba que não dá certo pq a variável gpd possui omissões
## método de pearson serve pra duas variáveis quantitativas numéricas
## Outros tipos de coeficientes: Taxa de correlação e coeficiente de Cramer
## Taxa de correlação: correlação entre variáveis numéricas e categóricas
## Coeficiente de Cramer: correlação entre var categóricas

# retirando NAs:
final <- drop_na(final)
summary(final)

## Agora sim crianco a matriz de correlação
matriz <- cor(final[2:6], method = "pearson")
matriz <- round(matriz,2)
## aí arredondou pra duas casas decimais.

# da pra ver q quanto maior o gini maior é o indice de homicidios
# ou seja, maior a concentração maior a violencia

## agora fazendo uma figura pra representar essa matriz de correlação
corrplot((matriz))

# customizando:
corrplot(matriz, method="number")
corrplot(matriz, method="color", order="alphabet")
corrplot(matriz, method="square")
corrplot(matriz, method="shade")
corrplot(matriz, method="ellipse")
corrplot.mixed(matriz)

## tirando as duplicações e pondo só uma diagonal
corrplot(matriz, method="number", type="upper", order="hclust")

## Podemos girar as variáveis pra melhor visualização:
corrplot(matriz, method = "number",
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)

## Podemos retirar a diagonal do meio q é 1
corrplot(matriz, method = "number",
         type="upper", order="hclust",
         tl.col="black", tl.srt=45,
         diag=FALSE)

## Pode girar tudo, também:
corrplot(matriz, method = "number",
         type="lower", order="hclust",
         tl.col="black", tl.srt=45,
         diag=FALSE)


# plotando no gráfico os índices de homicídio pelo gini
plot(final$gini, final$indice)
# perceba q países mais desiguais tem maior indice de homicidio

