#Aqui eu posso escrever o que eu quiser
#Isso aqui n�o altera o R e o c�digo de forma alguna
# Mas para que eu vou escrever? Isso aqui � Word? N�o, serve para me orientar

#Curso Intermedi�rio de R - Aula 01 
#C�digo elaborado por Claudiomar Filho - 13/09/2021

#Primeiramente explicar o que � o Script editor, o Environment, o Console e o Panel Packages

####VARI�VEIS####
#Exemplo de Opera��es no R

1+1
2-4
#Percebam que eu realizei as opera��es, mas isso n�o levou a nada.
#Foi como usar uma calculadora
#Vamos gravar as respostas?
a = 1+1
b=2-4
c=4*5

#As vari�veis s�o como gaveteiros. Deixamos o valor guardado ali para 
#usar posteriormente
class(a)

####DATA-FRAMES####

a=2
b=3
a = c(2,3)


nomes = c("Jo�o",
          "Maria",
          "Jos�")
idades = c(42,
           53,
           54)

#Criando um data-frame

planilha <- data.frame(nome=nomes,idade=idades)


#Para coluna, utilizamos mais o nome do dataframe com o s�mbolo "$"

planilha$idade

#INICIANDO A AULA

#Importando as bibliotecas

#install.packages("readxl)
library(readxl)
library(corrplot)
library(tidyr)

#Indicando qual � a pasta raiz

setwd("C:/Users/Claudiomar/Google Drive/Arquivos de trabalho/IDP/Data Science/Minicursos/R Intermedi�rio")

#Importando os bancos de dados
ind_hom <- read_excel("ind_hom.xlsx")
#Inicialmente vamos analisar os dados
summary(ind_hom)
#Como � poss�vel perceber, ainda que possamos ver os dados dos �ndices de 
#homic�dio, eles est�o salvos como "character" e n�o como n�meros
#dessa forma n�o conseguimos realizar opera��es
#precisamos transformar a coluna para num�rico 
#para isso, utilizamos a fun��o as.numeric

ind_hom$indice <- as.numeric(ind_hom$indice)

#agora sim poderemos ver m�dia, mediana, quadrantes...
summary(ind_hom)

#Se quisermos realizar algumas an�lises estat�sticas separadas, tamb�m
#conseguimos por meio de fun��es espec�ficas

mean(ind_hom$indice)
median(ind_hom$indice)
sd(ind_hom$indice)
var(ind_hom$indice)

#fazendo boxplots

boxplot(ind_hom$indice ~ ind_hom$regiao,
        xlab="Regi�o", ylab="Taxa de Homic�dios", main = "Taxa de homic�dios no mundo")

#Calculando outliers
#Nesta se��o vamos aprender o m�todo baseado na amplitude interquartil.
#Vamos usar a sigla IQR para nos referenciar � amplitude interquartil
# Ela � a diferen�a entre os Quartis 1 e 3 do conjunto de dados.
# Para calcular os outliers, calculamos o limite dos outliers superiores
# e inferiores
#Quem estiver acima ou abaixo desse limite � considerado outlier
#Out_sup = M�dia  + 1,5 x IQR
#Out_inf = M�dia  - 1,5 x IQR

outliers <- boxplot(ind_hom$indice)$out

#CORRELA��O
#(correla��o � qualquer rela��o dentro de uma ampla classe de rela��es estat�sticas 
#que envolva depend�ncia entre duas vari�veis)
#Inicialmente vamos carregar as bases de dados e armazen�-las em vari�veis

gini <- read_excel("dados.xlsx", sheet="gini")
ind_dem <- read_excel("dados.xlsx", sheet="democracia")
idade <- read_excel("dados.xlsx", sheet="idade")
gpd_pct <- read_excel("dados.xlsx", sheet="gpd_pct")

#Duas vari�veis vieram com muito mais informa��o do que precis�vamos
#vamos limpa-las

gini <- gini[,c(1,2)]
gpd_pct <- gpd_pct[,c(1,6)]

#Al�m delas, vamos limpar tamb�m o �ndice de homic�dios

ind_hom <- ind_hom [,c(1,3)]

#Vamos proceder � correla��o em si
#Inicialmente, iremos fundir todas vari�veis em um s� criando um tabel�o

final <- merge (ind_hom,gini)
final <- merge (final,gpd_pct)
final <- merge (final,idade)
final <- merge (final,ind_dem)

#Checando se todos est�o num�ricos

summary(final)

#Gini, idade m�dia e �ndice de democracia n�o est�o n�mericos
#Vamos torn�-los num�ricos

final$gini <- as.numeric(final$gini)
final$idade_media <- as.numeric(final$idade_media)
final$indice_dem <- as.numeric(final$indice_dem)

summary(final)

#Criando a matriz de correla��o

matriz <- cor(final[2:6], method = "pearson") #estamos pondo as colunas que importam 2 a 4

#Pib per capita possui NAs, o que atrapalha a matriz de correla��o

final <-  drop_na (final)
summary(final)

#Agora sim fazendo a matriz

matriz <- cor(final[2:6], method = "pearson") #estamos pondo as colunas que importam 2 a 4
matriz <- round(matriz,2)

#Realizando uma matriz de correla��o

corrplot(matriz)

#Customizando a matriz de correla��o

corrplot(matriz, method="number")
corrplot(matriz, method="color", order = "alphabet")
corrplot(matriz, method="square")
corrplot(matriz, method="shade")
corrplot(matriz, method="ellipse")
corrplot.mixed(matriz)


#Tirando as duplica��es e pondo s� uma diagonal
corrplot(matriz, method="number", 
         type="upper", order="hclust",
         diag=FALSE)

#Podemos "girar as vari�veis para melhor visualiza��o
corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)

#Podemos retirar a diagonal do meio
corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45,
         diag=FALSE)

#Podemos "girar" toda matriz de correla��o
corrplot(matriz, method="number", 
         type="lower", order="hclust",
         tl.col="black", tl.srt=45,
         diag=FALSE)



#Outras varia��es

plot (final$gini, final$indice)

#Pr�ximo passo ser� explicado na aula do Lucas

