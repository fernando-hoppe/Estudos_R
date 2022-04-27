#Aqui eu posso escrever o que eu quiser
#Isso aqui não altera o R e o código de forma alguna
# Mas para que eu vou escrever? Isso aqui é Word? Não, serve para me orientar

#Curso Intermediário de R - Aula 01 
#Código elaborado por Claudiomar Filho - 13/09/2021

#Primeiramente explicar o que é o Script editor, o Environment, o Console e o Panel Packages

####VARIÁVEIS####
#Exemplo de Operações no R

1+1
2-4
#Percebam que eu realizei as operações, mas isso não levou a nada.
#Foi como usar uma calculadora
#Vamos gravar as respostas?
a = 1+1
b=2-4
c=4*5

#As variáveis são como gaveteiros. Deixamos o valor guardado ali para 
#usar posteriormente
class(a)

####DATA-FRAMES####

a=2
b=3
a = c(2,3)


nomes = c("João",
          "Maria",
          "José")
idades = c(42,
           53,
           54)

#Criando um data-frame

planilha <- data.frame(nome=nomes,idade=idades)


#Para coluna, utilizamos mais o nome do dataframe com o símbolo "$"

planilha$idade

#INICIANDO A AULA

#Importando as bibliotecas

#install.packages("readxl)
library(readxl)
library(corrplot)
library(tidyr)

#Indicando qual é a pasta raiz

setwd("C:/Users/Claudiomar/Google Drive/Arquivos de trabalho/IDP/Data Science/Minicursos/R Intermediário")

#Importando os bancos de dados
ind_hom <- read_excel("ind_hom.xlsx")
#Inicialmente vamos analisar os dados
summary(ind_hom)
#Como é possível perceber, ainda que possamos ver os dados dos índices de 
#homicídio, eles estão salvos como "character" e não como números
#dessa forma não conseguimos realizar operações
#precisamos transformar a coluna para numérico 
#para isso, utilizamos a função as.numeric

ind_hom$indice <- as.numeric(ind_hom$indice)

#agora sim poderemos ver média, mediana, quadrantes...
summary(ind_hom)

#Se quisermos realizar algumas análises estatísticas separadas, também
#conseguimos por meio de funções específicas

mean(ind_hom$indice)
median(ind_hom$indice)
sd(ind_hom$indice)
var(ind_hom$indice)

#fazendo boxplots

boxplot(ind_hom$indice ~ ind_hom$regiao,
        xlab="Região", ylab="Taxa de Homicídios", main = "Taxa de homicídios no mundo")

#Calculando outliers
#Nesta seção vamos aprender o método baseado na amplitude interquartil.
#Vamos usar a sigla IQR para nos referenciar à amplitude interquartil
# Ela é a diferença entre os Quartis 1 e 3 do conjunto de dados.
# Para calcular os outliers, calculamos o limite dos outliers superiores
# e inferiores
#Quem estiver acima ou abaixo desse limite é considerado outlier
#Out_sup = Média  + 1,5 x IQR
#Out_inf = Média  - 1,5 x IQR

outliers <- boxplot(ind_hom$indice)$out

#CORRELAÇÃO
#(correlação é qualquer relação dentro de uma ampla classe de relações estatísticas 
#que envolva dependência entre duas variáveis)
#Inicialmente vamos carregar as bases de dados e armazená-las em variáveis

gini <- read_excel("dados.xlsx", sheet="gini")
ind_dem <- read_excel("dados.xlsx", sheet="democracia")
idade <- read_excel("dados.xlsx", sheet="idade")
gpd_pct <- read_excel("dados.xlsx", sheet="gpd_pct")

#Duas variáveis vieram com muito mais informação do que precisávamos
#vamos limpa-las

gini <- gini[,c(1,2)]
gpd_pct <- gpd_pct[,c(1,6)]

#Além delas, vamos limpar também o índice de homicídios

ind_hom <- ind_hom [,c(1,3)]

#Vamos proceder à correlação em si
#Inicialmente, iremos fundir todas variáveis em um só criando um tabelão

final <- merge (ind_hom,gini)
final <- merge (final,gpd_pct)
final <- merge (final,idade)
final <- merge (final,ind_dem)

#Checando se todos estão numéricos

summary(final)

#Gini, idade média e índice de democracia não estão númericos
#Vamos torná-los numéricos

final$gini <- as.numeric(final$gini)
final$idade_media <- as.numeric(final$idade_media)
final$indice_dem <- as.numeric(final$indice_dem)

summary(final)

#Criando a matriz de correlação

matriz <- cor(final[2:6], method = "pearson") #estamos pondo as colunas que importam 2 a 4

#Pib per capita possui NAs, o que atrapalha a matriz de correlação

final <-  drop_na (final)
summary(final)

#Agora sim fazendo a matriz

matriz <- cor(final[2:6], method = "pearson") #estamos pondo as colunas que importam 2 a 4
matriz <- round(matriz,2)

#Realizando uma matriz de correlação

corrplot(matriz)

#Customizando a matriz de correlação

corrplot(matriz, method="number")
corrplot(matriz, method="color", order = "alphabet")
corrplot(matriz, method="square")
corrplot(matriz, method="shade")
corrplot(matriz, method="ellipse")
corrplot.mixed(matriz)


#Tirando as duplicações e pondo só uma diagonal
corrplot(matriz, method="number", 
         type="upper", order="hclust",
         diag=FALSE)

#Podemos "girar as variáveis para melhor visualização
corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)

#Podemos retirar a diagonal do meio
corrplot(matriz, method="number", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45,
         diag=FALSE)

#Podemos "girar" toda matriz de correlação
corrplot(matriz, method="number", 
         type="lower", order="hclust",
         tl.col="black", tl.srt=45,
         diag=FALSE)



#Outras variações

plot (final$gini, final$indice)

#Próximo passo será explicado na aula do Lucas

