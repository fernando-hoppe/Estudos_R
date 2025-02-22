## loops e fun��es

# abrindo v�rios arquivos de uma vez com um loop
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(data.table)

## considerando q eu tenha um arquivo PEP1999 at� PEP2021, poderia
## utilizar o seguinte comando para ler o primeiro arquivo:
PEP1999 <- read_excel("localiza��o_do_arquivo")

## ao inves de repetir o comando 22 vezes eu poderia fazer um loop
## sintaxe b�sica: for(CONDI��O){COMANDO OU LISTA DE COMANDOS}

## No nosso caso a condi��o seria a localiza��o do arquivo no endere�o
## onde est�o os arquivos e o comando seria ler o arquivo

## para a realiza��o desse loop v�o ser necess�rios 5 comandos:
## list.files ; substring; lenght; assign e paste

# list.files: coleta os nomes de todos os arquivos em um dado endere�o
arquivos <- list.files(path = "localiza��o", all.files = FALSE)

## O "arquivos <-" serve pra criar um vetor que agrupa todos os arquivos
## all.files serve para evitar que d� problema caso os arquivos tenham
## sido mexidos

## para evitar que os arquivos apare�am com o .xlsx no final:
## usa-se o comando substring, selecionado os caracteres q permanecem

nomes <- substring(arquivos, 1,8)
# seleciona os caracteres do 1 ao 8 de cada nome de arquivo.


## o comando lenght retorna o n�mero de elementos de algum vetor/lista/objeto
## se fizermos lenght(arquivo) retornaria 23.

## loop:
for(i in 1:lenght(arquivos)){assign(nomes[i],read_excel(paste("localiza��o", arquivos[i],sep="")))}
## for(i in 1:lenght) � o mesmo que dizer de 1 a 23. Essa vai ser a condi��o: de q cada arquivo t� entre 1 e 23
## comandos v�o ser assign e paste

# paste � colar, concatenar. o sep="" serve para for�ar para que n�o haja separa��o entre estes arquivos
## note que no c�digo � paste("localiza��o", arquivos{i], sep...} ou seja: ele vai concatenar essa localiza��o
## com o i �simo arquivo, ele vai retornar o endere�o certinho de cada um dos arquivos

## assign vai associar valores � objetos, assign(nomes[1],...) vai associar o primeiro elemento do vetor nomes
## ao dataframe obtido com a leitura da primeira planilha.


## o for vai repetir esse comando para todos os 23 elementos. A� v�o aparecer os 23 arq no environment



## fun��es
## sintaxa b�sica: 
nome.da.fun��o <- function(argumentos)
{c�lculos baseados nos argumentos
  outros comandos}

## argumento s�o os valores para os quais vc quer q a fun��o funcione
# exemplo:
filtragem <- function(i,j){filter(pinhais, genero==i & idade==j)}

## esse comando vai filtrar o dataframe pinhais pelas vari�veis i e j




## rotina para simula��o
library(tidyverse)
library(readr)
library(dplyr)
library(foreach)

# acho que o foreach n�o est� instalado.

# primeiro passo � puxar os dados, no caso do prof, dados de servidores publicos do amapa (RAIS):
amapa <- read.csv2("localiza��o")

str(amapa)
## vai mostrar mais ou menos como os dados estao dispostos

## para substituir valores (p.ex. 1 e 2 por M e F)
amapa$genero <- gsub(2,"F", amapa$genero)
amapa$genero <- gsub(1,"M", amapa$genero)

## pra mudar o nome de uma coluna:
names(amapa)[3] <- "idade"
# vai no dataframe amapa, terceira coluna e muda o nome pra idades

## a� ele calcula a expectativa do numero de obitos em 2020 a partir da tabua de mortalidade de beltr�o
# primero puxa as tabuas:
tabua_m <- read.csv2("localiza��o")
tabua_h <- read.csv2("localiza��o")

#a� adiciona a coluna genero nas tabuas, j� q uma � pra mulher outra pra homem:
tabua_h$genero <- "M"
tabua_m$genero <- "F"

# a� junta as duas tabuas:
tabua <- rbind(tabua_h,tabua_m)

# a� apaga as tabuas inutilizadas
rm(tabua_h,tabua_m)

# a� seleciona apenas as variaveis que se quer trabalhar:
tabua <- select(tabua, genero, idade, mortal_0)

## a� vou agrupar os servidores por idade e genero:
amapa_agrup <- grup_by(amapa, idade, genero)
amapa_agrup <- summarise(amapa_agrup, qtde= n())
# a� ele vai dar a qtde de observa��es por cada uma dessas especifica��es de idade e genero

## agora pra juntar a tabua de mortalidade com o numero de observa��es por especifica��o
amapa_agrup <- left_join(amapa_agrup, tabua,by.x = idade, by.y = genero)
## vai agrupar por essas variaveis escolhidas, que s�o comuns as duas tabuas

## na tabela criada nos temos os grupos de genero por idade e a probabilidade de 1 individuo nesse grupo
## vier a �bito. E se quisermos saber a probalidade total do grupo? Fun��o binomial

# cria-se os argumentos da fun��o
c <- amapa_agrup$qtde
d <- amapa_agrup$mortal_0
# c vai ser a quantidade total de individuos em cada grupo; d a taxa de mortalidade individual por grupo

# criando a fun��o:
sintetico <- function(n,y)[set.seed(124)
                           rbinom(1, n, y)]

# agora para criar a fun��o para todos os grupos deve-se usar um loop, nesse caso, foreach
amapa_agrup$mortos <- foreach(i = c, j = d) %do% {sintetico(i,j)}
## ou seja, pra cada i e pra cada j se roda o sintetico, q � a fun��o acima
## a� se criou uma coluna chamada mortos na tabua.

# a� ele coloca esses novos valores em numero, pq eventualmente o foreach coloca outro caractere
amapa_agrup$mortos <- as.numeric(amapa_agrup$mortos)

# e a� eu pe�o pra me dar o somat�rio de mortos:
mortos_est <- sum(amapa_agrup$mortos)


## simulando a morte de pessoas em um dado ano (outro loop e fun��o)
# criando argumentos:
e <- amapa_agrup$genero
f <- amapa_agrup$idade
h <- 1:nrow(amapa_agrup)
# esse ultimo pega a quantidade de agrupamentos

# a� se cria uma lista vazia
df_list <- list()

# se cria a fun��o de filtro para genero e idade
filtering <- function(i,j){filter(amapa, genero==i & idade==j)}

## a� pra preencher a lista, usa-se o loop:
df_list <- foreach(i = e, j = f) %do% (filtering(i,j))

## agora mais uma fun��o pra ver a quantidade de obitos
obitos <- function(i) {if(amapa_agrup$mortos[i]==0) {
  df_list[[i]]}} else {head(df_list[[i]], amapa_agrup$mortos[i])}}
## essa fun��o vai tirar o numero de mortos de cada grupo de especifica��o

## o jeito mais facil � fazer o loop
df_list <- foreach(i = h) %do% {obitos(i)}

## agora criando um novo dataframe excluindo os que vieram � obito
amapa_sobrev <- rbindlist(df_list)

## pra identificar onde est�o os mortos
mortos <- setdiff(amapa, amapa_sobrev)





