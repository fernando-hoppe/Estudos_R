## loops e funções

# abrindo vários arquivos de uma vez com um loop
library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(data.table)

## considerando q eu tenha um arquivo PEP1999 até PEP2021, poderia
## utilizar o seguinte comando para ler o primeiro arquivo:
PEP1999 <- read_excel("localização_do_arquivo")

## ao inves de repetir o comando 22 vezes eu poderia fazer um loop
## sintaxe básica: for(CONDIÇÃO){COMANDO OU LISTA DE COMANDOS}

## No nosso caso a condição seria a localização do arquivo no endereço
## onde estão os arquivos e o comando seria ler o arquivo

## para a realização desse loop vão ser necessários 5 comandos:
## list.files ; substring; lenght; assign e paste

# list.files: coleta os nomes de todos os arquivos em um dado endereço
arquivos <- list.files(path = "localização", all.files = FALSE)

## O "arquivos <-" serve pra criar um vetor que agrupa todos os arquivos
## all.files serve para evitar que dê problema caso os arquivos tenham
## sido mexidos

## para evitar que os arquivos apareçam com o .xlsx no final:
## usa-se o comando substring, selecionado os caracteres q permanecem

nomes <- substring(arquivos, 1,8)
# seleciona os caracteres do 1 ao 8 de cada nome de arquivo.


## o comando lenght retorna o número de elementos de algum vetor/lista/objeto
## se fizermos lenght(arquivo) retornaria 23.

## loop:
for(i in 1:lenght(arquivos)){assign(nomes[i],read_excel(paste("localização", arquivos[i],sep="")))}
## for(i in 1:lenght) é o mesmo que dizer de 1 a 23. Essa vai ser a condição: de q cada arquivo tá entre 1 e 23
## comandos vão ser assign e paste

# paste é colar, concatenar. o sep="" serve para forçar para que não haja separação entre estes arquivos
## note que no código é paste("localização", arquivos{i], sep...} ou seja: ele vai concatenar essa localização
## com o i ésimo arquivo, ele vai retornar o endereço certinho de cada um dos arquivos

## assign vai associar valores à objetos, assign(nomes[1],...) vai associar o primeiro elemento do vetor nomes
## ao dataframe obtido com a leitura da primeira planilha.


## o for vai repetir esse comando para todos os 23 elementos. Aí vão aparecer os 23 arq no environment



## funções
## sintaxa básica: 
nome.da.função <- function(argumentos)
{cálculos baseados nos argumentos
  outros comandos}

## argumento são os valores para os quais vc quer q a função funcione
# exemplo:
filtragem <- function(i,j){filter(pinhais, genero==i & idade==j)}

## esse comando vai filtrar o dataframe pinhais pelas variáveis i e j




## rotina para simulação
library(tidyverse)
library(readr)
library(dplyr)
library(foreach)

# acho que o foreach não está instalado.

# primeiro passo é puxar os dados, no caso do prof, dados de servidores publicos do amapa (RAIS):
amapa <- read.csv2("localização")

str(amapa)
## vai mostrar mais ou menos como os dados estao dispostos

## para substituir valores (p.ex. 1 e 2 por M e F)
amapa$genero <- gsub(2,"F", amapa$genero)
amapa$genero <- gsub(1,"M", amapa$genero)

## pra mudar o nome de uma coluna:
names(amapa)[3] <- "idade"
# vai no dataframe amapa, terceira coluna e muda o nome pra idades

## aí ele calcula a expectativa do numero de obitos em 2020 a partir da tabua de mortalidade de beltrão
# primero puxa as tabuas:
tabua_m <- read.csv2("localização")
tabua_h <- read.csv2("localização")

#aí adiciona a coluna genero nas tabuas, já q uma é pra mulher outra pra homem:
tabua_h$genero <- "M"
tabua_m$genero <- "F"

# aí junta as duas tabuas:
tabua <- rbind(tabua_h,tabua_m)

# aí apaga as tabuas inutilizadas
rm(tabua_h,tabua_m)

# aí seleciona apenas as variaveis que se quer trabalhar:
tabua <- select(tabua, genero, idade, mortal_0)

## aí vou agrupar os servidores por idade e genero:
amapa_agrup <- grup_by(amapa, idade, genero)
amapa_agrup <- summarise(amapa_agrup, qtde= n())
# aí ele vai dar a qtde de observações por cada uma dessas especificações de idade e genero

## agora pra juntar a tabua de mortalidade com o numero de observações por especificação
amapa_agrup <- left_join(amapa_agrup, tabua,by.x = idade, by.y = genero)
## vai agrupar por essas variaveis escolhidas, que são comuns as duas tabuas

## na tabela criada nos temos os grupos de genero por idade e a probabilidade de 1 individuo nesse grupo
## vier a óbito. E se quisermos saber a probalidade total do grupo? Função binomial

# cria-se os argumentos da função
c <- amapa_agrup$qtde
d <- amapa_agrup$mortal_0
# c vai ser a quantidade total de individuos em cada grupo; d a taxa de mortalidade individual por grupo

# criando a função:
sintetico <- function(n,y)[set.seed(124)
                           rbinom(1, n, y)]

# agora para criar a função para todos os grupos deve-se usar um loop, nesse caso, foreach
amapa_agrup$mortos <- foreach(i = c, j = d) %do% {sintetico(i,j)}
## ou seja, pra cada i e pra cada j se roda o sintetico, q é a função acima
## aí se criou uma coluna chamada mortos na tabua.

# aí ele coloca esses novos valores em numero, pq eventualmente o foreach coloca outro caractere
amapa_agrup$mortos <- as.numeric(amapa_agrup$mortos)

# e aí eu peço pra me dar o somatório de mortos:
mortos_est <- sum(amapa_agrup$mortos)


## simulando a morte de pessoas em um dado ano (outro loop e função)
# criando argumentos:
e <- amapa_agrup$genero
f <- amapa_agrup$idade
h <- 1:nrow(amapa_agrup)
# esse ultimo pega a quantidade de agrupamentos

# aí se cria uma lista vazia
df_list <- list()

# se cria a função de filtro para genero e idade
filtering <- function(i,j){filter(amapa, genero==i & idade==j)}

## aí pra preencher a lista, usa-se o loop:
df_list <- foreach(i = e, j = f) %do% (filtering(i,j))

## agora mais uma função pra ver a quantidade de obitos
obitos <- function(i) {if(amapa_agrup$mortos[i]==0) {
  df_list[[i]]}} else {head(df_list[[i]], amapa_agrup$mortos[i])}}
## essa função vai tirar o numero de mortos de cada grupo de especificação

## o jeito mais facil é fazer o loop
df_list <- foreach(i = h) %do% {obitos(i)}

## agora criando um novo dataframe excluindo os que vieram à obito
amapa_sobrev <- rbindlist(df_list)

## pra identificar onde estão os mortos
mortos <- setdiff(amapa, amapa_sobrev)





