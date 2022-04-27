## AULA 4 - MANIPULAÇÃO DE DADOS NO R II

## Pro prof., o capítulo 5 do livro R for Data Science de Grolemund e Wickham cobre
# bem os aspectos básicos da manipulação de dados no R

## Seriam os comandos do pacote dplyr: filter, arrange, select, mutate, summarise, group_by
## Mas para o prof., vai além: envolve capacidade de solucionar 'pepinos'

# Vai trabalhar com dados da Rais e do Siconfi
# A ideia de usar Rais é pra obter os microdados do Siconfi. Enquanto Siconfi só daria
# quantidade de valores gastos pra determinadas categorias, a Rais dá informações 
# sobre quantidade de funcionários, etc.

## O primeiro pepeino é q o arquivo da Rais é gigante
# Logo, é interessante usar fread (de fast read) e pacote "data.table"
install.packages("data.table")

library(tidyverse)
library(dplyr)
library(data.table)
library(readr)

setwd("C:/Users/hoppe/Documents/Mestrado/R_Intermediário")

rais_norte <- fread("RAIS_VINC_PUB_NORTE.txt", dec = ",")
## decimal separada por vírgula (padrão BR)

## fread NÃO cria data frame, cria um objeto qualquer q precisa ser transformado
#em df

rais_norte <- as.data.frame(rais_norte)

head(rais_norte)
head(rais_norte, 100) #100 primeiras linhas
str(rais_norte) #dá as variáveis das colunas e os tipos

## muitas variáveis são inúteis pra essa análise. Poderia usar select e ir selecio-
#nando, mas como são 16, demoraria muito. Outra opção é considerá-las NULL:
rais_norte[,c(1:11, 13:19, 21:25, 27, 29:32, 34:46, 58:60)] <- list(NULL)

## Outro problema: os nomes das variáveis estão com espaço (R não reconhece)
# pra lidar com isso vc cria um vetor com nomes substitutos

v <- c("vinc_ativo", "idade", "munic", "nat_jur", "rem_dez","rem_jan","rem_fev",
       "rem_mar","rem_abr","rem_mai","rem_jun","rem_jul","rem_ago","rem_set",
       "rem_out","rem_nov")
# aí usando a função names você dá pra cada variável os nomes q tão no vetor, na ordem

names(rais_norte)[1:16] <- v

str(rais_norte)
# O número de dados no vetor tem q bater com o numero de dados na variável

## Queremos trabalhar apenas com servidores públicos de municípios. Logo, temos q
#usar select
#Tem q olhar o manual da base pra ver o código da natureza juridica da ocupação
# Servidores de orgão público executivo municipal: 103-1...

rais_norte_apu <- filter(rais_norte, nat_jur %in% c(1031, 1066, 1120, 1155, 1180, 1244, 1279, 1309, 1333))

# como agora a bd rais_norte não é mais necessária, pode-se apagá-la:
rm(rais_norte)

## Queremos saber a folha de pagamento por município NO ANO, não por mês
#Logo, precisamos somar os meses

rais_norte_apu <- mutate(rais_norte_apu, venc_anuais = (rem_dez+rem_jan+rem_fev+rem_mar+rem_abr+rem_mai+rem_jun+rem_jul+rem_ago+rem_set+rem_out+rem_nov))

## agora vamos agrupar tudo por município e obter estatísticas sumárias

rais_norte_apu_munic <- group_by(rais_norte_apu, by = munic)
rais_norte_apu_munic <- summarise(rais_norte_apu_munic, folha_pagamentos_anual = sum(venc_anuais, na.rm = TRUE))
# depois de criarmos um novo objeto com os dados agrupados por municipio,
#somamos os vencimentos anuais pra cada um deles criando a variavel folha de pagamentos
# OBS: deletemos observações omissas

##### Finalizamos com a Rais, dados devidamente manipulados #####

### Base do Siconfi:
stn <- read.csv2("finbra.csv", skip = 3)
## o skip é necessário pois a primeiras 3 linhas são de identificação

## Pepino: Código do IBGE no stn tá com 7 dígitos e na rais com 6 dígitos
stn$Cod.IBGE <- substring(stn$Cod.IBGE, 1, 6)
## vai pegar apenas os dígitos contido entre a 1 e 6 posição

## agora vamos pegar os dados da folha de pagamentos dos municípios na base stn:
stn_folha <- filter(stn, Conta == "3.1.90.11.00 - Vencimentos e Vantagens Fixas - Pessoal Civil" & Coluna == "Despesas Liquidadas")

# Aí temos de todos os municípios brasileiros, mas queremos apenas do norte:
stn_folha_norte <- filter(stn_folha, UF %in% c("AM","AP","AC","PA","RR","RO","TO"))

## Perceba que o numero de munic é diferente na base da rais e do stn
## vamos usar Join, aí tem q ver qual join usar e isso depende da chave de pareamento
## vamos usar full_join pra simplesmente juntar tudo, mesmo q uma delas tenha menos infos
## pareando o que da.

## Primeiramente precisamos igualar o nome da variável do código ibge
names(stn_folha_norte)[2] <- "munic"
names(rais_norte_apu_munic)[1] <- "munic"

## Join ainda não vai funcionar pq o tipo de variável tá diferente, veja:
str(stn_folha_norte) #munic caracter
str(rais_norte_apu_munic) #munic é int

stn_folha_norte$munic <- as.integer(stn_folha_norte$munic)
### Agora finalmente é possível juntar as duas bases:

sintese_stn_rais <- full_join(stn_folha_norte, rais_norte_apu_munic)
## R juntou tudo pela ID munic

## Será que os dados da rais e do siconfi batem? Podemos criar variavel de comparação

sintese_stn_rais <- mutate(sintese_stn_rais, consist = Valor/folha_pagamentos_anual)

## vamos criar uma consistencia arbitrária e filtrar por ele:
munics_consist <- filter(sintese_stn_rais, consist <= 1.15 & consist >= 0.85)
## achei 122 munics consistentes.