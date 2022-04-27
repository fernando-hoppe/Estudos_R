### MÓDULO 2 - R_ENAP - PANORAMA

#Vetores 

#Criando um vetor
?c()

cidade <-c("Brasília",
           "São Paulo",
           "Rio de Janeiro",
           "Porto Alegre")

#Visualizando os dados do vetor
cidade

temperatura <- c(32,22,35,17)
temperatura
regiao <- c(1,2,2,3)
regiao


#Acessando o primeiro elemento
cidade[1]

#Acessando um intervalo de elementos
temperatura[1:3]

#Copiando um vetor
cidade2 <- cidade
cidade2

#Excluindo o segundo elemento da consulta
temperatura[-2] #tirou a temperatura 22 da consulta

#Altereando um vetor
cidade2[3] <- "Belo Horizonte"
cidade2

#Adicionando um novo elemento
cidade2[5] <- "Curitiba"
cidade2 

#Deletando o vetor
cidade2 <- NULL
cidade2

#Fatores 
?factor

UF <- factor(c("DF", "SP", "RJ", "RS"))
UF


grau.instrucao <- factor(c("Nível Médio",
                           "Superior",
                           "Nivel Médio",
                           "Fundamental"),
                         levels = c("Fundamental",
                                    "Nivel Médio",
                                    "Superior"),
                         ordered = TRUE)
# Veja q usando levels e ordered = true, ele colocou em ordem os níveis e omitiu o repetido
grau.instrucao


#Listas
?list()

pessoa <- list(sexo = "M", cidade = "Brasília", idade = 20)

pessoa

#Acessando o primeiro elemento da lista
pessoa[1]

#Acessando o valor do primeiro elemento da lista
pessoa[[1]]

#Editando a lista
pessoa[["idade"]] <- 22
pessoa

#Deletando elemento da lista
pessoa[["idade"]] <- NULL
pessoa

#Criando novamente a lista
pessoa <- list(sexo = "M", cidade = "Bras?lia", idade = 20)

#Filtrando elementos da lista
pessoa[c("cidade", "idade")]

#Lista de listas
cidades <- list(cidade = cidade,
                temperatura = temperatura,
                regiao = regiao)
cidades


#Criando um data frame com vetores

df <- data.frame(cidade,temperatura)
df

df2 <- data.frame(cidades)
df2

#Filtrando valores do data frame
#Recuperando o valor da linha 1, coluna 2
df[1,2]

#Recuperando todas as linhas da primeira coluna
df[, 1]

#Recuperando a Primeira linha e todas as colunas
df[1,]

#Selecionando as 3 primeiras linhas da primeira e ultima coluna
df2[c(1:3), c(1,3)]


#Verificando o nome das colunas
names(df)

#Verificando numero de linhas x colunas
dim(df)

#Verificando os tipos de dados
str(df)


#Acessar uma coluna do dataframe
df$cidade
df['cidade']

?matrix()

#Matrizes

#Criando uma matriz
m <- matrix(seq(1:9), nrow = 3)
m
#Definiu q haveria uma sequencia de 1 a 9 e 3 linhas

m2 <- matrix(seq(1:25), 
             ncol = 5,
             byrow = TRUE,
             dimnames = list(c(seq(1:5)),
                             c('A','B','C','D','E'))
)
m2
#Definiu q seria uma sequencia de 1 a 25, com 5 colunas, a sequencia se dando por linha
# e dando o nome pras linhas como 1 a 5 e pras colunas A a E.

#Filtrando a matriz
m2[c(1:2), c("B","C")]
## linhas 1 a 2, colunas b e c


###### ESTRUTURAS DE REPETIÇÃO ###########

#A função “for()” é utilizada para acessar cada elemento de uma sequência (vetor, 
#coluna de matriz ou data frame matriz), enquanto a função “while()” é 
#utilizada para repetir uma operação por um determinado número de vezes. 

#Loops

#For:

# semântica do for:
# for (valor in sequencia){
#   código...
# }

?seq

#Exemplo para "for"
for (i in seq(12)){
  print(i)
}
### Pra cada i na sequencia de 1 a 12, printar i.

#While

# while(condição){
#   código...
# }


#Exemplo de "while"

i <- 0
while(i <= 10){
  print(i)
  i = i+1
}
## Enquanto i for menor ou igual a 10, printar i, sempre somando 1 a cada nova posição
## Caso não houvesse i + 1, a sequencia nunca chegaria a 10, continuando pra sempre.



###### CONTROLE DE FLUXO #####

#Controle de fluxo

# Semântica do IF
# if(condição){}
#   código...
# }


#Exemplo - Controle de fluxo

x = 10
if (x > 0){
  print("Número Positivo")
}
## Se x maior q 0, igual à Positivo

nota = 4
if (nota >=7){
  print("Aprovado")
}else if (nota > 5 && nota < 7){
  print("Recuperação")
}else{
  print("Reprovado")
}
## Se nota maior ou igual a 7, aprovado. Ou se entre 5 e 7, recuperação. Caso contrário,
# Reprovado.



##### CRIANDO FUNÇÕES ######

#Estrutura da função

### SEMÂNTICA:
# nome.funcao <- function(argumento){
#   código
# }


#Criando a função
par.impar <- function(num){
  if((num %% 2) == 0){    ##Se o modulo da divisão der 0, retornar "Par".
    return("Par")
  }else
    return("Ímpar")   ## Caso contrário, "Impar"
}

#Usando a função
num2 = 7   ## criamos um valor: 3
par.impar(num2)  ## agora testamos esse valor dada a nossa função q identifica pares e impares


#### FUNÇÃO APPLY #####
#Apply()
?apply

x <- seq(1:9)
x

matriz <- matrix(x, ncol = 3)
matriz

result1 <- apply(matriz, 1, sum)
result1
## 1 indica que a operação deve se dar por linha. Logo, cada linha é somada

result2 <- apply(matriz, 2, sum)
result2
## 2 indica que a operação deve se dar por coluna. Cada coluna é somada

?list

numeros.p <- c(2,4,6,8,10,12)
numeros.i <- c(1,3,5,7,9,11)
numeros <- list(numeros.p,numeros.i)

numeros #famigerada lista de listas


?lapply
lapply(numeros, mean) #tira a média de ambas listas, mantendo os valore separados


?sapply
sapply(numeros, mean) #tira a média de ambas listas, colocando os valores numa mesma linha


#### CRIANDO GRÁFICOS #####

#dataset/conjunto de dados
?mtcars

#Filtrando colunas do dataset
carros <- mtcars[,c(1,2,9)] #pegou as colunas 1,2 e 9 da bd carros

#Visualizando o dataset
head(carros)

#Histograma
hist(carros$mpg)

#Gráfico de dispersão
plot(carros$mpg,carros$cyl)

install.packages("ggplot2")
library(ggplot2)

#Criando gráfico de barras com ggplot2
ggplot(carros,aes(am))+
  geom_bar()


