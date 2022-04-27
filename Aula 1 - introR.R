#### aula 1 de R - Introdução

1+1
2+2
4*5
4 > 5
2**2

## Variáveis

#### atribuição de valores para variáveis
a = 1+1
b = 2-4
c=4*5
d=6/7
e=2^3
f=4>5
g=5>4

h=a
i=a+b
j=c*d
l="jajaca"
m="4"

## Na janela abaixo aparece o console, o output dos comandos. Ao lado,
## em value, aparecem os valores das variáveis

## comando class da a classe da variável numérica, lógica, caractere...

class(a)
class(l)
class(f)
n=as.numeric(m)
class(m)
class(n)

## classes diferentes não conseguem interagir

## DATA FRAMES

a = c('joão', 'maria', 'josé')
b = c(42,53,54)
## o c cria essa concatenação entre dados, uma linha ou coluna, no caso


## Criando o data frame
df <- data.frame(nome=a,idade=b)
## ai eu descrevo a variável a e b

## para visualizar é só clicar no df na caixa de classes
## aí se criou uma planilha com os nomes e idades

## pra ter um resumão sobre o data frame: summary
summary(df)
## aí da toda a descrição bonitinha, médias, medianas, máximo, 1o quarto, etc

## Para se obter um imput específico da planilha
## o valor constante na linha 1, coluna 2
df[1,2]
## é colchete pq () é pra funções

idade_de_João = df[1,2]

idade_de_João

## Para adicionar ou mudar um valor no data frame:
df[4,2] = 60

## Dá pra fazer isso visualmente com o comando fix
fix(df)
# qualquer mudança já fica salva

## pra ter os valores de linhas: nesse caso linha 1:
df[1,]

## pra ter os valores de colunas: nesse caso coluna 2:
df[,2]

## se não se sabe o numero da coluna ou da linha pode-se chamar pelo
## nome dela:
df$idade


####### PACOTES

install.packages('dplyr')
library(dplyr)
?dplyr
?as.numeric

### a função library chama a função que está instalada e vc quer usar
## se vc não se o que faz a função: ?dplyr
## aí aparece a explicação na caixa no canto direito inferior
## pacotes instalados também aparecem nessa caixa

install.packages("readxl")
library(readxl)

## pra ler arquivos excell --> readxl
## pra ler arquivos csv --> readcsv

## pra puxar uma planilha:
read_excel('C:/Users/hoppe/Documents/Cópia de Inadimplência x Tx Juros.xlsx')
#### obs: tem q mudar as barras \ para / pq dá erro

## atribuindo uma variável à planilha
inad_juros <- read_excel('C:/Users/hoppe/Documents/Cópia de Inadimplência x Tx Juros.xlsx')
summary(inad_juros)

## aí obtemos um resumão da planilha selecionada

## pra ter os dados de uma coluna específica:
inad_juros$`Inadimplência Crédito - PF`

## pra criar uma coluna:
inad_juros$mês <- 'JANEIRO'
## Aí criei a coluna mês com todos os valores Janeiro

## a seta <- é igual à = 

## Pra juntar duas planilhas: função rbind
#Supondo que tenho outro planilha chamada camb_infl
#variáveis <- rbind(inad_juros,camb_infl)
## acima eu chamei essa junção de 'variáveis' e posso visualizar
# fazendo o rbind vai aparecer as duas planilhas ajuntadas:
#rbind(inad_juros,camb_infl)

## AÍ pra filtrar dados, quero saber dados acima ou abaixo de um valor:
inadimplência <- filter(inad_juros, `Inadimplência Crédito - PF`>4)
## Aí filtrei pra todas observações dessa coluna maiores q 4

## Aí pra filtrar só valores duplicados:
inadimplência_i <- inadimplência[duplicated(inadimplência$'Limite de Confiança Inferior(Inadimplência Crédito - PF)'), , drop = FALSE]
## Aí dropei as observações que não possuiam repetição nessa variável limite de confiança

## aí pra ele me da esses dados bonitinho por data e atribuindo uma
## variável à isso
inadimplência_data <- inadimplência_i$Data

## pra pegar só dados de um valor, tipo, onde mês = janeiro
janeiro <- filter(inadimplência,mês=='JANEIRO')

## Pra fazer uma porcentagem
porcentagem_x <- count(janeiro)/count(inadimplência_i)
## aí ele conta a quantidade de linhas em cada variável e divide

#Aí pra salvar essas planilhas q criei:
write.csv(janeiro,file='C:/Users/hoppe/Documents/janeiro.csv')
## aí salvando em .csv ele salva tudo separado em virgulas, 
## aí tem q fazer a transformação
  
# pra salvar em xls tem q usar outra library writexl, 
# comando write_xlsx() mas nao usa o termo file
