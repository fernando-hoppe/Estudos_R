#### aula 1 de R - Introdu��o

1+1
2+2
4*5
4 > 5
2**2

## Vari�veis

#### atribui��o de valores para vari�veis
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
## em value, aparecem os valores das vari�veis

## comando class da a classe da vari�vel num�rica, l�gica, caractere...

class(a)
class(l)
class(f)
n=as.numeric(m)
class(m)
class(n)

## classes diferentes n�o conseguem interagir

## DATA FRAMES

a = c('jo�o', 'maria', 'jos�')
b = c(42,53,54)
## o c cria essa concatena��o entre dados, uma linha ou coluna, no caso


## Criando o data frame
df <- data.frame(nome=a,idade=b)
## ai eu descrevo a vari�vel a e b

## para visualizar � s� clicar no df na caixa de classes
## a� se criou uma planilha com os nomes e idades

## pra ter um resum�o sobre o data frame: summary
summary(df)
## a� da toda a descri��o bonitinha, m�dias, medianas, m�ximo, 1o quarto, etc

## Para se obter um imput espec�fico da planilha
## o valor constante na linha 1, coluna 2
df[1,2]
## � colchete pq () � pra fun��es

idade_de_Jo�o = df[1,2]

idade_de_Jo�o

## Para adicionar ou mudar um valor no data frame:
df[4,2] = 60

## D� pra fazer isso visualmente com o comando fix
fix(df)
# qualquer mudan�a j� fica salva

## pra ter os valores de linhas: nesse caso linha 1:
df[1,]

## pra ter os valores de colunas: nesse caso coluna 2:
df[,2]

## se n�o se sabe o numero da coluna ou da linha pode-se chamar pelo
## nome dela:
df$idade


####### PACOTES

install.packages('dplyr')
library(dplyr)
?dplyr
?as.numeric

### a fun��o library chama a fun��o que est� instalada e vc quer usar
## se vc n�o se o que faz a fun��o: ?dplyr
## a� aparece a explica��o na caixa no canto direito inferior
## pacotes instalados tamb�m aparecem nessa caixa

install.packages("readxl")
library(readxl)

## pra ler arquivos excell --> readxl
## pra ler arquivos csv --> readcsv

## pra puxar uma planilha:
read_excel('C:/Users/hoppe/Documents/C�pia de Inadimpl�ncia x Tx Juros.xlsx')
#### obs: tem q mudar as barras \ para / pq d� erro

## atribuindo uma vari�vel � planilha
inad_juros <- read_excel('C:/Users/hoppe/Documents/C�pia de Inadimpl�ncia x Tx Juros.xlsx')
summary(inad_juros)

## a� obtemos um resum�o da planilha selecionada

## pra ter os dados de uma coluna espec�fica:
inad_juros$`Inadimpl�ncia Cr�dito - PF`

## pra criar uma coluna:
inad_juros$m�s <- 'JANEIRO'
## A� criei a coluna m�s com todos os valores Janeiro

## a seta <- � igual � = 

## Pra juntar duas planilhas: fun��o rbind
#Supondo que tenho outro planilha chamada camb_infl
#vari�veis <- rbind(inad_juros,camb_infl)
## acima eu chamei essa jun��o de 'vari�veis' e posso visualizar
# fazendo o rbind vai aparecer as duas planilhas ajuntadas:
#rbind(inad_juros,camb_infl)

## A� pra filtrar dados, quero saber dados acima ou abaixo de um valor:
inadimpl�ncia <- filter(inad_juros, `Inadimpl�ncia Cr�dito - PF`>4)
## A� filtrei pra todas observa��es dessa coluna maiores q 4

## A� pra filtrar s� valores duplicados:
inadimpl�ncia_i <- inadimpl�ncia[duplicated(inadimpl�ncia$'Limite de Confian�a Inferior(Inadimpl�ncia Cr�dito - PF)'), , drop = FALSE]
## A� dropei as observa��es que n�o possuiam repeti��o nessa vari�vel limite de confian�a

## a� pra ele me da esses dados bonitinho por data e atribuindo uma
## vari�vel � isso
inadimpl�ncia_data <- inadimpl�ncia_i$Data

## pra pegar s� dados de um valor, tipo, onde m�s = janeiro
janeiro <- filter(inadimpl�ncia,m�s=='JANEIRO')

## Pra fazer uma porcentagem
porcentagem_x <- count(janeiro)/count(inadimpl�ncia_i)
## a� ele conta a quantidade de linhas em cada vari�vel e divide

#A� pra salvar essas planilhas q criei:
write.csv(janeiro,file='C:/Users/hoppe/Documents/janeiro.csv')
## a� salvando em .csv ele salva tudo separado em virgulas, 
## a� tem q fazer a transforma��o
  
# pra salvar em xls tem q usar outra library writexl, 
# comando write_xlsx() mas nao usa o termo file
