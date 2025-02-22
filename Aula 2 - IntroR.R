library(readr)
library(dplyr)

## ANALISE DE QUALIDADE DOS DADOS, SUPONDO QUE <10 � ESTRANHO

## LENDO OS ARQUIVOS CSV. DELIM � PRA ARQ DELIMITADOS POR ;
dados_municipios_todos <- read_delim("DadosFakesM.csv", ";", 
      escape_double = FALSE, locale = locale(decimal_mark = ",", 
      grouping_mark = "."), trim_ws = TRUE)

dados_estados_todos <- read_delim("DadosFakesE.csv", ";", 
      escape_double = FALSE, locale = locale(decimal_mark = ",",
      grouping_mark = "."), trim_ws = TRUE)

dados_uni�o_todos <- read_delim("DadosFakesU.csv", ";", 
    escape_double = FALSE, locale = locale(decimal_mark = ",",
    grouping_mark = "."), trim_ws = TRUE)

## GERANDO ARQUIVO COMBINADO:

dados_totais <- dados_uni�o_todos %>%
  bind_rows(dados_estados_todos, dados_municipios_todos)

## GERANDO ARQUIVO RDS COM TODOS OS DADOS:
saveRDS(dados_totais, "dados_totais.rds")

summary(dados_totais)

## Vou supor que valores abaixo de 10 sejam estranhos
## a� poderiamos filtrar todos os dados em que valor1 � menor que 10
## usando a fun��o pipe %>%

dados_totais %>% filter(valor1 < 10)

## A� se quisermos selecionar s� algumas colunas:
dados_totais %>% filter(valor1 < 10) %>% select(periodo, valor2, valor3)

## agora agrupando valores menores que 10 por n�vel
dados_totais %>% filter(valor1 < 10) %>%
  group_by(nivel) %>% summarise(quantidade = n())
## A� ele me deu quantas linhas em cada n�vel tem valor1 menor que 10
# Estados tem 5 linhas menores que 10
# Municipios tem 2 linhas menores que 10
# Uni�o tem 9 linhas menores que 10


## A� pra criar uma nova coluna em que do lado dos valores estranhos
## apare�a uma mensagem ou Ok.
dados_totais %>%
  mutate(situacao= ifelse(valor1>=10, "OK", "Aten��o")) %>% #cria a coluna Ok ou Aten��o
  group_by(nivel, situacao) %>% #agrupa por n�vel e situacao
  summarize(quantidade = n())
  
## agora temos a quantidade de valores em m, e, u em estado de aten��o e ok.

## Poder�amos visualizar as mesmas informa��es num gr�fico
install.packages("ggplot2")
library(ggplot2)
dados_totais %>%
  mutate(situacao= ifelse(valor1>=10, "OK", "Aten��o")) %>%
  ggplot() +
  geom_bar(aes(x= nivel, fill= situacao))

## A gente pode tbm criar um gr�fico filtrando os dados menores que 10
## apenas para municipios
dados_totais %>%
  filter(valor1 < 10, nivel == "M") %>% #cria o filtro pra municipios
  ggplot() +
  geom_bar(aes(y=nivel))

## acabou que n�o apareceu nada aqui pq n�o tenho distin��o intramunic�pios
## eu deveria ter varios municipios sob o nivel M, ai apareceriam 
## varias linhas pra cada municipio indicando os valores menores q 10

## Se eu tivesse muitos dados e quisse extrair s� os 5 maiores por municipio
dados_totais %>% filter(valor1 < 10,
  nivel == "M") %>% #cria o filtro
  group_by(nivel, periodo) %>% #agrupa por nivel e periodo
  summarise(quantidade = n()) %>%
  ungroup() %>%
  mutate(nivel = reorder(nivel, quantidade)) %>% #reordena a coluna nivel
  slice_max(
    quantidade, n=5) %>% #executar pipe s� pra fun��o a seguir

## a� se eu quisesse criar um gr�fico disso:
  ggplot() + geom_col(aes(x=quantidade, y=nivel, fill= factor(periodo)))

## lembrando q estou criando filtro pro valor1,
## mas poderia ser pros outros tbm, bem como pra E ou U, ou anos

## � importante colocar o ungroup depois do summarize pra desagrupar
## j� que poderia vir numa ordena��o que n�o gostariamos
## inclusive � boa pratica sempre fazer isso


### AQUI TERMINA ESSE BLOCO SOBRE ANALISE DE QUALIDADE DOS DADOS

## agora supondo que queiramos ver quanto cada ente gasta por per�odo
## no valor 1

dados_totais %>% filter(valor1 >= 10) %>% 
  group_by(nivel, periodo) %>%
  summarise(total_valores = sum(valor1)) %>% 
  ungroup() %>%
  mutate(periodo= reorder(periodo, total_valores)) %>%
  ggplot() + geom_col(aes(y=periodo, x = total_valores, fill= nivel))

## Na aula ele filtrou pelo tipo de gasto, aqui eu filtrei por periodo

## como as pontas ficaram muito discrepantes podemos criar mais um
## filtro pra pegar s� algumas observa��es

## Digamos que eu queira tirar as observa��es acima de 2040
dados_totais %>% 
  filter(valor1 >= 10,
         !periodo %in% c("2040", "2041", "2042", "2043", "2044",
                         "2045", "2046")) %>%
  group_by(nivel, periodo) %>%
  summarise(total_gasto = sum(valor1)) %>%
  ungroup() %>%
  mutate(periodo= reorder(periodo, total_gasto)) %>%
  ggplot() + geom_col(aes(y=periodo, x = total_gasto, fill= nivel))

### Perceba que ! funciona como nega��o
### c � de conjunto
### quando trabalhamos com mais de 1 elemento no filtro se usa %in% 

### a gente poderia tentar ver quanto a m�dia dos valores gastos:
dados_totais %>%
  filter(valor1 >= 10, nivel == "M") %>%
  group_by(nivel, periodo) %>%
  summarise(media_gasto = mean(valor1)
            ) %>%
  ungroup() %>%
  mutate(periodo= reorder(periodo, media_gasto)) %>%
  ggplot() + geom_col(aes(y=periodo, x = media_gasto))

### poderia trocar mean (media) por median (mediana)

## fazendo um boxplot pra ver se tem outliers por periodo:
dados_totais %>% 
  filter(valor1 >= 10,
         nivel == "M",
         periodo %in% c("2040", "2041", "2042", "2043", "2044,",
                        "2045", "2046")) %>%
  ggplot() +
  geom_boxplot(aes(x= periodo, y= valor1))
### aqui no caso n�o aparece nada pq n�o tem valores discrepantes.

## pra visualizar quem s�o outliers:
dados_totais %>% 
  filter(valor1 >= 10,
         nivel == "M",
         periodo %in% c("2040", "2041", "2042", "2043", "2044,",
                        "2045", "2046")) %>%
  group_by(nivel, periodo) %>%
  summarise(valor_total = sum(valor1)) %>%
  ungroup() %>%
  slice_max(valor_total, n = 10) %>%
  mutate(nivel = reorder(nivel, valor_total)) %>%
  ggplot() +
  geom_col(aes(y= nivel, x= valor_total, fill = periodo))

## a� da um gr�fico que n�o faz sentido nenhum, mas na aula,
## o prof fez um grafico em que cada municipio mais gastador,
## ficava com seus gastos discricionados por cor
## tudo isso num gr�fico s�. E a� se percebia que S�o Paulo era bem
## discrepante dos demais e o tipo de gasto ficava discricionado.

## Podemos tamb�m ponderar um valor pelo outro.
dados_totais %>%
  filter(valor1 >= 10,
         nivel == "M") %>%
  group_by(nivel, periodo) %>%
  summarise(gasto_por_gasto = sum(valor1/valor2)) %>%
  ggplot() + geom_boxplot(aes(x= nivel, y= gasto_por_gasto))

## novamente, temos um boxplot sem nada, Mas o prof obteve um
## boxplot pra cada tipo de gasto numa raz�o por habitante.

## se eu quiser colocar em log era s� botar no final:
scale_y_log10()
## A� com o boxplot a gente consegue comparar medianas

## pra fazer um gr�fico violino (em que tu tem a distribui��o de cada
## gasto)
## mesma coisa,
## ggplot() +
## geom_violin(aes(x= nivel, y= gasto_por_gasto)) +
## scale_y_log10()

### agora pra ver correla�oes
install.packages("corrplot")
install.packages("tidyr")

library(tidyr)
library(corrplot)

## pra fazer a correla��o ele transp�e a matriz de gasto por tipo,
## pq na planilha dele o tipo de gasto � uma coluna e o valor do gasto,
## � outra coluna.
## transpondo ele obtem duas linhas, tipo x gasto
## ele faz isso assim:
dados_totais_num <- dados_totais %>%
  filter(valor1 >= 10) %>%
  group_by(nivel, periodo) %>%
  summarise(valor_total = sum(valor1, valor2, valor3)) %>%
  ungroup() %>%
  pivot_wider(names_from = periodo, values_from = valor_total) %>%
  #pivot_wider faz a transposi��o
  select(3:5) ##seleciona as colunas 2 a 5, pq pra fazer 
##  corr n�o pode ter palavras

cc <- cor(na.omit(dados_totais_num)) ##gera uma matriz de correla��o
corrplot(cc,method="color") ## a� deveria gerar um gr�fico
## quanto mais forte o azul maior a correla��o positiva, no caso,
## como deu tudo 1, tudo fica bem azul.
## mais pr�ximo do vermelho, maior a corr negativa.

### FAZENDO UM GR�FICO DE DISPERS�O:
dados_totais %>%
  filter(valor1 > 10,
         periodo %in% c("2000","2001")) %>%
  group_by(nivel, periodo) %>%
  summarise(valor_total = sum(valor1, valor2, valor3)) %>%
  ungroup() %>%
  pivot_wider(names_from = periodo, values_from = valor_total) %>%
  ggplot() + 
  geom_point(aes(x="2000", y="2001")) + 
  scale_x_log10() +
  scale_y_log10()
## aqui no caso d� errado. Na aula ele fez a correla��o entre
## seguran�a publica e cultura, a� esses termos v�o no filter, onde
## coloquei 2000 e 2001. 
## a� terias um gr�fico de disper��o dos municipios onde y=sp;x=cult
## claramente teria uma correla��o positiva.




