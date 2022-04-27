library(readr)
library(dplyr)

## ANALISE DE QUALIDADE DOS DADOS, SUPONDO QUE <10 É ESTRANHO

## LENDO OS ARQUIVOS CSV. DELIM É PRA ARQ DELIMITADOS POR ;
dados_municipios_todos <- read_delim("DadosFakesM.csv", ";", 
      escape_double = FALSE, locale = locale(decimal_mark = ",", 
      grouping_mark = "."), trim_ws = TRUE)

dados_estados_todos <- read_delim("DadosFakesE.csv", ";", 
      escape_double = FALSE, locale = locale(decimal_mark = ",",
      grouping_mark = "."), trim_ws = TRUE)

dados_união_todos <- read_delim("DadosFakesU.csv", ";", 
    escape_double = FALSE, locale = locale(decimal_mark = ",",
    grouping_mark = "."), trim_ws = TRUE)

## GERANDO ARQUIVO COMBINADO:

dados_totais <- dados_união_todos %>%
  bind_rows(dados_estados_todos, dados_municipios_todos)

## GERANDO ARQUIVO RDS COM TODOS OS DADOS:
saveRDS(dados_totais, "dados_totais.rds")

summary(dados_totais)

## Vou supor que valores abaixo de 10 sejam estranhos
## aí poderiamos filtrar todos os dados em que valor1 é menor que 10
## usando a função pipe %>%

dados_totais %>% filter(valor1 < 10)

## Aí se quisermos selecionar só algumas colunas:
dados_totais %>% filter(valor1 < 10) %>% select(periodo, valor2, valor3)

## agora agrupando valores menores que 10 por nível
dados_totais %>% filter(valor1 < 10) %>%
  group_by(nivel) %>% summarise(quantidade = n())
## Aí ele me deu quantas linhas em cada nível tem valor1 menor que 10
# Estados tem 5 linhas menores que 10
# Municipios tem 2 linhas menores que 10
# União tem 9 linhas menores que 10


## Aí pra criar uma nova coluna em que do lado dos valores estranhos
## apareça uma mensagem ou Ok.
dados_totais %>%
  mutate(situacao= ifelse(valor1>=10, "OK", "Atenção")) %>% #cria a coluna Ok ou Atenção
  group_by(nivel, situacao) %>% #agrupa por nível e situacao
  summarize(quantidade = n())
  
## agora temos a quantidade de valores em m, e, u em estado de atenção e ok.

## Poderíamos visualizar as mesmas informações num gráfico
install.packages("ggplot2")
library(ggplot2)
dados_totais %>%
  mutate(situacao= ifelse(valor1>=10, "OK", "Atenção")) %>%
  ggplot() +
  geom_bar(aes(x= nivel, fill= situacao))

## A gente pode tbm criar um gráfico filtrando os dados menores que 10
## apenas para municipios
dados_totais %>%
  filter(valor1 < 10, nivel == "M") %>% #cria o filtro pra municipios
  ggplot() +
  geom_bar(aes(y=nivel))

## acabou que não apareceu nada aqui pq não tenho distinção intramunicípios
## eu deveria ter varios municipios sob o nivel M, ai apareceriam 
## varias linhas pra cada municipio indicando os valores menores q 10

## Se eu tivesse muitos dados e quisse extrair só os 5 maiores por municipio
dados_totais %>% filter(valor1 < 10,
  nivel == "M") %>% #cria o filtro
  group_by(nivel, periodo) %>% #agrupa por nivel e periodo
  summarise(quantidade = n()) %>%
  ungroup() %>%
  mutate(nivel = reorder(nivel, quantidade)) %>% #reordena a coluna nivel
  slice_max(
    quantidade, n=5) %>% #executar pipe só pra função a seguir

## aí se eu quisesse criar um gráfico disso:
  ggplot() + geom_col(aes(x=quantidade, y=nivel, fill= factor(periodo)))

## lembrando q estou criando filtro pro valor1,
## mas poderia ser pros outros tbm, bem como pra E ou U, ou anos

## é importante colocar o ungroup depois do summarize pra desagrupar
## já que poderia vir numa ordenação que não gostariamos
## inclusive é boa pratica sempre fazer isso


### AQUI TERMINA ESSE BLOCO SOBRE ANALISE DE QUALIDADE DOS DADOS

## agora supondo que queiramos ver quanto cada ente gasta por período
## no valor 1

dados_totais %>% filter(valor1 >= 10) %>% 
  group_by(nivel, periodo) %>%
  summarise(total_valores = sum(valor1)) %>% 
  ungroup() %>%
  mutate(periodo= reorder(periodo, total_valores)) %>%
  ggplot() + geom_col(aes(y=periodo, x = total_valores, fill= nivel))

## Na aula ele filtrou pelo tipo de gasto, aqui eu filtrei por periodo

## como as pontas ficaram muito discrepantes podemos criar mais um
## filtro pra pegar só algumas observações

## Digamos que eu queira tirar as observações acima de 2040
dados_totais %>% 
  filter(valor1 >= 10,
         !periodo %in% c("2040", "2041", "2042", "2043", "2044",
                         "2045", "2046")) %>%
  group_by(nivel, periodo) %>%
  summarise(total_gasto = sum(valor1)) %>%
  ungroup() %>%
  mutate(periodo= reorder(periodo, total_gasto)) %>%
  ggplot() + geom_col(aes(y=periodo, x = total_gasto, fill= nivel))

### Perceba que ! funciona como negação
### c é de conjunto
### quando trabalhamos com mais de 1 elemento no filtro se usa %in% 

### a gente poderia tentar ver quanto a média dos valores gastos:
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
### aqui no caso não aparece nada pq não tem valores discrepantes.

## pra visualizar quem são outliers:
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

## aí da um gráfico que não faz sentido nenhum, mas na aula,
## o prof fez um grafico em que cada municipio mais gastador,
## ficava com seus gastos discricionados por cor
## tudo isso num gráfico só. E aí se percebia que São Paulo era bem
## discrepante dos demais e o tipo de gasto ficava discricionado.

## Podemos também ponderar um valor pelo outro.
dados_totais %>%
  filter(valor1 >= 10,
         nivel == "M") %>%
  group_by(nivel, periodo) %>%
  summarise(gasto_por_gasto = sum(valor1/valor2)) %>%
  ggplot() + geom_boxplot(aes(x= nivel, y= gasto_por_gasto))

## novamente, temos um boxplot sem nada, Mas o prof obteve um
## boxplot pra cada tipo de gasto numa razão por habitante.

## se eu quiser colocar em log era só botar no final:
scale_y_log10()
## Aí com o boxplot a gente consegue comparar medianas

## pra fazer um gráfico violino (em que tu tem a distribuição de cada
## gasto)
## mesma coisa,
## ggplot() +
## geom_violin(aes(x= nivel, y= gasto_por_gasto)) +
## scale_y_log10()

### agora pra ver correlaçoes
install.packages("corrplot")
install.packages("tidyr")

library(tidyr)
library(corrplot)

## pra fazer a correlação ele transpõe a matriz de gasto por tipo,
## pq na planilha dele o tipo de gasto é uma coluna e o valor do gasto,
## é outra coluna.
## transpondo ele obtem duas linhas, tipo x gasto
## ele faz isso assim:
dados_totais_num <- dados_totais %>%
  filter(valor1 >= 10) %>%
  group_by(nivel, periodo) %>%
  summarise(valor_total = sum(valor1, valor2, valor3)) %>%
  ungroup() %>%
  pivot_wider(names_from = periodo, values_from = valor_total) %>%
  #pivot_wider faz a transposição
  select(3:5) ##seleciona as colunas 2 a 5, pq pra fazer 
##  corr não pode ter palavras

cc <- cor(na.omit(dados_totais_num)) ##gera uma matriz de correlação
corrplot(cc,method="color") ## aí deveria gerar um gráfico
## quanto mais forte o azul maior a correlação positiva, no caso,
## como deu tudo 1, tudo fica bem azul.
## mais próximo do vermelho, maior a corr negativa.

### FAZENDO UM GRÁFICO DE DISPERSÃO:
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
## aqui no caso dá errado. Na aula ele fez a correlação entre
## segurança publica e cultura, aí esses termos vão no filter, onde
## coloquei 2000 e 2001. 
## aí terias um gráfico de disperção dos municipios onde y=sp;x=cult
## claramente teria uma correlação positiva.




