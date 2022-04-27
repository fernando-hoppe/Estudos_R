## AULA 5 - ggplot

options(scipen = 999) #remove a exibição de notação científica

##PACOTES##
install.packages("ggplot2")
install.packages("RColorBrewer")

library(ggplot2)
library(RColorBrewer)

## puxar a base, já inferindo um objeto à essa base
sen2018 <- read.csv2("sen2018.csv") #carrega a base em csv separada por ponto e vírgula
## se a base é separada apenas por vírgula usa-se read.csv

## gráfico de dispersão
plot(sen2018$TOTAL_RECEITAS, sen2018$votos) ## esse gráfico é do RBase
## perceba: seleciona-se o objeto e a variável, por eixo (x,y)
## gráfico fica meio ruim, inintelígivel

## ggplot divide o gráfico em termos semânticos: estético e geométrico
dis <- ggplot(sen2018, aes(TOTAL_RECEITAS, votos)) ##mapa estético do gráfico anterior
#sintaxe: função ggplot(base de dados, definição mapa estético(eixo x, eixo y))

# pra exibir de fato o gráfico, tem q adicionar o objeto geométrico:
dis + geom_point() # objeto geométrico do gráfico
## é bem mais bonitinho q o plot do rbase

## pra ficar melhor, ele vai colocar os valores em log
## primeiro é necessário tirar valores = 0
sen2018 <- subset(sen2018, votos > 0) #seleciona apenas os casos >0
sen2018$logrec <- log(sen2018$TOTAL_RECEITAS) #transforma o total de receitas em log
## logrec é uma variável criada
sen2018$logvot <- log(sen2018$votos) #transforma votos em log

## agora da pra criar um novo grafico de dispersão com as var em log
dis <- ggplot(sen2018, aes(logrec, logvot)) #mapa estético
dis + geom_point() #objeto geométrico
## já fica parecendo uma tendência

### pra salvar esse conjunto de mapa estético + objeto geométrico:
dis <- dis+geom_point()

## agora simplesmente chamando dis, exibe o gráfico


## alterando elementos do gráfico:
dis + labs(title = "Arracadação e Votos Recebidos por Candidato",
           subtitle = "Função Logarítmica do Total Arrecadado..."
           x = "log do total de receitas"
           y = "log de votos recebidos"
           caption = "Fonte: TSE")
## aí já fica com titulo, subtítulo, nome dos eixos e legenda

## pra mudar o tema do gráfico:
dis + labs(title = "Arracadação e Votos Recebidos por Candidato",
           subtitle = "Função Logarítmica do Total Arrecadado..."
           x = "log do total de receitas"
           y = "log de votos recebidos"
           caption = "Fonte: TSE")+
           theme_classic()

## preto e branco:
dis + labs(title = "Arracadação e Votos Recebidos por Candidato",
           subtitle = "Função Logarítmica do Total Arrecadado..."
           x = "log do total de receitas"
           y = "log de votos recebidos"
           caption = "Fonte: TSE")+
           theme_gray()

## outros temas:
theme_dark()
theme_gray()
theme_light()
theme_minimal()
theme_void()

### adicionando linha de regressao ao gráfico de dispersão
dis + geom_smooth(method = lm)+
       labs(title = "Arracadação e Votos Recebidos por Candidato",
       subtitle = "Função Logarítmica do Total Arrecadado..."
       x = "log do total de receitas"
       y = "log de votos recebidos"
       caption = "Fonte: TSE")+
  theme_bw()
## exibe a linha de tendência e o erro padrão no gráfico de dispersão

## pra remover o erro padrão:
dis + geom_smooth(method = lm, se = F)+
  labs(title = "Arracadação e Votos Recebidos por Candidato",
       subtitle = "Função Logarítmica do Total Arrecadado..."
       x = "log do total de receitas"
       y = "log de votos recebidos"
       caption = "Fonte: TSE")+
  theme_bw()

## Adicionando texto dentro do gráfico: r2 por exemplo
reg <- lm(logvot~logrec, data = sen2018) #definindo a formula da equação da reta
## modelo linear simples, logvotos/logrec, aí tem q se definir o dataset
# aí vai criar mais um objeto. Fazendo summary(reg) vc vai ter o output dessa equação
# aí vai aparecer o r2

#pra adicionar esse r2 no gráfico, primeiro tem q criar o objeto
r2lab <- bquote(R^2~"="~.(round(summary(reg)$r.squared, 2))) #transformou o r2 em expressão
## round serve pra arredondar o numero, no caso, duas casas

dis + geom_smooth(method = lm, se = F)+
  labs(title = "Arracadação e Votos Recebidos por Candidato",
       subtitle = "Função Logarítmica do Total Arrecadado..."
       x = "log do total de receitas"
       y = "log de votos recebidos"
       caption = "Fonte: TSE")+
  annotate(geom = "text", x = 6, y = 15.5, label = r2lab)+
  theme_bw()
## esse 6 e 15 é pra indicar mais ou menos onde o valor é pra aparecer


## Alterando o tamanho do texto

# alterando o tamanho de todas as fontes:
dis + geom_smooth(method = lm, se = F)+
  labs(title = "Arracadação e Votos Recebidos por Candidato",
       subtitle = "Função Logarítmica do Total Arrecadado..."
       x = "log do total de receitas"
       y = "log de votos recebidos"
       caption = "Fonte: TSE")+
  annotate(geom = "text", x = 6, y = 15.5, label = r2lab)+
  theme_bw()+
  theme(text = element_text(size = 15))

## alterando elementos específicos (valores dos eixos
dis + geom_smooth(method = lm, se = F)+
  labs(title = "Arracadação e Votos Recebidos por Candidato",
       subtitle = "Função Logarítmica do Total Arrecadado..."
       x = "log do total de receitas"
       y = "log de votos recebidos"
       caption = "Fonte: TSE")+
  annotate(geom = "text", x = 6, y = 15.5, label = r2lab)+
  theme_bw()+
  theme(axis.text = element_text(size = 15))

# outros elementos passiveis de alteração
axis.text.x #eixo x
axis.text.y #eixo y
axis.title # rotulo dos eixos
axis.title.x
axis.title.y
plot.title #titulo do gráfico
legend.text
help(theme) #mostra todas as possibilidades

###GRÁFICO DE BARRAS###
## Primeiro, criar uma variável para servir de marcador, fazer contagem:
sen2018$cont <- 1

## Vamos plotar o número de candidatos das 5 maiores bancadas no senado: MDB (15),
## PSD (11), PODE (9), PP(7), PSDB (7)

## duas maneiras: criando um novo data frame usando subset
sen2018m5 <- subset(sen2018, SIGLA_PARTIDO == "MDB" | SIGLA_PARTIDO == "PSD" |
                      SIGLA_PARTIDO == "PSDB" ...)
table(sen2018m5$SIGLA_PARTIDO)
## o problema de usar essa forma é q se cria um novo df, o q gera mais gasto
## em termos de memória

## outra maneira: subset dentro do ggplot (ai não precisa criar novo df)
bar <- ggplot(subset(sen2018, SIGLA_PARTIDO == "MDB" | SIGLA_PARTIDO == "PSD" |
                       ...), aes(SIGLA_PARTIDO, cont))
bar + geom_bar(stat = "identity")
## o aes indica aes(eixo x, eixo y). Cont é o marcador q criamos

#Alterando elementos textuais
bar + geom_bar(stat = "identity") + 
  labs(title = "Número de Candidatos por Partido",
       subtitle = "Candidaturas ao Senado em 2018 das cinco maiores bancadas"
       x = "partido",
       y = "número de candidatos",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15))

## Alterando cores do gráfico
bar + geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Número de Candidatos por Partido",
       subtitle = "Candidaturas ao Senado em 2018 das cinco maiores bancadas"
       x = "partido",
       y = "número de candidatos",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15))
## muda a cor de todas as barras

## atribuindo uma cor para cada partido (requer alteração no mapa estético):
bar <- ggplot(subset(sen2018, SIGLA_PARTIDO == "MDB" | SIGLA_PARTIDO == "PSD" |
                       ...), aes(SIGLA_PARTIDO, cont, fill = SIGLA_PARTIDO))
bar + geom_bar(stat = "identity") +
  labs(title = "Número de Candidatos por Partido",
       subtitle = "Candidaturas ao Senado em 2018 das cinco maiores bancadas"
       x = "partido",
       y = "número de candidatos",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15))
## agora cada partido ficou com uma cor. Automaticamente vai se criar uma legenda
# indicando qual é a cor de cada partido

## removendo a legenda:
bar + geom_bar(stat = "identity") +
  labs(title = "Número de Candidatos por Partido",
       subtitle = "Candidaturas ao Senado em 2018 das cinco maiores bancadas"
       x = "partido",
       y = "número de candidatos",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15), legend.position = "none")

# a legenda também pode ser posicionada em outros locais, "top", "bottom", "left"

# usando uma paleta de cores
## A função "scale_fill_brewer()" emprega a paleta de cores do pacote 'RColorBrewer'
# Já 'scale_fill_manual()' atribui manualmente as cores. A atribuição é feita
# com a função c(), colocando o código html de cada cor, usando # antes do código
# Por fim, 'scale_fill_grey emprega escala de cinza.

bar + geom_bar(stat = "identity") +
  labs(title = "Número de Candidatos por Partido",
       subtitle = "Candidaturas ao Senado em 2018 das cinco maiores bancadas"
       x = "partido",
       y = "número de candidatos",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15), legend.position = "none")+
  scale_fill_brewer(palette = "Dark2")


bar + geom_bar(stat = "identity") +
  labs(title = "Número de Candidatos por Partido",
       subtitle = "Candidaturas ao Senado em 2018 das cinco maiores bancadas"
       x = "partido",
       y = "número de candidatos",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15), legend.position = "none")+
  scale_fill_manual(values = c("#FFFF00", "#90EE90", ...))

# Adicionando uma terceira variável no gráfico de barras
# precisa alterar o mapa estético

sen2018$DESCRICAO_SEXO <- as.factor(sen2018$DESCRICAO_SEXO)
bar <- ggplot(subset(sen2018, SIGLA_PARTIDO == "MDB" | ...)
              aes(as.factor(SIGLA_PARTIDO), cont, fill = DESCRICAO_SEXO))
## OU SEJa, to pedindo pra q as cores sejam preenchidas de acordo com o sexo

bar + geom_bar(stat = "identity")


# alterando elementos textuais e posição da legenda
bar + geom_bar(stat = "identity") +
  labs(title = "Número de Candidatos por Partido",
       subtitle = "Candidaturas ao Senado em 2018 das cinco maiores bancadas"
       x = "partido",
       y = "número de candidatos",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15), legend.position = "bottom")+
  scale_fill_discrete(name = "sexo")
# legenda posicionada embaixo, legenda agora ta escrito sexo, ao inves do nome da variável

# alterando a posição das barras, pra separar candidaturas femininas e masculinas
# nesse caso é necessário alterar a base de dados, já q o ggplot não faz o computo
# de duas categorias. Vai ser necessário sumarizar os casos de modo que apresente
# as somas por cada par de categorias (partido e sexo). É feito por meio de 
# trible com o comando 'mudate' do tidyverse. Dá pra fazer pelo aggregate tbm

bar1 <- aggregate(cont~SIGLA_PARTIDO+DESCRICAO_SEXO,
                  data = subset(sen2018, SIGLA_PARTIDO == "MDB" | ...),
                                FUN = "sum")
# definiiu q quer a contagem pela sigla do partido e sexo do candidato
# e base de dados com esses valores citados e somar os casos.
# isso ai em cima criou um novo dataset

bar <- ggplot(bar1, aes(SIGLA_PARTIDO, cont, fill = DESCRICAO_SEXO))
# aes(eixo x, eixo y, opções)
bar + geom_bar(stat = "identity", position=position_dodge(preserve = "single"))+
  labs(title = "Número de candidatos por partido e sexo",
       subtitle = "Candidaturas ao senado em 2018 das cinco maiores bancadas",
       x = "partido",
       y = "numero de cadidatos",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15), legend.position = "bottom")+
  scale_fill_discrete(name = "sexo")
# position_dodge: exibir informações lado a lado (separando masc e fem)
# preserve = "single": pro caso em q só há um sexo, aí preserva a barra cheia


## Plotando médias em gráficos de barras##
bar.m <- ggplot(subset(sen2018, SIGLA_PARTIDO == "MDB" | ...),
                aes(SIGLA_PARTIDO, TOTAL_RECEITAS)) #novo mapa estético

bar.m + geom_bar(stat = "identity") #plota a soma dos gastos por categoria(partidos)
# aí em cima vai simplesmente exibir o otal de receitas por partido

bar.m + geom_bar(stat = "summary", fun = "mean") #plota a média por categoria(partidos)
# agora é a média por candidatura por partido

#Adicionando elementos textuais:
bar.m + geom_bar(stat = "summary", fun = "mean", fill = "steelblue")+
  labs(title = "Média de Arrecadação por Candidatura",
       subtitle = "valor médio arrecadado das candidaturas ao Senado em 2018 das 5 maiores bancadas",
       x = "partido",
       y = " média de arrecadação (R$)",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15))

# Adicionando o valor de cada barra
bar.m + geom_bar(stat = "summary", fun = "mean", fill = "steelblue")+
  stat_summary(aes(label=round(..y.., 2)), fun=mean, geom="text", size=4, vjust = 10,
               color = "black")+
  labs(title = "Média de Arrecadação por Candidatura",
       subtitle = "valor médio arrecadado das candidaturas ao Senado em 2018 das 5 maiores bancadas",
       x = "partido",
       y = " média de arrecadação (R$)",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15))
## round: vai arredondar o valor de y em 2 casas decimais, usando a função média,
# usando objeto geométrico de texto, de tamanho 4 e justificação vertical = 10
# Aí rodando fica o valor médio de cada partido dentro da barra.


## Adicionando barras de erro
bar.m + geom_bar(stat = "summary", fun = "mean", fill = "steelblue")+
  stat_summary(aes(label=round(..y.., 2)), fun=mean, geom="text", size=4, vjust = 10,
               color = "black")+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5)+
  labs(title = "Média de Arrecadação por Candidatura",
       subtitle = "valor médio arrecadado das candidaturas ao Senado em 2018 das 5 maiores bancadas",
       x = "partido",
       y = " média de arrecadação (R$)",
       caption = "Fonte: TSE")+
  theme_bw()+
  theme(text = element_text(size = 15))
# mean_se: média do standard error
# aí vai criar aquela linha q dá o erro padrão.


# Gráfico de Linhas##
# usado em geral pra séries temporais. Qualquer outro tipo de informação é melhor 
# não usar. Aqui vamos ver pra fins didáticos.

# Já q não é exatamente um série de tempo ele vai preparar os dados fazendo
# o total de candidaturas por sexo e estado
lin1 <- aggregate(cont~DESCRICAO_SEXO+SIGLA_UF, data = sen2018, FUN = "sum")

# Mapa estético do gráfico de linhas - Total de Candidaturas
lin <- ggplot(lin1, aes(SIGLA_UF, cont, group = 1))
lin+geom_line()
## aes(eixo x, eixo y, agrupando tudo em 1)
# é um gráfico q não faz mt sentido pq é a contagem de candidaturas por estado, só
# q em linhas

## aí adicionando pontos no gráfico:
lin+geom_line()+
  geom_point()

## agora separando candidaturas masc e fem em duas linhas
lin <- ggplot(lin1, aes(SIGLA_UF, cont, group = DESCRICAO_SEXO))
lin+geom_line()
## aí vai se ter duas linhas mas sem identificação

## identificando as linhas do gráfico
lin <- ggplot(lin1, aes(SIGLA_UF, cont, group = DESCRICAO_SEXO))
lin+geom_line(aes(colour = DESCRICAO_SEXO))+
  geom_point(aes(colour = DESCRICAO_SEXO))

# Alterando elementos textuais e legenda
lin+geom_line(aes(colour = DESCRICAO_SEXO))+
  geom_point(aes(colour = DESCRICAO_SEXO))+
  labs(title = "Total de candidaturas por sexo",
       subtitle = "Candidaturas masculinas e fem por UF",
       x = "UF",
       y = "Média de Arrecadação",
       caption = "Fonte: TSE",
       color = "Sexo")+
  theme_bw()+
  theme(text = element_text(size = 15), legend.position = "bottom")

## Alterando a espessura e tipo de linha e ponto
lin+geom_line(aes(colour = DESCRICAO_SEXO, linetype = DESCRICAO_SEXO), size = 1.5)+
  geom_point(aes(colour = DESCRICAO_SEXO, shape = DESCRICAO_SEXO), size = 3)+
  labs(title = "Total de candidaturas por sexo",
       subtitle = "Candidaturas masculinas e fem por UF",
       x = "UF",
       y = "Média de Arrecadação",
       caption = "Fonte: TSE",
       color = "Sexo")+
  theme_bw()+
  theme(text = element_text(size = 15), legend.position = "bottom")+
  guides(linetype = F, shape = F)
## esse guides com linetype e shape = false é pra tirar isso da legenda




