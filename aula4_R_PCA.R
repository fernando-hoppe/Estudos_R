### aula 4 - Análise de Componente Principal (PCA)
### a ideia do pca é reduzir ou simplificar modelos
## caso vc tenha muitas variáveis num modelo, aplicando PCA é possível identificar os componentes principais,
# excluindo aqueles não tão relevantes para o modelo.


## verifica as primeiras linhas da base
head(iris)

## Passo 0 - Configurando ambiente
#Antes de começar a implementação configure o _workspace_ e importe todos os pacotes e execute o preprocessamento
# da base

# Extraindo informações da base, selecionando só as colunas 1 a 4. Coluna 5 não é uma variável. Não tem nada
# antes da virgula, o q significa q pega todas as linhas.
dados <- iris[,1:4]

dim(iris)
## dá a dimensão da base, 150 linhas e 5 colunas

dim(dados)

summary(iris)

## PASSO 1 - Análise de Componentes Principais

pca <- prcomp(x = dados, center = TRUE, scale. = FALSE)
## define a base 'dados' como entrada, define que está centralizado, scale. = false significa que não é pra 
# normalizar os dados.
## perceba q rodando não ocorre output nenhum

print(pca)
## aí ele vai dar uma matriz de valores pra cada variável. Pra entender isso aí é melhor fazer:
summary(pca)
## o retorno vai ser Importância dos componentes, dando desvio padrão pra cada componente, a proporção de 
# variância e a proporção cumulativa.
# Veja que a proporção da variancia do PC1 é muito grande, do PC4 e 3 é bem pequena.

## colocando isso num gráfico:
plot(pca, type = 'l', pch = 16, main = "Scree plot")
## Percebe-se q com 1 componente temos 4 variâncias
# com dois já cai pra menos de 1 e com 4 se aproxima de 0
# Portanto, dá pra ve q com 1 componente é possível explicar
## praticamente toda a variancia acumulada

## OU SEJA, 4 variáveis não são necessárias, apenas uma
## já é responsável por quase toda a variância.

## despois de gerar um gráfico é sempre interessante 
# clicar no x pra remove-lo e evitar sobreposição.


par(bg=NA); biplot(pca)
## gera autovetores: petal.lenght e petal.width estão em
## cima do mesmo vetos, portanto eles devem estar dando
## informações muito parecidas
## Agora, sepal.witdth e sepal.lenght tem vetores em direções
## bem diferentes
## bg= cor de fundo

## PASSO 2 - ANALISE DE COMPONENTES PRINCIPAIS E NORMALIZAÇÃO

# Executando a redução de dimensionalidade com o prcomp com normalização de dados

# função prcomp para criar os autovetores e autovalores da base de dados usando
# a normalização dos atributos, isto é, scale. = TRUE

# redução da dimensionalidade:
pca2 <- prcomp(x = dados, center = TRUE, scale. = TRUE)

print(pca2)
## percebe q os valores são diferentes em relação ao pca, sendo q só mudamos a normalização

summary(pca2)
## perceba a diferença, agora a proporção da variância do pc1 caiu pra 0.72;
## já o pc2 subiu pra 0.228. Assim, fazendo o ajuste da normalização, talvez fizesse mais sentido
## usar os dois componentes, ao invés de apenas 1

# veja no gráfico
plot(pca2, type = 'l', pch = 16, main = "scree plot")
# componente 1 com 3 variâncias e componente 2 com uma variância.

## RESULTADOS
# O conjunto de dados de 4 variáveis pode ser simplificado para uma única (PCA1, eigenvalue>4)
# Foi possível melhor visualizar o conjunto de dados, permitindo verificar visualmente dois grupos de lírios
# Foi possível construir um modelo preditivo, por meio do qual baseado nas medições de
# tamanho e comprimento das sépalas e pétalas, informa-se qual espécie de lírio determinada
#planta pertence

# A única variável necessária é, portanto, PC1

# escalonamento (normalização) faz com que cada atributo passe a ter variância unitária, de
# modo a equilibrar a contribuição de cada uma delas na analise de componentes principais.
# Quando se tem features que tem alta variancia comparado com as demais, a grande variância desses
# pequenos grupos vai enviesar os autovetores. Aí talvez seja válido normalizar 

# Por outro lado, essa normalização vai diminuir a variância acumulada nos primeiros componentes,
# o que faz com que, em geral, tenha-se q considerar mais atributos para explicar os dados.


### PASSO 3
# Aplique a redução de dimensionalidade com a técnica PCA e gere um gráfico de dispersão dos
# dados. As colunas 1 a 4 são usadas

# gerando o gráfico de dispersão:
colors <- rainbow(length(unique(as.factor(iris[,4]))))
names(colors) <- unique(iris[,4])

## plotando o gráfico. lembrar que definimos que x = dados. Com pca$x definimos q queremos
#apenas pca1 e pca2. Main = título. Colors=as.factor - um de cada cor
{plot(pca$x[,1:2], main = "PCA", col=colors[as.factor(iris[,4])])
  
  text(pca$x[,1:2], labels=as.factor(iris[,4]), col=colors[as.factor(iris[,4])], cex=0.5)}

## Perceba q uma espécie é totalmente distinguivel das outras duas
# mas o verde e o azul/roxo ficam mais ou menos juntos

### normalizando a especie verde já fica um pouco mais diferenciada da azul/roxa
{plot(pca2$x[,1:2], main = "PCA", col=colors[as.factor(iris[,4])])
  
  text(pca2$x[,1:2], labels=as.factor(iris[,4]), col=colors[as.factor(iris[,4])], cex=0.5)}

## aí ela fez o mesmo exercício pra uma base bem maior, 600 colunas
## a analise de componente demonstrou q já no componente 38 a variância acumulada chegava a 88%
## logo, daria pra trabalhar apenas com essas 38 variáveis

## PC1 E PC2 não são as variáveis em si, é uma combinação: autovalor e autovetor

