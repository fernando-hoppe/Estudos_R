## MODELOS REGRESSÃO LOGÍSTICA - AULA 2

# é uma extensão das técnicas de regressão linear empregando variáveis
# categóricas binárias. Variável dependente é geralmente 0 ou 1.

# Pertence a família dos Modelos Lineares Generalizados, assim como o probit,
# poisson, gama, etc.

## Esses modelos são usados quando a distribuição não é normal, mas sim
## BINOMIAL

## Modelos regressão linear: inferir Y a partir de valores de X.
## Modelos logísticos: inferir probabilidade de B em detrimento de A.

## A regressão logística estima as razões de chance de um evento ocorrer
# a partir do emprego de uma base logarítima das suas probabilidades

## Se modelos lineares estimam a reta (intercepto) da equação, modelos 
# logísticos ajustam a curva logística em formato de S.

# USOS:
#Epidemiologia: estimação razões de chance de doença acontecer
#Crédito: estimação de risco de inadimplência
#Seguradoras: estimação probabilidade de sinistro
#Política: estimação razões de chance de voto
#Linguagem Natural e Machine Learning: sistemas de classificação e treinamento de máquinas


# Pressuposto são mais relaxados pq a variável dependente é binária, 
# não sofrendo com heterocedasticidade.
# Algumas observações:
# Em geral recomenda-se amostra > 400 casos
# Razão mínima de 10 observações pra cada variável explicativa
# Independência das variáveis explicativas (uma forma de fazer isso é
# fazer uma matriz de correlação, visto na última aula)
# Proporção mínima maior que 15% nas categorias da variável dependente

## Prática: estiamr as razões de chance do voto em Bolsonaro nas eleições
# de 2018

## oprções
options(scipen = 999) #remove notação científica

## PACOTES ###
install.packages("sjlabelled") #remove os rótulos do spss
install.packages("sjPlot") #exibição de resultados de regressão
install.packages("caret") #roda diagnósticos das regressões
install.packages("performance") #diagnóstico
install.packages("InformationValue") #diagnóstico
library(sjlabelled)
library(haven)

setwd("~/Mestrado/R_Intermediário")

### Dados ###
bd <- read_sav("ESEB.sav")
bd <- remove_all_labels(bd)  #removeu os rótulos da bd

## Recodificando variáveis ##
# Variável dependente: precisa ser criada para fins de reg logísitca
bd$dep <- ifelse(bd$Q12P2_B != 2, 0, 1)
# Q12P2_B: declaração de voto do eleitor no 2o turno. 2 = Bolsonaro
# Ou seja, se voto bolsonaro no 2o turno = 1. 

# transformando a variável númerica em fator:
bd$dep <- as.factor(bd$dep)
levels(bd$dep)
# É importanto fazer isso pra q o valor de referencia seja 0. Assim,
# se estima a probabilidade de 1 em decorrencia de 0, a probabilidade
# de voto em bolsonaro em decorrencia dos demais votos


## Variáveis independentes (preditoras)
# Sexo #
bd$sexo <- ifelse(bd$D2_SEXO == 1, "Masculino", "Feminino")
## 1 se masculino, 0 se feminino
bd$sexo <- as.factor(bd$sexo)
levels(bd$sexo)
## Feminino é o valor de referência

# Idade #
bd$idade <- bd$D1A_ID

# Renda #
bd$renda <- ifelse(bd$D9 < 9999998, bd$D9, NA)
# ou seja, se a renda for menor q aquele valor é pra atribuir o próprio
# valor da variável. Se maior, atribuir NA

# Escolaridade #
bd$escol <- bd$D3_ESCOLA

# Não gosta do PT
bd$antipt <- ifelse(bd$Q1501 < 96, bd$Q1501, NA)
bd$antipt <- 10 - bd$antipt
# aqui no fim ele inverteu, pq a pergunta era "o quanto gosta do PT"


## Redução das variáveis do Banco ##
bd <- bd[c("dep","sexo","idade","renda","escol","antipt")]
summary(bd)

# Assim como em modelos lineares, é necessário remover NA
bd <- subset(bd, !is.na(renda))
bd <- subset(bd, !is.na(antipt))
# fez um subset de todos os dados em que renda e antipt != de NA
summary(bd)
# se perdeu uma caralhada de observações: pra lidar com isso geralmente
# se faz imputação (pelo valor médio)

### ESTIMAÇÃO DO MODELO (por glm) ###
mod <- glm(dep~sexo+idade+renda+escol+antipt, data = bd, family = "binomial")
# ta estimando a variavel dep em função daquelas outras.
# como glm tem muitos tipos, é preciso especificar a distribuição binomial

summary(mod)
## aí voce ve os coeficientes estimados, os desvios, o valor z e p-values
## veja q também dá o AIC, q serve de comparação com outros modelos. Maior melhor
# Como os resultados tão em termos logaritimos, precisam ser transformados p/ interpretação
# em resumo: idade, renda e escolaridade não eram fatores q diferenciavam o eleitor
# de bolsonaro
# os homens apresentaram maior probabilidade de votar em JB, assim como os que nutrem
# antipatia pelo PT

# pra determinar um valor é preciso fazer transformação exponencial

## Transformação dos Coeficientes ##
exp(mod$coefficients)
## aí já temos as chances do voto em JB. Ser do sexo masculino aumenta a chance de 
## votar em JB em 1.35x quando comparado c/ eleitor do sexo feminino.
## Um eleitor ser antipt aumenta em 1.33x a chance de votar em JB qndo comparado 
# com eleitor q nao nutre antipatia

## Outra forma de interpretar é transformar em %
1.35736388 - 1
# Ser homem aumenta em 35.7% as chances de votar em JB
1.33247656 - 1
# Ser antipt aumenta em 33.2% as chances de votar em JB
# variação de +1 na escala antipt aumenta em 33% a chance de votar JB


# Ajuste do Modelo ##
library(performance)
r2_mcfadden(mod)
r2_nagelkerke(mod)
r2_tjur(mod)
#interpretação do r2 é diferente em modelos logísticos (chama pseudo R2)
# pra modelos de humanas, 0.17 é um bom r2
## A literatura tem apontado q o mais preciso é o tjur
# O uso de r2 nessa caso serve apenas para comparação com outros modelos, vendo o
# mais ajustado

## Diagnósticos do Modelo ##
# Colinearidade
install.packages("car")
library(car)
vif(mod)
## tolera-se valores até 4. Acima de 4 considera-se colinearidade no modelo

# Importância das Variáveis Preditoras ##
library(caret)
varImp(mod)
## aí da pra ver qual das variaveis de maior importancia: antipt
## OBS: o modelo está mal ajustado, retirou-se varias variáveis q explicariam o voto
# para fins didáticos

### Criando Amostra de Treino e Teste ###
# é uma forma de verificar a taxa de acerto do modelo. A partir dos resultados do
#modelo de treino, se vai prever os valores do modelo de teste
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(bd), replace=TRUE, prob=c(0.7,0.3))
## separou a base de dados em 70% pra treino, 30% pra teste

treino <- bd[sample, ]
teste <- bd[!sample, ]

## Estimação do modelo de treino
modt <- glm(dep~sexo+idade+renda+escol+antipt, data = treino, family="binomial")
# ao inves de usar a base toda, usa apenas o q foi separado pra treino: 70%
summary(modt)
## resultados são ligeiramente diferentes (redução da amostra) mas parecidos

## Calculando a probabilidade prevista do voto em JB na base de teste##
previsto <- predict(modt, teste, type = "response")

## Calculando a melhor probabilidade de voto ##
library(InformationValue)
otimo <- optimalCutoff(teste$dep, previsto)[1]
otimo
# indica a linha de corte mais adequada pra se trabalhar
# É o melhor cutoff da base teste, variavel dependente, a partir do resultados
# da base de treino, opção 1 (voto no JB)

## a linha de corte ótima seria 49,6%.

## Matriz de confusão
# a partir do modelado na base de treino, tenta prever o valor da base de teste
confusionMatrix(teste$dep, previsto)
## o modelo previu 222 votos em não bolsonaro, 73 casos q seriam não bolsonaro mas
# votaram volsonaro; 81 votos q seriam JB mas não votaram; 169 q seriam JB e
# de fato votaram JB
## a diagonal 222 e 169 é quanto o modelo acertou.

prop.table(confusionMatrix(teste$dep, previsto)) #tabela de proporção
## 40% e 31% dos resultados previstos corretamente

# Calculando a sensibilidade (taxa de positivo verdadeira)
sensitivity(teste$dep, previsto)
# acertou os positivos verdadeiros em 67,6%

# Calculando a especificidade (taxa de negativo verdadeiro)
specificity(teste$dep, previsto)
## o modelo acerta mais o não voto em bolsonaro do que o voto em bolsonaro

## Calculando o Erro de Classificação
misClassError(teste$dep, previsto, threshold = otimo)
# o modelo ta errando 28,3% dos casos --> é alto

## Estimando a curva de ROC (características operação do receptor)
plotROC(teste$dep, previsto)
# varia de 0 a 1. Quanto + proximo de 1 mais preciso o modelo

## exibindo os resultados
library(sjPlot)
tab_model(mod)
## gera uma tabelinha bonitinha em formato html. valores exponenciados
tab_model(mod, p.style = "stars")
# mostra agora p value como ***
# outras opções: show.ci = F (omite intervalo de confiança)
# show.se = T : mostra o desvio padrão
# collapse.se = T : junta erro padrão e odds ratio na mesma coluna

plot_model(mod)
# visualização gráfica no ggplot. As alterações são feitas pelo ggplot

# FIM


