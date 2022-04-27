#### APLICAÇÃO - PREVENDO OCORRÊNCIA DE DIABETES

# Objetivo: Identificar pacientes com alta probabilidade de serem diagnosticados 
#com diabetes, tendo, no mínimo, 75% de acurácia.

setwd("C:/Users/hoppe/Documents/Mestrado/Análise Dados R_ENAP")

diabetes <- read.csv(
  file = "C:/Users/hoppe/Documents/Mestrado/Análise Dados R_ENAP/diabetes.csv"
)

head(diabetes)


### PREPARAÇÃO DOS DADOS

#Verificando o tipo dos dados das colunas do dataset
str(diabetes)

#Verificando se existem valores não preenchidos
colSums(is.na(diabetes))

#Verificando a proporção dos valores de cada categoria
table(diabetes$Outcome)


#Alterando o tipo da coluna "Outcome" que é int para factor
diabetes$Outcome <- as.factor(diabetes$Outcome)

#Verificando valores min, max, média, mediana...
summary(diabetes$Insulin)

#Criando o gráfico de boxplot para cada coluna do dataset
boxplot(diabetes)

#Criando o boxplot apenas da coluna "Insulin"
boxplot(diabetes$Insulin)

#Criando um histograma da coluna "Insulin"
hist(diabetes$Insulin)

install.packages("dplyr")
library(dplyr)

#Filtrando o dataset por Insulin - Remoção de outliers
diabetes2 <- diabetes %>%
  filter(Insulin <= 250)

boxplot(diabetes2$Insulin)


### Análise exploratória ###

#Criação do boxplot para identificar outliers nas colunas do dataset
boxplot(diabetes2)

#Criação de histogramas para visualizar a distribuição dos dados
hist(diabetes2$Pregnancies)
hist(diabetes2$Age)
hist(diabetes2$BMI)

#Visualizando os valores de min, max, média, mediana...
summary(diabetes2$Insulin)


### CONSTRUÇÃO DO MODELO  #####

install.packages("caTools")
library(caTools)  #é pacote q permite usar sample.split

# Divisão dos dados em treino e teste - 70% dos dados para treino e 30% dos dados 
#para teste
set.seed(123) #não sei pra q serve isso
index = sample.split(diabetes2$Pregnancies, SplitRatio = .70) #divide 70%
index

train = subset(diabetes2, index == TRUE) #cria a bd de treino
test  = subset(diabetes2, index == FALSE) #cria a bd de teste

dim(diabetes2) #pra verificar se a divisão foi certa
dim(train)
dim(test)

install.packages("caret")
install.packages("e1071")

library(caret)
library(e1071)

?caret::train

#Treinando a primeira versão do modelo - KNN - K Nearest Neighbour
modelo <- train( #train (var resposta ~ variaveis preditoras (nesse caso, todas))
  Outcome ~., data = train, method = "knn")

#Visualizando os resultados do modelo
modelo$results  #acuracia pra cada valor de k
modelo$bestTune  #melhor valor de k


#Treinando a segunda versão do modelo - testando o comportamento do modelo 
#com outros valores de k
modelo2 <- train(
  Outcome ~., data = train, method = "knn",
  tuneGrid = expand.grid(k = c(1:20)))

#Visualizando os resultados do modelo
modelo2$results
#Identificando o melhor valor de k
modelo2$bestTune
# k de 16 obteve acuracia de 72%, o melhor

#Visualizando a performance do modelo - gráfico de linhas
plot(modelo2)



#Treinando a terceira versão do modelo - Naive bayes
install.packages("naivebayes")
library(naivebayes)

modelo3 <- train(
  Outcome ~., data = train, method = "naive_bayes")

#Visualizando os resultados do modelo
modelo3$results
modelo3$bestTune
### Acurácia de 75%


#Treinando a quarta versão do modelo - randomForest
install.packages("randomForest")
library(randomForest)

modelon4 <- train(
  Outcome ~., data = train, method = "rpart2"
)
modelon4
### modelo com acurácia abaixo de 75%

#Verificando a importância das váriaveis para o aprendizado do modelo
varImp(modelon4$finalModel)
#As colunas "Insulin e Blood Pressure" não contribuem muito para o aprendizado 
#do modelo  
## contribuição de 0

#Treinando o modelo sem as colunas "Insulin e BloodPressure" - train[,c(-3,-5)] 
#exclui as colunas
modelon4_1 <- train(
  Outcome ~., data = train[,c(-3,-5)], method = "rpart2"
)
modelon4_1
## acurácia diminui ainda mais

# Visualizando a arvore de decisão
plot(modelon4_1$finalModel)
text(modelon4_1$finalModel)


### modelo 5 com método radial sigma
install.packages("kernlab")
library(kernlab)

set.seed(100)
modelo5 <- train(
  Outcome ~., data = train, method = "svmRadialSigma"
  ,preProcess=c("center")
)

modelo5$results
modelo5$bestTune
### acurácia de 76% para o melhor modelo



##### Avaliando o modelo ####
?predict

#Testando o modelo com os dados de teste
predicoes <- predict(modelo5,test) #predict retorna as previsões com base no modelo
#treinado e os dados de teste

# Visualizando o resultado das prediçoes do modelo
predicoes

#Com as previões geradas pelo modelo, podemos comparar com os dados esperados do
#conjunto de dados de treino.

?caret::confusionMatrix
#Criando a confunsion matrix para Verificar os resultados do modelo
confusionMatrix(predicoes, test$Outcome) #calcula a diferença entre previstos e es-
#perados
### A acurácia do algoritmo ficou em 79% quando apresentado à novos dados!

##Agora q o modelo foi criado e validado, cria-se um df com a mesma estrutura, si-
#mulando dados de um novo paciente

# Realizando predições

#Criando um dataframe apenas com o registro de um unico paciente para simular a utilização do modelo
novos.dados <- data.frame(
  Pregnancies = c(3),           
  Glucose = c(111.50),
  BloodPressure = c(70),
  SkinThickness = c(20),          
  Insulin = c(47.49),
  BMI = c(30.80),       
  DiabetesPedigreeFunction = c(0.34),
  Age = c(28)                     
)

novos.dados

#Utilizando o modelo para gerar a previsão - passando os dados do paciente
previsao <- predict(modelo5,novos.dados)

resultado <- ifelse(previsao == 1, "Positivo","Negativo")

#Verificando o resultado da predição do modelo
print(paste("Resultado:",resultado))
## O resultando, segundo o modelo de predição, é negativo.


### VISUALIZAÇÃO DOS RESULTADOS

#Criando o arquivo com os resultados das predições
write.csv(predicoes,'resultado.csv')

#Lendo o arquivo de previsões que foi gerado
resultado.csv <- read.csv('resultado.csv')

#Alterando o nome das colunas do dataframe
names(resultado.csv) <- c('Indice','Valor previsto')

#Visualizando o dataframe
resultado.csv
