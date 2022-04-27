#### INTRO À MACHINE LEARNING - ALGORITMOS E ML

# Machine learning ou aprendizagem de máquina é uma representação que tem como 
#objetivo criar um modelo a partir de dados históricos para generalizar decisões

# Variável dependente: resposta, classe, label ou target
# Variável independente: features, atributos, dimensões

### Algoritmos de aprendizagem supervisionada, não supervisionada

## Algoritsmo de aprendizagem supervisionada: Na aprendizagem supervisionada, 
#a predição é estimada com base na relação entre os dados de entrada (features) e 
#os dados de saída (variável resposta). Para cada entrada, é apresentado o 
#resultado esperado. 
#A aprendizagem supervisionada pode ser utilizada para resolver problemas de 
#classificação e regressão. A classificação tem como resultado uma saída 
#categórica/discreta. Já a regressão tem como resultado uma saída numérica.

###### Tipos de aprendizagem supervisionada:
# Regressão Linear: O objetivo é prever o valor de uma variável contínua. 
#A regressão linear assume que existe uma relação linear entre a variável resposta 
#e a variável explicativa.

# K-Nearest Neighbors (KNN): algoritmo de classificação que se baseia nos vizinhos 
#mais próximos. Quando um novo dado é apresentado ao algoritmo, ele irá classificá-lo 
#com base nos exemplos mais próximos apresentados na fase de treinamento.
#O parâmetro k representa a quantidade de vizinhos mais próximos que deve ser 
#considerada pelo algoritmo.

# Árvore de Decisões: estrutura que armazena regras de decisão e possui nós, ramos 
#e folhas. Os nós representam as variáveis, os ramos representam os valores 
#possíveis de cada nó e as folhas representam o valor final de um nó.



## Algoritmos de aprendizagem não supervisionada: é usado quando não temos os 
#dados rotulados, ou seja, quando não temos a saída esperada para uma determinada 
#entrada. Assim, o algoritmo aprende sem informações adicionais para produzir 
#uma saída.
#Os algoritmos de aprendizagem não supervisionada são baseados em medidas de 
#similaridade e padrões ocultos nos dados. Normalmente, é utilizado para resolver 
#problemas de agrupamento (clustering), associação e detecção de anomalias.
#Clustering é uma atividade frequentemente utilizada para agrupar os dados 
#que possuem características similares.

# K-Means: o algoritmo se chama assim pois encontra k clusters diferentes no 
#conjunto de dados. O centro de cada cluster será chamado centroide e terá 
#a média dos valores neste cluster.


## Outros algoritmos de machine learning
# Regressão Logística | Classificação | Supervisionada
# Naive Bayes | Classificação | Supervisionada
# Árvore de Regressões | Regressão | Supervisionada
# Redes Neurais | Classificação/Regressão | Supervisionada
# Máquinas vetorias de suporte | classificação/regressão | Supervisionada
# Floresta Aleatória | classificação/regressão | Supervisionada
# PCA | Redução de Dimensionalidade | Não supervisionada
# Regras de associação | Detecção de padrões | Não supervisionada
# DBSCAN | Agrupamento | Não supervisionada


##### CONSTRUÇÃO DE MODELO PREDITIVO #####

# Dados precisam ser transformados e apresentados ao algoritmo
# Na fase de pré-processamento, dividem-se em dados de treino e dados de teste.

# Dados de treino: apresentados ao algoritmo para que ele aprenda o relacionamento 
#entre as variáveis e crie o modelo.
# Dados de teste: utilizados para avaliar o quanto o algoritmo aprendeu.

#Ao apresentar os dados de teste ao modelo, as previsões são realizadas com base no 
#que foi aprendido na fase de treinamento. Essas previsões são comparadas com as 
#respostas esperadas para calcular o desempenho do modelo. Uma vez criado e 
#validado, o modelo pode ser utilizado para realizar novas previsões quando for 
#apresentado a novos dados.

## Etapas da construção de modelo de machine learning:
# Pré-Processamento dos dados: melhorar a qualidade dos dados que serão apresentados ao algoritmo.
# Técnicas: feature selection, feature engineering, normalização, redução de
#dimensionalidade, divisão dos dados em treino e teste
# feature selection: selecionar os atributos mais relevantes que serão utilizados para treinar o modelo.
# feature engineering: É a arte de criar variáveis a partir de um conjunto de dados para melhorar a performance do modelo.
# Normalização: Um dataset pode conter variáveis em diferentes escalas e, assim sendo, recomenda-se padronizar esses dados para uma mesma escala

# Aprendizagem: o modelo é construído a partir dos dados que são apresentados ao algoritmo.
# Técnicas: Cross-Validation, Métricas de desempenho, Otimização de hiperparâmetros
# cross-validation: É utilizada para treinar e validar um modelo com o mesmo 
#conjunto de dados, dividindo-os em partições. a cada iteração, o algoritmo troca os 
#dados de treino e teste com o objetivo de obter um melhor desempenho.
# métricas de desempenho: ex: podemos medir a acurácia, que é o percentual de 
#previsões corretas em problemas de classificação.
# otimização: Cada algoritmo possui um conjunto de hiperparâmetros que podem ser 
#alterados. Assim, essa técnica busca encontrar a combinação certa de valores com 
#o objetivo de melhorar a performance do modelo.

# Avaliação do modelo: os dados de teste são apresentados ao modelo e, com isso, 
#são geradas previsões. Essas previsões são comparadas com os resultados desejados 
#para avaliar o desempenho do modelo.

# Predição: Se o modelo avaliado apresentar um bom resultado, poderá ser utilizado 
#para receber novos dados e realizar previsões.







