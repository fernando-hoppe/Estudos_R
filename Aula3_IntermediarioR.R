## AULA 3 - MANIPULAÇÃO DE DADOS

## Muito antes de visualizar e analisar dados (sql, power bi, tableau, etc) é necessário
## manipular os dados (ETL/ELT)
# ETL: extrair, transformar e carregar

# Obter boa fonte de dados; carregá-los; realizar algumas limpezas e manipulações pra q
# os dados estejam preparados p/ analise; por fim, execução de modelos preditivos, 
# visualização de dados, tabelas agregadas, relatórios etc.

## Tidyverse: pacote guarda-chuva q consolida uma série de ferramentas q fazem parte
# do ciclo da ciência de dados;
# Tidyverse: ggplot2, dplyr, tidyr, purrr, readr... TRABALHAM EM CONJUNTO
#tibble: para data frames repaginados;
#readr: para importar bases p/ R;
#tidyr e dplyr: pra arrumação e manipulação de dados;
#stringr: pra trabalhar com textos;
#forcats: pra trabalhar com fatores;
#ggplot2: gráficos;
#purrr: pra programação funcional

## PASSO A PASSO:
# Importar -> Arrumar -> (->transformar->visualizar->modelar->) -> Automatizar e Comunicar

#Importar: readr, xml2, haven, readxl
#Arrumar: tidyr, janitor, tibble
#Transformar: dplyr, stringr, forcats, lubridate, data.table
#Visualizar: ggplot2
#Modelar: caret, mlr, lme4, .keras
#Comunicar: rmarkdown, shiny
#Automatizar: plumber, opencpu

#####   %>% -> magrittr #####

#####################################

# Carregando Tidyverse
library(tidyverse)
tidyverse_packages() #mostra quais pocotes vc já tem

## Importando dados (geralmente usariamo readr, mas a bd é do próprio RBase)
data("iris")

iris %>% head() #comande praxe pra conhecer as variaveis. Mostra as 6 primeiras linhas

iris %>% sample_n(15) #pra mostrar amostras aleatórias

### %>%: basicamente diz o q queremos da bd

## olhando a amostra, parece q temos 3 tipos de especies de folhas:
## setosa, virginica e versicolor.
## Queremos diferenciá-las pelas outras variáveis.

## É necessário ter essas 4 variáveis p/ distinguir essas 3 espécies?

iris %>%
  group_by(Species) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  pivot_longer(-Species) %>%
  arrange()

## com esse código acima agrupamos tudo por espécie, sumarizamos pela média se o 
## valor não for NA, desagrupamos, aí fazemos um pivo de acordo com as especies e
# arranjamos. Ungroup() é mais um vicio de programação

# Veja: deu a média pra cada característica de acordo com cada tipo de folha.

install.packages("styler") #padrão tidyverse de identação. Deixa o código + bonito


### Transformando medidas
iris %>%
  group_by(Species) %>%  #agrupando por especies
  mutate(comprimento_petala_polegadas = Petal.Length / 2.54) %>% #transformação dos dados de cm para polegadas
  summarise_if(is.numeric, mean) #sumarizando pelas médias, omitindo os NAs ou letras
#Perceba q criou uma nova variável: comprimento_petala_polegada

### Outra opção: across = sumario para as colunas - usado qndo temos várias colunas
iris %>%
  group_by(Species) %>%
  mutate(comprimento_petala_polegadas_1 = Petal.Length / 2.54) %>%
  summarise(across(where(is.numeric), mean))
## é exatamente a mesma coisa


### Para conhecer quantas observações são de fato numéricas:
iris %>% select_if(is.numeric)
## deu 150, logo, todas as observações são numéricas

### outra opção
iris %>% filter(!is.na(Petal.Length))

iris %>% count(!is.na(Petal.Length))
## Fez a contagem de todas observações numéricas

### Podemos fazer PCA?
iris_pca <- iris %>%
  select_if(is.numeric) %>% #pra selecionar todos os valores numéricos
  prcomp() #comando q faz o pca

iris_pca
# É uma forma mais simplificada de fazer o q fizemos na outra aula de PCA

### Filtrando os dados

filter(iris, Sepal.Length == 7)

filter(iris, Petal.Length == 3)

filter(iris, Petal.Length == 1.5)

#acima vimos q apenas uma observação tem sepal.lenght igual a 7.
# apenas uma observação com petal.length igual a 3
# 13 observações com petal.length = a 1.5

iris2 <- filter(iris, Petal.Length > 3)
## Acima, criou objeto só com petal.length maior q 3
iris_sml <- select(iris2, Species, Sepal.Length)
# Acima, criou novo objeto baseado no iris2 pegando só variaveis de espécie e sepala


### Analises gráficas

iris %>%
  select_if(is.numeric) %>%
  pairs(col = iris$Species) #apresenta correlações - simples. 
# pairs: Cria pares divididos por especie
## resulta numa matriz


## agora usando o psych
install.packages("psych")
iris %>% psych::pairs.panels() #correlação e pearson e histograma e regressão
# dá pra ver q só sepal.width tem uma distribuição certinha
# dá pra ver tbm q algumas correlações são bem altas
# sepal.length com sepal.width: correlação fraca
# sepal.length com petal.length: correlação alta
# especie correlaciona fortemente com petal.length e petal.width


#Usando o GGally
install.packages("GGally")

iris %>% GGally:: ggpairs() #gráficos de distribuição
## medidas de correlação, diagrama, regressão e boxplot
## distribuição e correlação já tivemos no gráfico anterior
## diferencial aqui é o boxplot
## boxplot tbm é uma ferramenta pra visualizar se é possível diferenciar uma especie da outra
## sepal.length dá pra diferenciar
## sepal.width já fica dificil
## petal.legth dá pra diferenciar
## petal.width tbm já fica mais difícil

##Usando o GGally mas distinguindo as espécies
iris %>% GGally:: ggpairs(mapping = ggplot2::aes(colours = Species))
## agora distinguiu as especies por cores
## Dá pra perceber q não faz sentido analisar as características sem distinguir por espécie
## Veja q correlação de sepal.width com sepal.length tinha dado muito baixo 0,118
## Mas na verdade a correlação é média/forte considerando a espécie setosa
## Petal.length com sepal.width tinha dado alta, mas é fraca considerando setosas


## Voltando pra questão inicial:
# Usando esse gráfico, o PCA e modelos lineares mistos, chega-se a conclusão de que
# com duas variáveis essa análise poderia ser feita. 
# Essas variáveis são um combo dessas 4. São duas variáveis agregadas dessas 4.








