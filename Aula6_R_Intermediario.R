## AULA 6 - INTRODUÇÃO À CLUSTERIZAÇÃO

## Olhando pra gastos públicos, como podemos saber a priori o são gastos elevados?
# Uma técnica é usar clusters.
## Nós vamos replicar o q foi feito pelo prof no Mapas Contando Histórias

library(readxl)
library(tidyr)
library(tidyverse)
install.packages("viridis")
install.packages("viridisLite")
library(viridis)
library(purrr)
library(cluster)

setwd("C:/Users/hoppe/Documents/Mestrado/R_Intermediário")

##abrindo a base:
dados_munic_ibge_2010_2017 <- read_excel("PIB dos Municípios - base de dados 2010-2017.xls")

names(dados_munic_ibge_2010_2017)

## vamos selecionar apenas as variáveis relacionadas ao setor do pib:
dados_economicos <- dados_munic_ibge_2010_2017 %>%
  select(c(1,7,8,33:37,38,40))

## vamos renomear:
names(dados_economicos) <- c("ano","cod_mun","nome_mun","agro","industria","servicos","administracao",
                             "var_pib","pib_total","pib_pc")

## Vamos ver os valores máximos de algumas variáveis:
max(dados_economicos$agro)
max(dados_economicos$pib_total)
max(dados_economicos$pib_pc)
## perceba q são dados em escalas completamente diferentes. Por isso vamos reescalar:

dados_cluster <- dados_economicos %>% 
  filter(ano == 2017) %>% 
  mutate_at(c(4:10), ~(scale(.) %>% as.vector))
## filtramos pra pegar só 2017 e reescalona todas variaveis entre colunas 4 e 10

max(dados_cluster$agro)
max(dados_cluster$pib_total)            
max(dados_cluster$pib_pc)
## perceba q os dados agora não são tão discrepantes, estão mais ou menos na mesma escala
## pra conhecer mais desse processo tem q ver como funciona o scale

set.seed(1972)
## não vamos usar o método k-means pra fazer cluster (q se basearia no centróide)
## vamos usar outro método q usa medióide.
## o medioide usa uma observação de fato como centro. O centróide cria um centro 'virtual'

sil_info <- map_dbl(3:6, function(k){
  print(k)
  model_loop <- cluster::pam(dados_cluster[4:10], k=k)
  model_loop$silinfo$avg.width})

## cluster::pam(dados_cluster[4:10], k=k)
## pam indica o método (medioide), 4:10 indica as variáveis q serão clusterizadas
## o que estamos fazendo ai em cima é testando qual a quantidade ideal de clusters
## para esses dados e variáveis. Testamos de 1 a 6 usando o criério da silhueta
## Perceba q geramos objeto sil_info com os dados dessas silhuetas, quanto + perto
## de 1, melhor. A utilização de 6 clusters é portanto, mais adequada.
# quanto maior o valor, maior é associação entre os pontos
## a silhueta indica o grau de pertencimento de um objeto a um determinado cluster e
# e menor a chance daquele objeto ter sido colocado no cluster errado
# $silinfo$avg.width: vai pegar a largura média da silhueta dos pontos de cada cluster
## Poderíamos testar com mais clusters mas aí começaria a ficar muito confuso e os ganhos são baixos

model <- cluster::pam(dados_cluster[4:10], k = 6)
dados_cluster$cluster <- model$clustering
# criamos o objeto model q informa qual o cluster de cada município
# aí criamos a variável cluster dentro da base com essa informação. Veja:
names(dados_cluster)

### Aí precisamos reordenar os clusters, pq a ordem deles é aleatória:
## queremos que os municípios fiquem numa escala de 1 = mais pobre pra 6 = mais rico

dados_cluster_graph<-
  dados_cluster %>%
  gather(key = "item_pib", value = "valor",-c(1:3,11))%>%
  mutate(cluster =case_when(
    cluster ==5 ~1,
    cluster ==3 ~2,
    cluster ==1 ~3,
    cluster ==4 ~4,
    cluster ==2 ~5,
    cluster ==6 ~6
  ))

## cluster 5 é transformado em 1, cluster 3 em 2, cluster 1 em 3...

## AGora vamos fazer os gráficos representando os clusters por setor
dados_cluster_graph%>%
  mutate(cluster= factor(cluster)) %>% #cluster é numerico, transform em fator
  inner_join(
    dados_economicos%>%
      gather(key = "item_pib", value = "valor_original", -c(1:3))
  ) %>% #dados das colunas q não sejam 1 a 3 foram rotacionados usando inner_join com gather
  ggplot() +
  geom_jitter(aes(x=item_pib, y = valor, color=cluster), alpha=0.5) +
  scale_color_viridis(discrete=TRUE)+
  theme_light()+
  theme(
    axis.text.x = element_text(angle = 90),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  facet_wrap(cluster ~., scales = "free_y")  #escalas livres, ou seja, cada cluster independente

## Fizemos o gráfico basicão de cada cluster, agora vamos inserir as medióides
# perceba q no objeto model constam as medioides

#Os valores nas medioides:
model[["medoids"]]
## Perceba q o número dos cluster é o original, não o transformado

#Os IDs das medioides
model[["id.med"]]

#Exemplo, qual é o município do ID 4111?
dados_cluster[4111,]
# É mandirituba

## Portanto, o q queremos é identificar esses elementos desse vetos id.med

# Identificação dos munics medioides:
analise_cluster <- dados_cluster %>% mutate(id=row_number())
# Vamos criar uma nova coluna na base original chamada id q é o numero da linha
# e aí cria-se um novo objeto

## Agora posso criar um filtro com as linhas q se referem apenas ao meu vetor id.med
analise<-
analise_cluster %>%
  filter(id %in% model[["id.med"]]) %>%    ## filtrando pelos id.meds
  inner_join(    #essa parte é pra recuperar os dados originais, não reescalados
    dados_economicos%>%
      filter(ano == 2017), by = c("cod_mun" = "cod_mun")
  )
## criamos um objeto chamado analise pra poder visualizar a tabela, veja q além
# dos dados transformados, constam também os dados originais por conta do join


### Outros dados interessantes:
## No objeto model, se olharmos silinfo, clus.avg.width,
## vemos q o modelo 5 tem silhueta de 0.62
## ou seja, é o mais coeso. 5 é o grupo dos munics mais frageis
## Ou seja, considerando as variaveis conjuntamente, esse grupo é o mais coeso





