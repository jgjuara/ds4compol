library(topicmodels)
library(tidytext)
library(tm)
library(tidyverse)

# Una de las estructuras más comunes con las que trabajan los paquetes de minería
# de texto es la matriz de término de documento (o DTM). Esta es una matriz donde:
# cada fila representa un documento (como un libro o un artículo),
# cada columna representa un término, y cada valor (típicamente) contiene 
# el número de apariciones de ese término en ese documento.

data("AssociatedPress")
AssociatedPress

# tidy() convierte una matriz de término de documento en un dataframe
# Este verbo proviene del paquete broom ( Robinson 2017 ) , que proporciona funciones 
# de limpieza similares para muchos modelos y objetos estadísticos.

tidy(AssociatedPress) %>% head()

# cast() convierte un dataframe de un término por fila en una matriz. 
# tidytext proporciona tres variaciones de este verbo, cada una de las cuales se 
# convierte en un tipo diferente de matriz: cast_sparse()(convertir en una matriz 
# dispersa del paquete Matrix), cast_dtm()(convertir en un DocumentTermMatrixobjeto
# de tm) y cast_dfm()(convertir en un dfmobjeto de quanteda).

terms <- Terms(AssociatedPress)
head(terms)

# LDA

# La asignación latente de Dirichlet (LDA) es un método 
# para ajustar un modelo de temas. Trata cada documento como una mezcla 
# de temas y cada tema como una mezcla de palabras. Esto permite que
# los documentos se "superpongan" entre sí en términos de contenido, 
# en lugar de separarse en grupos discretos, de una manera que refleja 
# el uso típico del lenguaje natural.
# Las técincas de Modelado de Tópicos tratan de captar los temas de los 
# que habla un corpus de texto. 
# LDA es un modelo de tipo inferencial bayesiano. 
# Cada documento es una mezcla de temas. Imaginamos que cada documento
# puede contener palabras de varios temas en proporciones particulares. 
# Por ejemplo, en un modelo de dos temas, podríamos decir: "El documento 1 tiene 
# un 90 % del tema A y un 10 % del tema B, mientras que el documento 2 tiene
# un 30 % del tema A y un 70 % del tema B".Cada tema es una mezcla de palabras.

# ajustar el modelo

# set a seed so that the output of the model es reproducible
# el param k define la cantidad de tópicos a identificar (dimensiones latentes)
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda %>% str()

# Probabilidades palabra-tema
# el param beta indica la probabilidad estimada para este termino en ese topico
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics %>% head(10)

# exploracion
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% # selecciona top 10
  ungroup() %>%
  arrange(topic, -beta) # ordena

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>% # ordena term para graficar correctamente
  ggplot(aes(x = beta, y = term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") + # divide en el grafico en paneles x topico
  scale_y_reordered() # escala para usar con reorder_within

# Como alternativa, podríamos evaluar la diferencia en el param beta de un mismo
# término entre los 2 topicos, de forma de identificar el diferencial entre tópicos
# de forma similar a la metrica tf-idf lo ideal es usar el log(beta2/beta1)

# rearmamos el df 
beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

# 
beta_wide


beta_wide %>% 
  filter(abs(log_ratio) > 10) %>% #selecciono terms con log mayor a 10
  mutate(prevalecencia = ifelse(log_ratio > 0, "topic2", "topic1"), #etiqueta
         term = fct_reorder(term, log_ratio)) %>% #orden para graficar bien eje x
  ggplot() +
    geom_col(aes(x = term, y = log_ratio, fill = prevalecencia)) +
  coord_flip()

# comparar con grafico anterior


# el param gamma del modelo expresa una proporción estimada de palabras de 
#  un documento que se generan a partir de un tema modelado. Es decir,
# LDA también modela cada documento en función de los temas

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents %>% arrange(-document) %>% head(10) # documentos con maximos valores de gamma 








