library(tidyverse)
library(tidytext)
library(stopwords)
library(pdftools)
library(syuzhet)
library(parallel)

# https://digitallibrary.un.org/search?ln=es&as=1&m1=p&p1=Resolutions+and+decisions+of+the+Economic+and+Social+Council&f1=series&op1=a&m2=a&p2=&f2=&op2=a&m3=a&p3=&f3=&dt=&d1d=&d1m=&d1y=&d2d=&d2m=&d2y=&rm=&ln=en&action_search=Search&sf=year&so=d&rg=50&c=United+Nations+Digital+Library+System&of=hb&fti=0&fti=0

# lee pdfs con texto (hay opcion para ocr)
ces_es_19 <- pdftools::pdf_text("2_analisis_de_textos/docs/E_2019_99-ES.pdf")
ces_es_20 <- pdftools::pdf_text("2_analisis_de_textos/docs/E_2020_99-ES.pdf")
ces_es_21 <- pdftools::pdf_text("2_analisis_de_textos/docs/E_2021_99-ES.pdf")

ces_en_19 <- pdftools::pdf_text("2_analisis_de_textos/docs/E_2019_99-EN.pdf")
ces_en_20 <- pdftools::pdf_text("2_analisis_de_textos/docs/E_2020_99-EN.pdf")
ces_en_21 <- pdftools::pdf_text("2_analisis_de_textos/docs/E_2021_99-EN.pdf")

# armo una funcion para simplificar
sentimiento_neto_es <- function(texto) {
  # cargo stopr words
  stopwords_es <- stopwords(language = "es",source = "snowball")
  
  # remueve stopwords
  texto <- tm::removeWords(texto, stopwords_es)
  
  # reemplaza los simbolos de corte de linea por espacios
  texto <- str_replace_all(texto, "\\n", " ")
  
  # pasa el vector a un tibble
  texto <- tibble(texto) 
  
  # tokeniza por palabras todo el documento
  texto <- texto %>%
    unnest_tokens(word, texto)
  # removemos los tokens que sean digitos o puntuacion
  # tener en cuenta que es str_detect detecta la presencia aunque no sea lo unico
  # aca es seguro de usar porque cada fila es un token
  texto <- texto %>%
    filter(!str_detect(word,pattern = "[:digit:]|[:punctuation:]"))
  # elimino tokens de 1 solo character
  texto <- texto %>%
    filter(str_length(word)>1)
  # replazo simbolos extraños que puedan haber quedado
  texto <- texto %>%
    mutate(word = textclean::replace_non_ascii(word)) %>%
    count(word)
  # esta parte es para usar varios nucleos en paralelo y acelerar el proceso
  nclusters <- detectCores() - 2
  cl <- makeCluster(nclusters) # or detect_cores() - 1
  clusterExport(cl = cl, c("get_sentiment", "get_sent_values", "get_nrc_sentiment", "get_nrc_values", "parLapply"))
  # lo importante es esta funcion que busca cada token en el diccionario
  # y te devuelve los valores que le corresponden
  texto_sentimiento <- get_nrc_sentiment(texto$word, cl=cl, language = "spanish")
  # bovary_nrc_par <- get_sentiment(bovary_v, method='nrc', cl=cl)
  stopCluster(cl)
  # armo dataframe con token + valores de sentimiento
  texto_sentimiento <- bind_cols(texto, texto_sentimiento)
  # objeto que devuelve mi funcion ad hoc
  texto_sentimiento
  
}

# idem para ingles
sentimiento_neto_en <- function(texto) {
  stopwords_en <- stopwords(language = "en",source = "snowball")
  
  texto <- tm::removeWords(texto, stopwords_en)
  
  texto <- str_replace_all(texto, "\\n", " ")
  
  texto <- tibble(texto) 
  
  texto <- texto %>%
    unnest_tokens(word, texto)
  
  texto <- texto %>%
    filter(!str_detect(word,pattern = "[:digit:]|[:punctuation:]"))
  
  texto <- texto %>%
    filter(str_length(word)>1)
  
  texto <- texto %>%
    mutate(word = textclean::replace_non_ascii(word)) %>%
    count(word)
  
  nclusters <- detectCores() - 2
  cl <- makeCluster(nclusters) # or detect_cores() - 1
  clusterExport(cl = cl, c("get_sentiment", "get_sent_values", "get_nrc_sentiment", "get_nrc_values", "parLapply"))
  texto_sentimiento <- get_nrc_sentiment(texto$word, cl=cl, language = "english")
  # bovary_nrc_par <- get_sentiment(bovary_v, method='nrc', cl=cl)
  stopCluster(cl)
  texto_sentimiento <- bind_cols(texto, texto_sentimiento)
  
  texto_sentimiento
  
}

# corro la funcion sobre cada documento
ces_en_20_sentimiento <- sentimiento_neto_en(ces_en_20)
ces_en_19_sentimiento <- sentimiento_neto_en(ces_en_19)
ces_en_21_sentimiento <- sentimiento_neto_en(ces_en_21)

ces_es_20_sentimiento <- sentimiento_neto_es(ces_es_20)
ces_es_19_sentimiento <- sentimiento_neto_es(ces_es_19)
ces_es_21_sentimiento <- sentimiento_neto_es(ces_es_21)

# les agrego el anio
ces_en_19_sentimiento <- ces_en_19_sentimiento %>% mutate(anio = 2019)
ces_en_20_sentimiento <- ces_en_20_sentimiento %>% mutate(anio = 2020)
ces_en_21_sentimiento <- ces_en_21_sentimiento %>% mutate(anio = 2021) 

ces_es_19_sentimiento <- ces_es_19_sentimiento %>% mutate(anio = 2019)
ces_es_20_sentimiento <- ces_es_20_sentimiento %>% mutate(anio = 2020)
ces_es_21_sentimiento <- ces_es_21_sentimiento %>% mutate(anio = 2021) 

# junto todos los docs en ingles
df_en <- bind_rows(ces_en_19_sentimiento,
                   ces_en_20_sentimiento,
                   ces_en_21_sentimiento)

# junto todos los docs en español
df_es <- bind_rows(ces_es_19_sentimiento,
                   ces_es_20_sentimiento,
                   ces_es_21_sentimiento)

# comparacion entre documentos y entre idiomas

# comparo el peso de palabras neutras sobre el total de los docs
df_en %>% 
  group_by(anio, neutras = positive == 0 & negative == 0) %>% 
  summarise(n = sum(n)) %>% 
  mutate(peso = n/sum(n))

df_es %>% 
  group_by(anio, neutras = positive == 0 & negative == 0) %>% 
  summarise(n = sum(n)) %>% 
  mutate(peso = n/sum(n))

# en ambos casos el 2020 tiene el menor numero absoluto de palabras con carga de sentimiento
# y el 2021 tiene el mayor numero absoluto
# en términos relativos el menor peso de palabras carga de sentimiento está en 2020
# y el mayor en 2019.

# Sin embargo en inglés la proporción de palabras con carga de 
# sentimiento está es del 20% en promedio mientras que es español es de 12% aprox.
# esto puede ser un sesgo del diccionario.

df_en %>% 
  group_by(neutras = positive == 0 & negative == 0) %>% 
  summarise(n = sum(n)) %>% 
  mutate(peso = n/sum(n))

df_es %>% 
  group_by(neutras = positive == 0 & negative == 0) %>% 
  summarise(n = sum(n)) %>% 
  mutate(peso = n/sum(n))

# calculo el sentimiento neto
df_en <- df_en %>% mutate(neto = positive-negative, sentimiento_peso = neto*n)
df_es <- df_es %>% mutate(neto = positive-negative, sentimiento_peso = neto*n)

# df_es <- df_es %>% filter(!word %in% c("consejo", "general","informe"))

# calculo el sentimiento neto por año

df_en %>% 
  group_by(anio) %>% 
  summarise(sentimiento = sum(sentimiento_peso)) %>% 
  mutate(idioma = "ingles")

df_es %>% 
  group_by(anio) %>% 
  summarise(sentimiento = sum(sentimiento_peso)) %>% 
  mutate(idioma = "español")

# armo resumencito por idioma y año para graficar
# calculo un sentimiento ponderado por proporcion de modo que 
# si todas las palabras tienen valor positivo 1 la suma da 1
# si todas las palabras tienen valor negativo -1 la suma da -1

resumen_en <- df_en %>% 
  group_by(anio) %>% 
  mutate(sentimiento_prop = sentimiento_peso/sum(n)) %>% 
  summarise(sentimiento = sum(sentimiento_peso),
            sentimiento_prop = sum(sentimiento_prop)) 
resumen_en
resumen_es <- df_es %>% 
  group_by(anio) %>% 
  mutate(sentimiento_prop = sentimiento_peso/sum(n)) %>% 
  summarise(sentimiento = sum(sentimiento_peso),
            sentimiento_prop = sum(sentimiento_prop))
resumen_es
# junto los resumenes
resumen <- bind_rows(
  resumen_en %>% mutate(idioma = "ingles"),
  resumen_es %>% mutate(idioma = "español")
)

# los grafico
p1 <- resumen %>% 
  ggplot() +
  geom_col(aes(x = anio, y = sentimiento, fill = anio)) +
  facet_wrap(~ idioma, nrow = 2) +
  guides(fill = "none")

p2 <- resumen %>% 
  ggplot() +
  geom_col(aes(x = anio, y = sentimiento_prop, fill = anio)) +
  facet_wrap(~ idioma, nrow = 2) +
  guides(fill = "none")

# esto es de un paquete "cowplot" para juntar plots
cowplot::plot_grid(p1, p2)

# ahora veamos el top 5 positivo y negativo de palabras por año e idioma
top_es <- bind_rows(
  df_es %>% 
   select(anio, word, n, neto, sentimiento_peso) %>% 
   group_by(anio) %>% 
   slice_max(order_by = sentimiento_peso, n = 5, with_ties = F),
  df_es %>% 
    select(anio, word, n, neto, sentimiento_peso) %>% 
    group_by(anio) %>% 
    slice_min(order_by = sentimiento_peso, n = 5, with_ties = F))

top_en <- bind_rows(df_en %>% 
                      select(anio, word, n, neto, sentimiento_peso) %>% 
                      group_by(anio) %>% 
                      slice_max(order_by = sentimiento_peso, n = 5, with_ties = F),
                    df_en %>% 
                      select(anio, word, n, neto, sentimiento_peso) %>% 
                      group_by(anio) %>% 
                      slice_min(order_by = sentimiento_peso, n = 5, with_ties = F))


tops <- bind_rows(
  top_en %>% mutate(idioma = "ingles"),
  top_es %>% mutate(idioma = "español")
)

tops %>% 
  # esta funcion 'reorder_within' es importante para que words quede correctamente ordenada
  mutate(word = reorder_within(x = word, within = list(idioma, anio), by = sentimiento_peso)) %>%
  ggplot() +
  geom_col(aes(x = word, y = sentimiento_peso, fill = sentimiento_peso)) +
  scale_fill_viridis_c() +
  coord_flip() +
  facet_wrap(idioma ~ anio, scales = "free_y") +
  guides(fill = "none")

# denle zoom al grafico para poder verlo bien

# veamos que lo que parece cambiar más son las palabras negativas, a partir de 
# año 2020 aparece enfermedad, disease y pandemic como terminos importantes de carga negativa

# vemos tmb que en los terminos positivos hay palabras que parecen ser más bien institucionales
# como consejo, council, assembly, general, informe.

df_en <- df_en %>% 
  filter(! word %in% c("united", "council", "general", "assembly"))
df_es <- df_es %>% filter(!word %in% c("consejo", "general","informe"))

# repito exploracion anterior

df_en %>% 
  group_by(anio) %>% 
  summarise(sentimiento = sum(sentimiento_peso)) 

df_es %>% 
  group_by(anio) %>% 
  summarise(sentimiento = sum(sentimiento_peso))

resumen_en <- df_en %>% 
  group_by(anio) %>% 
  mutate(sentimiento_prop = sentimiento_peso/sum(n)) %>% 
  summarise(sentimiento = sum(sentimiento_peso),
            sentimiento_prop = sum(sentimiento_prop)) 

resumen_es <- df_es %>% 
  group_by(anio) %>% 
  mutate(sentimiento_prop = sentimiento_peso/sum(n)) %>% 
  summarise(sentimiento = sum(sentimiento_peso),
            sentimiento_prop = sum(sentimiento_prop))

resumen <- bind_rows(
  resumen_en %>% mutate(idioma = "ingles"),
  resumen_es %>% mutate(idioma = "español")
)

p1 <- resumen %>% 
  ggplot() +
  geom_col(aes(x = anio, y = sentimiento, fill = anio)) +
  facet_wrap(~ idioma, nrow = 2) +
  guides(fill = "none")

p2 <- resumen %>% 
  ggplot() +
  geom_col(aes(x = anio, y = sentimiento_prop, fill = anio)) +
  facet_wrap(~ idioma, nrow = 2) +
  guides(fill = "none")

cowplot::plot_grid(p1, p2)

top_es <- bind_rows(df_es %>% 
                      select(anio, word, n, neto, sentimiento_peso) %>% 
                      group_by(anio) %>% 
                      slice_max(order_by = sentimiento_peso, n = 5, with_ties = F),
                    df_es %>% 
                      select(anio, word, n, neto, sentimiento_peso) %>% 
                      group_by(anio) %>% 
                      slice_min(order_by = sentimiento_peso, n = 5, with_ties = F))

top_en <- bind_rows(df_en %>% 
                      select(anio, word, n, neto, sentimiento_peso) %>% 
                      group_by(anio) %>% 
                      slice_max(order_by = sentimiento_peso, n = 5, with_ties = F),
                    df_en %>% 
                      select(anio, word, n, neto, sentimiento_peso) %>% 
                      group_by(anio) %>% 
                      slice_min(order_by = sentimiento_peso, n = 5, with_ties = F))


tops <- bind_rows(
  top_en %>% mutate(idioma = "ingles"),
  top_es %>% mutate(idioma = "español")
)

tops %>% 
  mutate(word = reorder_within(x = word, within = list(idioma, anio), by = sentimiento_peso)) %>%
  ggplot() +
  geom_col(aes(x = word, y = sentimiento_peso, fill = sentimiento_peso)) +
  scale_fill_viridis_c() +
  coord_flip() +
  facet_wrap(idioma ~ anio, scales = "free_y") +
  guides(fill = "none")

# veamos que ahora cambio un poco el top positivo sobre todo
# pero podría refinarse más todavía y evaluarse también en función a otras categorías de sentimientos
# de todas formas era ver que los diccionarios implican ciertas diferencias entre idiomas
# y también deben ser ajustados en función del género de textos que estemos usando


