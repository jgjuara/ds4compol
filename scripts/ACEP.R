library(tidyverse)
# install.packages("tidytext")
library(tidytext)
# devtools::install_github("ropensci/gutenbergr")
library(gutenbergr)
# install.packages("RVerbalExpressions")
library(RVerbalExpressions)
# install.packages("tm")
library(tm)
# install.packages("textclean")
library(textclean)
library(SnowballC)
library(ACEP)
# install.packages("syuzhet")
library(lubridate)
library(syuzhet)

# syuzhet::get_nrc_sentiment(language = "spanish")

get_sentiments()
ACEP::acep_bases$ln_arg

notas_ln <- acep_load_base(acep_bases$ln_arg)

# notas_ln <- read_rds("2_analisis_de_textos/notas_ln.rds")
# notas_ln %>% write_rds("2_analisis_de_textos/notas_ln.rds")

notas_ln %>% head() %>% view()

dicc_confl_sismos <- acep_diccionarios$dicc_confl_sismos

notas_ln %>% head()
notas_ln$fecha %>% summary()

diciembres <- notas_ln %>% filter(month(fecha) == 12) %>% 
  group_by(año = year(fecha)) %>% 
  slice_sample(n = 100) %>% ungroup() %>% select(-año)

diciembres$texto[1]

diciembres$conflictos <- acep_men(diciembres$texto, dicc_confl_sismos)

diciembres %>% 
  group_by(fecha) %>% 
  summarise(sum(conflictos))

# diciembres %>% 
#   group_by(fecha) %>% 
#   summarise(confictos = sum(str_detect(tolower(texto), dicc_confl_sismos)))

diciembres_token <- diciembres %>% 
  unnest_tokens(word, texto)

diciembres_token <- diciembres_token %>% 
  filter(!str_detect(word, pattern = "\\d"))

stop_words_es <- tm::stopwords(kind = "es") %>% textclean::replace_non_ascii()

diciembres_token <- diciembres_token %>% 
  filter(!word %in% stop_words_es)

tokens <- diciembres_token %>% 
  count(word, titulo)

sentimientos <- syuzhet::get_nrc_sentiment(tokens$word[1:1000],
                                           language = "spanish")

tokens <- bind_cols(tokens[1:1000,], sentimientos)

notas_sentimientos <- tokens %>% 
  group_by(titulo) %>% 
  summarise(across(.cols = 3:12, .fns = sum))

notas_sentimientos <- notas_sentimientos %>% 
  filter(!if_all(.cols = where(is.numeric),
                 .fns = ~ . == 0))

  plot <- notas_sentimientos %>% 
    slice_sample(n = 20) %>% 
  mutate(
         indice = positive -negative) %>% 
    arrange(indice) %>% 
    ggplot(aes(x = titulo, y = indice, fill = indice>0)) +
      geom_col() +
      coord_flip() +
      scale_fill_viridis_d()

diciembres %>% 
  group_by(fecha) %>% 
  summarise(conflictos = sum(conflictos)) %>% 
  ggplot(aes(x = fecha, y = conflictos)) +
  # scale_x_discrete() +
  geom_col()



## Otro ejercicio

