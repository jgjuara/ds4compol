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

# Limpieza de texto y tf-idf 

# grepl(pattern = "[^]", "hoy  vi el video de Clase del curso de ciencia de datos")

obras <- gutenberg_works(languages = "es", only_text = T, distinct = T)

fierro <- gutenberg_download(14765)

fierro$text
Encoding(fierro$text)

fierro$text <- iconv(fierro$text, "latin1", "UTF-8")
fierro$text

fierro$text <- tolower(fierro$text)

# construct an expression
regex_capitulo <- rx_start_of_line() %>% 
  RVerbalExpressions::rx_none_or_more() %>% 
  RVerbalExpressions::rx_seek_suffix(' -| .')
  
fierro <- fierro %>% 
  mutate(linea = row_number(),
         capitulo = cumsum(str_detect(text,
                                      pattern = '^(x|v|i)*(?=\\s-|\\.\\s|\\s\\.)'))) 

fierro <- fierro %>% 
  filter(capitulo != 0)

fierro <- fierro %>% 
  mutate(estrofa = cumsum(str_detect(text,
                                     pattern = rx_digit())))

# Un token es una unidad significativa de texto,
# generalmente una palabra, que nos interesa usar 
# para un análisis más detallado, y la tokenización
# es el proceso de dividir el texto en tokens.

fierro <- fierro %>% 
  unnest_tokens(tbl = ., drop = F, word, text)

fierro %>% view()

# La tokenización predeterminada es para palabras, pero
# otras opciones incluyen caracteres, n-gramas, oraciones, 
# líneas, párrafos o separación alrededor de un patrón de
# expresiones regulares.

# token = Built-in options are "words" (default), "characters",
#  "character_shingles", "ngrams", "skip_ngrams", "sentences",
#  "lines", "paragraphs", "regex", "tweets" 

fierro$word <- textclean::replace_non_ascii(fierro$word)

stop_words_es <- tm::stopwords(kind = "es") %>% textclean::replace_non_ascii()

stop_words_es_stem <- SnowballC::wordStem(stop_words_es, language = "es")

fierro %>% 
  filter(word %in% stop_words_es)

fierro <- fierro %>% 
  filter(!word %in% stop_words_es)

fierro <- fierro %>% 
  filter(!str_detect(word, pattern = "\\d"))

fierro$word %>% wordStem("spanish")

fierro %>% 
  filter(wordStem(word, "spanish") %in% stop_words_es_stem)

fierro <- fierro %>% 
  filter(!wordStem(word, "spanish") %in% SnowballC::wordStem(stop_words_es, language = "es"))

# fierro %>%
#   count(word = wordStem(word, "spanish"), capitulo, sort = TRUE) %>%

diccionario  <- fierro %>% 
  mutate(word_stem = wordStem(word, "spanish")) %>% 
  group_by(word_stem, word) %>% 
  summarise(n = n())

diccionario <- diccionario %>% 
  group_by(word_stem) %>% 
  slice_max(n = 1, order_by = n) %>% 
  select(word_stem, word)

capitulos <- fierro %>%
  count(word_stem = wordStem(word, "spanish"),
        capitulo, sort = TRUE) %>%
  filter(word_stem != "pa") %>% 
  group_by(capitulo) %>% 
  slice_head(n =10) %>% 
  left_join(diccionario) %>% 
  distinct(word_stem, capitulo, n, .keep_all = T) %>% 
  ungroup()

capitulos %>% 
  mutate(word = reorder_within(word,by =  n, within = capitulo)) %>% 
  # ungroup() %>% 
  ggplot(aes(n, word)) +
    geom_col(aes(fill = word_stem)) +
    labs(y = NULL) +
    # scale_x_discrete() +
    facet_wrap(~ capitulo, scales = "free") +
  guides(fill = "none")

capitulos_tf_idf <- capitulos %>%
  bind_tf_idf(term = word, document = capitulo, n = n)

fierro_tf_idf <- fierro %>% 
  ungroup() %>% 
  count(word, capitulo) %>% 
  bind_tf_idf(term = word, document = capitulo, n = n) %>% 
  arrange(capitulo)

capitulos_tf_idf %>% 
  # slice_max(tf_idf, n = 15) %>%
  mutate(word = reorder_within(word, by =  tf_idf, within = capitulo)) %>% 
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = word_stem )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~capitulo, scales = "free") +
  labs(x = "tf-idf", y = NULL)

fierro_tf_idf %>% 
  group_by(capitulo) %>% 
  arrange(-tf_idf) %>% 
  slice_head(n = 10) %>%
  ungroup() %>% 
  mutate(word = reorder_within(word,by =  tf_idf, within = capitulo)) %>% 
    ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = word )) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~capitulo, scales = "free") +
    labs(x = "tf-idf", y = NULL) +
    scale_fill_viridis_d()

fierro %>% 
  ungroup() %>% 
  count(word, capitulo) %>% 
  group_by(capitulo) %>% 
  arrange(-n) %>% 
  slice_head(n = 10) %>%
  ungroup() %>% 
  mutate(word = reorder_within(word,by =  n, within = capitulo)) %>% 
  ggplot(aes(n, fct_reorder(word, n), fill = word )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~capitulo, scales = "free") +
  labs(x = "n", y = NULL)
