library(tidyverse)
library(tidytext)
library(stopwords) 
library(textclean)
library(glue)
library(igraph)
library(ggraph)
library(topicmodels)
library(syuzhet)

syuzhet::get_nrc_sentiment()

sdal_dictionary <- read_csv2("sdal.csv")

sdal_dictionary <- sdal_dictionary %>% 
  separate(col = Word_O, sep = "_",
           into = c("token", "funcion"))

df <- read_rds("scripts/discursos_rosada.rds")

df <- df %>% 
  select(fecha_discurso, texto)

stopwords_es <- stopwords("es", "snowball")

stopwords_es <- textclean::replace_non_ascii(stopwords_es)

# stopwords_es <- stopwords_es[! stopwords_es %in% c("no", "sin")]

stopwords_es <- unique(stopwords_es)

stopwords_es_btws <- sapply(stopwords_es, function(x) {paste0(" ", x, " ")})
stopwords_es_start <- sapply(stopwords_es, function(x) {paste0("^",x, " ")})
stopwords_es_end <- sapply(stopwords_es, function(x) {paste0(" ",x, "$")})


df <- df %>% mutate(texto_clean = replace_non_ascii(tolower(texto)))

df <- df %>% mutate(texto_clean = str_remove_all(texto_clean,
                                                          "[:punct:]"))


df <- df %>% mutate(texto_clean = str_remove_all(texto_clean,
                                                 "[:digit:]"))



df <- df %>% 
  mutate(texto_clean = mgsub_regex(texto_clean, c(stopwords_es_btws,
                                                    stopwords_es_end,
                                                    stopwords_es_start),
                                    " "))


df <- df %>%
  mutate(texto_clean = str_squish(texto_clean))

# df <- df %>%
#   mutate(texto_clean = str_replace_all(texto_clean, "s |s$", " "))
# 
# df <- df %>%
#   mutate(texto_clean = str_squish(texto_clean))

df <- df %>% 
  mutate(anio =  str_extract_all(fecha_discurso, "\\d{4}") %>% 
           unlist() %>% as.numeric()) 

# tokens ------------------------------------------------------------------

df <- df %>% 
  mutate(id = 1:n())

tokens_df <- df %>% 
  unnest_tokens(input = texto_clean,
                output = token
  )





# token ppal x anio -----------------------------------------------------

ppal_token_anio <- tokens_df %>% 
  group_by() %>% 
  count(token) # %>% 


ppal_token_anio <- tokens_df %>% 
  group_by(anio) %>% 
  count(token) # %>% 
  # filter(! is.na(anio) & ! is.na(token))


p1 <- ppal_token_anio  %>% 
  mutate(token_fct  = tidytext::reorder_within(token, n, anio)) %>% 
  slice_max(order_by = n, n = 10, with_ties = F) %>% 
  ggplot() +
  geom_col(aes(x  = token_fct, y = n, fill = token)) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ anio, scales = "free") +
  guides(fill = "none") +
  xlab("") + ylab("Frecuencia") +
  labs(title = "Principales diez términos por Año") +
  ggthemes::theme_tufte(base_size = 16) 

p1 %>% 
  ggsave(filename = "tokens_ppals_region.png",
         height = 5,
         width = 10, bg = "white")


# tf-idf x anio ---------------------------------------------------------

tfidf_region <- tokens_df %>% 
  count(anio, token, sort = T) %>% 
  bind_tf_idf(term = token, document = anio, n = n)

p2 <- tfidf_region %>% 
  filter(!is.na(anio)) %>% 
  group_by(anio) %>% 
  arrange(-tf_idf) %>% 
  slice_max(order_by = tf_idf, n = 5, with_ties = F) %>% 
  ungroup() %>% 
  mutate(fct_token  = reorder_within(x = token, by = tf_idf, anio)) %>% 
  group_by(anio) %>% 
  arrange(fct_token) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(x  = fct_token, y = tf_idf, fill = token)) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ anio, scales = "free") +
  guides(fill = "none") +
  xlab("") + ylab("Frecuencia") +
  labs(title = glue("Relevancia diferencial de términos por año"),
       subtitle= "Tf-idf de token por año") +
  ggthemes::theme_tufte(base_size = 16)

p2 %>% 
  ggsave(filename = "tf-idf_anio.png",
         height = 5,
         width = 10, bg = "white")


# -------------------------------------------------------------------------

dtm_discursos <-  tokens_df %>% 
  count(token, id) %>% 
  cast_dtm(id, token, n)

tokens_df %>% 
  count(token, id) %>% 
  slice_sample(n = 500) %>% 
  pivot_wider(t)

discursos_lda <- LDA(dtm_discursos,
              k = 6,
              control = list(seed = 42))

discursos_topics <- tidy(discursos_lda,
                  matrix = "beta")

discursos_topics %>% 
  group_by(topic) %>% 
  slice_max(order_by = beta, n = 10) %>% 
  arrange(topic, desc(beta)) %>% 
  view()

beta_wide <- discursos_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic,
              values_from = beta) %>% 
  mutate(log_ratio = log2(topic2 / topic1))

discursos_documents <- tidy(discursos_lda,
                            matrix = "gamma")


# sentimientos ------------------------------------------------------------

sdal_dictionary <- sdal_dictionary %>% 
  mutate(token = replace_non_ascii(token)) %>% 
  distinct(token, .keep_all = T)

sdal_dictionary %>% 
  janitor::get_dupes(token) %>% view

tokens_sentimientos <- left_join(tokens_df, sdal_dictionary,
          by = c("token" = "token"))

tokens_sentimientos %>% 
  group_by(anio) %>% 
  summarise(agradabilidad = mean(Pleasantness,
                                 na.rm = T)) %>% view
terminos <- tokens_df %>% 
  pull(token) %>% unique()

terminos_sentimientos <- syuzhet::get_nrc_sentiment(char_v = terminos, language = "spanish")

