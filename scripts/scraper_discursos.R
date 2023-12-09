library(httr2)
library(rvest)
library(tidyverse)


# scraping inicial --------------------------------------------------------


url_base <- "https://www.casarosada.gob.ar"

req_base <- request(paste0(url_base, "/informacion/discursos"))

response_base <- req_base %>% 
  req_perform()

response_base %>% 
  resp_status()

response_html <- response_base %>% 
  resp_body_html() 


# conseguir urls de paginacion --------------------------------------------


attrs_pagination <- response_html %>% 
  html_element(".pagination") %>% 
  html_elements("a") %>% 
  html_attrs() 
  
# chequear que acá faltan urls
# cómo se podría resolver aunque sea seteando algun valor a mano?

attrs_pagination <- attrs_pagination %>% 
  bind_rows() 

max_index  <- attrs_pagination %>% 
  mutate(indice = str_remove_all(href, "[^[:digit:]]")) %>% 
  pull(indice) %>% as.numeric() %>% max()

vector <- attrs_pagination %>% 
  mutate(indice = str_remove_all(href, "[^[:digit:]]")) %>% 
  pull(indice) %>% as.numeric()

(max(vector[1:9])-min(vector[1:9]))/(length(vector[1:9])-1)

href <- attrs_pagination$href %>% str_remove_all(., "[:digit:]") %>% unique()
index <- seq(0,max_index, 40)

df <- tibble(urls = paste0(url_base, href, index))

df <- df %>% 
  slice_head(n = 17)

df <- df %>% 
  mutate(webs_pags = map(urls, function(x) {

    print(x)
    
    x %>% 
      request() %>% 
      req_perform() %>% 
      resp_body_html() 
  }
         ))


# scrapear paneles --------------------------------------------------------

# acá puede armarse el loop para capturar todos los paneles de cada pagina


df <- df %>% 
  mutate(paneles = map(webs_pags, ~ html_elements(.x,
                                                  ".panel")))
  

df <- df %>% 
  mutate(paneles_attrs = map(paneles, ~ html_attrs(.x) %>% 
                               bind_rows()))

df <- df %>% 
  unnest(cols = paneles_attrs, names_sep = "_")

df <- df %>% 
  mutate(urls_discursos = paste0(url_base, paneles_attrs_href))

# cada vez que se capturan los enlaces de paneles debería scrapearse la web asociada a ese panel

df <- df %>% 
  mutate(web_panel = map(urls_discursos, function(x) {
    x %>% 
      request() %>% 
      req_perform() %>% 
      resp_body_html()
  }))

df <- df %>% 
  mutate(fecha_discurso = map(web_panel,
                              ~ html_element(.x,
                                             "time") %>% 
                                html_text()))

df <- df %>% 
  mutate(texto = map(web_panel,
                     ~ html_elements(.x, "p") %>% 
                       html_text2() %>% 
                       paste(collapse = " ")))

# write_rds(df, file = "scripts/discursos_rosada.rds")
