# Sun Oct 02 17:06:03 2022 ------------------------------

# objetivo ----------------------------------------------------------------

# Obtener toda la metadata posible de cada app según su bundleid


# webs objetivo -----------------------------------------------------------

#  Apple no tiene api de consulta publica de datos
# un ejemplo del app store de apple a scrapear: 
# https://apps.apple.com/es/app/picsart-editor-de-fotos-videos/id587366035

# Google no tiene api de consulta publica de datos
# un ejemplo del app store de google a scrapear: 
# https://play.google.com/store/apps/details?id=com.badoo.mobile


# caracterizacion ---------------------------------------------------------

# son webs estaticas, pueden tener captcha/recaptcha
# vamos a realizar pocas llamadas: 600 en total entre ambas apps stores
# son paginas simples

# campos criticos (solo lo podemos saberlo post analisis de la web)
# android: nombre de app, tiene ads, 

# codigo ------------------------------------------------------------------

# ejemplo android ---------------------------------------------------------
library(tidyverse) #conjunto de librerias de manipulacion de datos (sobre todo para EDA)
library(httr) #funciones get, post, etc..
library(rvest) #funciones de lectura html

# url
id <- "com.picsart.studio"
url_app_store <- "https://play.google.com/store/apps/details?id="
url_app_android <- paste0(url_app_store, id)

#llamada get
# quiero que devuelve info en español, hay que corregir encoding
response <- GET(url_app_android,
                add_headers("useragent" = "Mozilla/5.0 (Windows NT 5.1; rv:52.0) Gecko/20100101 Firefox/52.0" , #es recomendado definir un user-agent especifico
                                             "accept-encoding" = "gzip, deflate, br", #defino para evitar problemas de encoding
                                             "accept-language" = "es-AR,es-US;q=0.9,es;q=0.8,en-US;q=0.7,en;q=0.6,es-419;q=0.5")) #idiomas aceptados = español

x <- read_html(url_app_android)

# status ok?
response$status_code == 200

# cabezales
headers(response)$`content-type`

# la respuesta es html?
grepl(pattern = "text/html",
      x = headers(response)$`content-type`)

# el contenido viene codificado
response$content

# para poder leerlo necesitamos pasarlo a strings (parse)
read_html(response)

# pasamos el html interpretado a un nuevo objeto
cuerpo_html <- read_html(response)

# Ya tenemos todo el contenido de la web cargado en memoria
# Dependiendo del proyecto podríamos querer guardar una copia entera de la web ahora
xml2::write_xml(x = cuerpo_html,
           file = "ejemplo_android_Es.html")

