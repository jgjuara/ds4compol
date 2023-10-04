# Cargar librerias
# install.packages('devtools') #si no tiene instalado devtools
# devtools::install_github("politicaargentina/discursAr")
library(discursAr)
library(tidyverse)


# ahora veamos los discursos
show_available_speech(viewer = T)

# map lo que hace es iterar sobre cada valor de .x y aplicar la funcion deseada
# la salida es una lista
discursos <- map(.x = 2012:2019, .f = function(x) get_speech(year = x, raw = T)) 

class(discursos)

# pasamos la lista a dataframe con bind_rows()
discursos <- discursos %>% 
  bind_rows()

glimpse(discursos)

# veamos un discurso
cfk_2012 <- discursos$discurso[1]

# Verifica si la cadena de texto contiene el patron buscado, devuelve T o F
str_detect(cfk_2012, "Perón")

# lo mismo para cada discurso del periodo
str_detect(discursos$discurso, "Perón")

# Devuelve un vector de índices de la cadena de texto donde se encuentra el patrón
str_which(discursos$discurso, "Perón")

# Cuenta la cantidad de veces que aparece "Perón" en cada elemento
str_count(discursos$discurso, "Perón")

# Encuentra la posición de la primera ocurrencia de "Perón" en el texto
str_locate(cfk_2012, "Perón")

# Devuelve la longitud de cada cadena de texto 
str_length(discursos$discurso)

# Elimina espacios en blanco a la izquierda, derecha o ambos lados de la cadena
str_trim("   Juan    Gabriel Juara  ")

# Elimina espacios en blanco a ambos lados y reemplaza espacios múltiples entre palabras por uno solo
str_squish("  Juan    Gabriel Juara ")

# Reemplaza todas las ocurrencias de "Argentina" por "MI PAIS" 
str_replace_all(discursos$discurso, pattern = "Argentina", replacement = "MI PAIS")

# Busca todas las ocurrencias de cualquier expresión regular
# y devuelve todas las coincidencias para el total de la cadena y cada grupo de captura
str_match_all(discursos$discurso, pattern = "[:digit:]")

# Extrae todas las ocurrencias de cualquier patrón de texto en una cadena
str_extract_all(discursos$discurso, pattern = "[:digit:]")

# ejemplo de uso de map para crear una columna tipo lista con cada palabra unica que se usa en el discurso

discursos <- discursos %>% 
  mutate(palabras = map(discurso,
                        .f  = function(x) {
                          x %>% 
                            tolower(.) %>% 
                            str_split(., 
                                      pattern = " ") %>% 
                            unique(.)
                        })
  )

