# Cargar librerias
library(tidyverse)
# install.packages("readxl")
library(readxl)

# vamos a levantar un excel de varias paginas

url <- "https://www.argentina.gob.ar/sites/default/files/trabajoregistrado_2305_estadisticas.xlsx"

# los excel no se pueden leer directo de la url a diferencia de los csv, json, txt y otros
# descargar y guardar en archivo local
download.file(url = url, destfile = "entradas/trabajoregistrado_202305.xlsx", mode = "wb")

ruta_excel <- "entradas/trabajoregistrado_202305.xlsx"

# vamos a leer la hoja "T.2.2" con readxl::read_excel()
readxl::read_excel(path = ruta_excel)

readxl::excel_sheets(ruta_excel)

readxl::read_excel(path = ruta_excel, sheet = "T.2.2")

trabajo <- readxl::read_excel(path = ruta_excel, sheet = "T.2.2", skip = 1)

# inspeccionar la tabla
glimpse(trabajo)

# 1) emprolijar nombres de columnas
# pista: janitor::clean_names()

trabajo <- trabajo %>% 
  janitor::clean_names()


trabajo %>% 
  glimpse()


# 2) ajustar las fechas
class(trabajo$periodo)
unique(trabajo$periodo)

# para la conversion hacemos: pasamos de texto a numero y de numero a fecha definiendo el origen 
# para excel suele ser origin = "1899-12-31"
# ver as.Date()
# revisar cómo funcionan formatos de fecha
# ver la documentacion de as.Date() y de strftime()


trabajo %>% 
  mutate(fecha = as.Date(as.numeric(periodo), origin = "1899-12-31")) %>% 
  glimpse()

# vemos que dice Caused by warning in `as.Date()`: ! NAs introduced by coercion
# esto nos avisa que hay datos que no se pudieron convertir a fecha y se tranformaron en NAs
# recordatorio NAs son datos faltantes, en gral debido a un error de ingreso o procesamiento de datos
# NAs son diferentes de datos nulos o ceros

# revisemos qué paso
trabajo %>% 
  mutate(fecha = as.Date(as.numeric(periodo), origin = "1899-12-31")) %>% 
  filter(is.na(fecha)) %>% 
  select(periodo, fecha) %>% 
  mutate(periodo = str_replace(string = periodo, pattern = "-|_", replacement = "."),
         periodo = str_replace(string = periodo, pattern = "sep", replacement = "sept"),
         periodo = str_replace(string = periodo, pattern = "\\*", replacement = ""), 
         fecha = parse_date(periodo, format = "%b%y",
                            locale = locale(date_names = "es"))) 

str_replace("juan gabriel juara", pattern = "gabriel|juara", replacement = "naranja")
str_replace_all("juan gabriel juara", pattern = "gabriel|juara", replacement = "naranja")

trabajo %>% 
  mutate(fecha = as.Date(as.numeric(periodo), origin = "1899-12-31")) %>% 
  mutate(periodo = str_replace(string = periodo, pattern = "-|_", replacement = "."),
         periodo = str_replace(string = periodo, pattern = "sep", replacement = "sept"),
         periodo = str_replace(string = periodo, pattern = "\\*", replacement = ""), 
         fecha = if_else(is.na(fecha), parse_date(periodo, format = "%b%y",
                            locale = locale(date_names = "es")), fecha)) %>% 
  filter(is.na(fecha))

trabajo %>% 
  mutate(fecha = as.Date(as.numeric(periodo), origin = "1899-12-31")) %>% 
  mutate(periodo = str_replace(string = periodo, pattern = "-|_", replacement = "."),
         periodo = str_replace(string = periodo, pattern = "sep", replacement = "sept"),
         periodo = str_replace(string = periodo, pattern = "\\*", replacement = ""), 
         fecha = if_else(is.na(fecha),
                         parse_date(periodo, format = "%b%y",
                                                  locale = locale(date_names = "es")),
                         fecha)) %>% 
  filter(is.na(fecha))

trabajo <- trabajo %>% 
  mutate(fecha = as.Date(as.numeric(periodo), origin = "1899-12-31")) %>% 
  mutate(periodo = str_replace(string = periodo, pattern = "-|_", replacement = "."),
         periodo = str_replace(string = periodo, pattern = "sep", replacement = "sept"),
         periodo = str_replace(string = periodo, pattern = "\\*", replacement = ""), 
         fecha = if_else(is.na(fecha),
                         parse_date(periodo, format = "%b%y",
                                    locale = locale(date_names = "es")),
                         fecha))

"sept"
"may.17" # %b%y

"may.-17" # %b-%y

# agregar datos de puestos de trabajo de asalariados privados y autonomos por año
# para ello calculamos el total por mes y el promedio anual de puestos mensuales

