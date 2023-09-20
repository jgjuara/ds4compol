# Cargar librerias
library(tidyverse)

# vamos a levantar un excel de varias paginas

url <- "https://www.argentina.gob.ar/sites/default/files/trabajoregistrado_2305_estadisticas.xlsx"

# los excel no se pueden leer directo de la url a diferencia de los csv, json, txt y otros
# descargar y guardar en archivo local


# vamos a leer la hoja "T.2.2" con readxl::read_excel()

# inspeccionar la tabla
glimpse(trabajo)

# 1) emprolijar nombres de columnas
# pista: janitor::clean_names()

# 2) ajustar las fechas
# para la conversion hacemos: pasamos de texto a numero y de numero a fecha definiendo el origen 
# para excel suele ser origin = "1899-12-31"
# ver as.Date()
# revisar cómo funcionan formatos de fecha
# ver la documentacion de as.Date() y de strftime()

# vemos que dice Caused by warning in `as.Date()`: ! NAs introduced by coercion
# esto nos avisa que hay datos que no se pudieron convertir a fecha y se tranformaron en NAs
# recordatorio NAs son datos faltantes, en gral debido a un error de ingreso o procesamiento de datos
# NAs son diferentes de datos nulos o ceros

# revisemos qué paso
trabajo %>% 
  mutate(fecha = as.Date(as.numeric(periodo), origin = "1899-12-31")) %>% 
  filter(is.na(fecha)) %>% 
  select(periodo, fecha)

# agregar datos de puestos de trabajo de asalariados privados y autonomos por año
# para ello calculamos el total por mes y el promedio anual de puestos mensuales
