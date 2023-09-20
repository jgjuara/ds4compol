# install.packages('eph') si no tiene instalado devtools
library(tidyverse)

# funciones de lectura
?read.csv


atencion_ciudadano <- read_csv("http://bitsandbricks.github.io/data/gcba_suaci_barrios.csv")
#atencion_ciudadano2 <- read.csv("http://bitsandbricks.github.io/data/gcba_suaci_barrios.csv")

# crear directorios
# funciones de escritura de archivos
#write.csv(atencion_ciudadano, "salidas/atencion_ciudadano.csv")

dir.create("salidas")
write_csv(atencion_ciudadano, "salidas/atencion_ciudadano.csv")
write_rds(atencion_ciudadano, "salidas/atencion_ciudadano.rds")


# eliminar un objeto del ambiente
rm(respaldo)

# tibbles o dataframes
cantidad_filas <- nrow(atencion_ciudadano)

dim(atencion_ciudadano)

view(atencion_ciudadano)

head(atencion_ciudadano, n = 10)
str(atencion_ciudadano)

columnas <- colnames(atencion_ciudadano)
columnas

atencion_ciudadano %>% 
  janitor::clean_names()

columnas <- tolower(columnas)

colnames(atencion_ciudadano) <- columnas

atencion_ciudadano

reclamos_barrio_periodo <- atencion_ciudadano %>% 
  select(periodo, barrio, total)

combinaciones_unicas <- atencion_ciudadano %>% 
  distinct(rubro, tipo_prestacion, barrio)

atencion_ciudadano %>% 
  pull(rubro) %>% 
  unique()


atencion_ciudadano %>% 
  pull(tipo_prestacion) %>% 
  unique()

atencion_ciudadano %>% 
  pull(barrio) %>% 
  unique()

atencion_ciudadano_recorte <- atencion_ciudadano %>% 
  filter(tipo_prestacion %in% c("QUEJA", "DENUNCIA", "RECLAMO") & 
           barrio %in% c("BOCA", "PALERMO"))

atencion_ciudadano_recorte <- atencion_ciudadano_recorte %>% 
  mutate(periodo = lubridate::ym(periodo),
         anio = lubridate::year(periodo), 
         mes = lubridate::month(periodo)) 
  


atencion_ciudadano_recorte <- atencion_ciudadano_recorte %>% 
  select(periodo, anio, mes, everything())

atencion_ciudadano_recorte %>% 
  filter(if_any(everything(), is.na))

atencion_ciudadano_recorte %>% 
  filter(!if_any(everything(), is.na))

atencion_ciudadano_recorte <- atencion_ciudadano_recorte %>% 
  filter(if_any(everything(), function(x) !is.na(x)))

atencion_ciudadano_recorte <- atencion_ciudadano_recorte %>% 
  arrange(desc(anio), desc(mes), tipo_prestacion, barrio)

tabla_resumen <- atencion_ciudadano_recorte %>% 
  group_by(barrio, anio) %>% 
  summarise(cantidad_rubros = n_distinct(rubro), 
            total = sum(total)) %>% 
  ungroup()

atencion_ciudadano_recorte <- atencion_ciudadano_recorte %>% 
  arrange(barrio, anio, mes) %>% 
  group_by(barrio, anio, mes) %>%
  mutate(total_mes = sum(total), 
         proporcion = total/total_mes) %>% 
  ungroup()
  