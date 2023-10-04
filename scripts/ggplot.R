library(tidyverse)
# install.packages("readxl")
library(readxl)

# vamos a levantar un excel de varias paginas

url <- "https://www.argentina.gob.ar/sites/default/files/trabajoregistrado_2305_estadisticas.xlsx"

# los excel no se pueden leer directo de la url a diferencia de los csv, json, txt y otros
# descargar y guardar en archivo local
download.file(url = url, destfile = "entradas/trabajoregistrado.xlsx", mode = "wb")

ruta_excel <- "entradas/trabajoregistrado.xlsx"


trabajo <- readxl::read_excel(path = ruta_excel, sheet = "T.2.2", skip = 1)


trabajo <- trabajo %>% 
  janitor::clean_names() %>% 
  mutate(fecha = as.Date(as.numeric(periodo), origin = "1899-12-31")) %>% 
  mutate(periodo = str_replace(string = periodo, pattern = "-|_", replacement = "."),
         periodo = str_replace(string = periodo, pattern = "sep", replacement = "sept"),
         periodo = str_replace(string = periodo, pattern = "\\*", replacement = ""), 
         fecha = if_else(is.na(fecha),
                         parse_date(periodo, format = "%b%y",
                                    locale = locale(date_names = "es")),
                         fecha))

trabajo <- trabajo %>% 
  filter(year(fecha) >= 2018) %>% 
  select(-c(periodo, total)) %>% 
  pivot_longer(cols = -c(fecha))

trabajo_medias <- trabajo %>% 
  mutate(anio = year(fecha)) %>% 
  group_by(anio, name) %>% 
  mutate(media = mean(value)) %>% ungroup()

# grafico de serie de tiempo puestos de trabajo por fecha y tipo

trabajo %>% 
  ggplot() +
  # geom_algo 


# mismo graf agregar una serie de tiempo que use los datos de trabajo_media
trabajo %>% 
  ggplot() +
  # geom_algo() +
  # geom_algo(data= algo, aes(),
        # linewidth = 1,
        #linetype = "dashed")

