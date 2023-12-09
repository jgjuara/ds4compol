# Ejemplo tomado de: https://martinmontane.github.io/ScrapingA ------------------------------


library(tidyverse)
library(rvest)
library(httr)
library(xml2)

# analizar la estructura de la url
# parse_url("https://es.wikipedia.org/wiki/Anexo:Ganadores_del_Premio_Nobel")

# podemos hacer llamados HTTP GET a la url pero no suele hacerse para scraping de html
# x <- GET("https://es.wikipedia.org/wiki/Anexo:Ganadores_del_Premio_Nobel")

# directamente usamos read_html sobre la url
# conviene guardar la respuesta para no reiterarla cada vez que vemos un elemento
pagina <- read_html("https://es.wikipedia.org/wiki/Anexo:Ganadores_del_Premio_Nobel")

pagina %>% 
  html_text()

# podemos seleccionar elementos por su tag HTML, x ej.: <h1>
# html_text va a extraer el text del elemento
pagina %>% 
  html_elements(css = "h1") %>% 
  html_text()

# sin html_text nos devuelve solo el elemento tipo xml
pagina %>% 
  html_element(css = "h1")

# podemos seleccionar por id del tag html, x ej.: <cualquiertag id='firstHeading'>Texto</cualquiertag>
pagina %>% 
  html_elements(css = "#firstHeading") #id

# por la clase: <cualquiertag class='firstHeading'>Texto</cualquiertag>
pagina %>%
  html_elements(css = ".firstHeading") #clases

# por el tag y la clase o el id: <h1 class='firstHeading'>Texto</h1>
pagina %>%
  html_elements(css = "h1[class='firstHeading mw-first-heading']")

pagina %>%
  html_elements(css = "h1[id=firstHeading]") %>% 
  html_text()

# seleccionar otros elementos como <a class ='interlanguage-link-target'></a>
pagina %>% 
  html_elements(css = "a[class=interlanguage-link-target]") %>% 
  html_text()

# html_table nos permite formatear correctamente una tabla html
tabla_wiki <- pagina %>%  
  html_element("table")

tabla_wiki %>% 
  html_text()

# vs

pagina %>%  
  html_element("table") %>%
  html_table()

view(pagina %>%  
       html_element(css ="table") %>%
       html_table())

# Veamos que la tabla tiene problemas, los nombres aparecen repetidos
# Si exploramos la pagina de wikipedia vemos que es porque en cada celda <td> 
# de la tabla que queremos existen 2 <span> diferentes, ambos con el nombre.

# podemos modificar una pagina web con las funciones de xml2

# por ejemplo eliminamos los elementos <span class='display:none;'> de la copia
# de la pagina que recibimos

# selecciono los elementos a eliminar
nodosEliminar <- pagina %>% html_elements("span[style='display:none;']")

# y uso la funcion del paquete xml2
# no es necesario asignar el resultado debido a como funciona la clase de objeto xml
xml_remove(nodosEliminar)

# ahora cuando leemos la tabla está ok
tabla <- pagina %>%
  html_element("table") %>% 
  html_table(header = F)

head(tabla)


tabla <- tabla %>% 
  mutate(across(everything(), .fns =  textclean::replace_non_ascii))

colnames(tabla) <- tabla[1,]

tabla <- tabla[-1,]

tabla <- tabla %>% janitor::clean_names()

# html_attr nos permite tomar datos de atributos 
# por ejemplo los links dentro de la pagina suelen ser atributos
# acá tomamos el dato del atributo llamado "href" de los elementos <a> dentro de elementos <td>

links<- pagina %>% 
  html_elements("td") %>% 
  html_elements("a") %>% 
  html_attr("href")

# es igual a
links<- pagina %>% 
  html_elements("td a") %>% 
  html_attr("href")

# tomamos los nombres asociados a esos links
nombres<- pagina %>% 
  html_elements("td a") %>% 
  html_text() %>%  
  str_squish() #elimina espacios en blanco a inicio, fin y los dobles que estén en medio del textos

linksNombre <- tibble(nombres,links) # armamos una tabla de correspondencia link , nombre

linksNombre <- distinct(linksNombre) # quitamos duplicados

tabla <- tabla %>%
  slice(-nrow(.)) %>% # la ultima fila tiene de nuevo los nombres de columnas, la quitamos
  pivot_longer(cols = -1,names_to="Disciplina",values_to="Ganadores") # pasamos a un formato alargado

tabla <- tabla %>% 
  filter(!Ganadores %in% c("No se entregó","—")) #quitamos filas que no nos interesan

tabla <- tabla %>% 
  #divimos ganadores en varias columnas
  separate(Ganadores,sep=";",into=c("Ganador1","Ganador2","Ganador3")) 

glimpse(tabla)

tabla <- pivot_longer(tabla,cols = -c(1,2), # y ahora colapsamos las tres columnas de ganadores en una sola más larga
                      names_to = "NroGanador",values_to="Ganador")

glimpse(tabla) # ahora la tabla tiene una columna ganadores con un solo nombre por fila

# limpiamos la tabla de NA
tabla <- tabla %>% 
  filter(!is.na(Ganador)) %>% 
  mutate(Ganador=str_squish(Ganador)) %>% #limpiamos espacios en blanco dobles o finales/iniciales
  select(-NroGanador) #quitamos esta columna dummy

# le asignamos su link a cada nombre en la tabla premios nobel
tabla <- left_join(tabla,linksNombre,by=c("Ganador"="nombres"))

# hay nombres a los que no se les asoció un link
sum(is.na(tabla$links))

# veamos esos na
view(tabla %>% filter(is.na(links)))

# corregimos puntualmente 2 casos
tabla <- tabla %>%
  mutate(Ganador=gsub(", el XIV Dalái Lama","",Ganador)) %>% 
  filter(Ganador != "Olga Tokarczuk\notorgado en el año 2019") %>% 
  select(-links) # sacamos links para unir la version corregida

# volvemos a unir links
tabla <- tabla %>%  
  left_join(linksNombre, by=c("Ganador"="nombres"))

sum(is.na(tabla$links)) # contamos cuantos NA hay

# vectorización del scraping ------------------------------

# veamos como iterar sobre los links
tabla <- tabla %>% 
  mutate(links= ifelse(is.na(links), links, paste("https://es.wikipedia.org",links,sep="")))

tabla %>% slice(1) %>% pull(links)

#  probamos leer 1 link
resumenPersona <- read_html(tabla$links[3]) %>% 
  # lo que vamos a querer es la info personal de la persona que ganó el nobel
  html_element("table[class='infobox biography vcard']") %>% #este es el selector
  html_table() # y lo pasamos por html_table()

# Usen View() para ver qué tenemos
resumenPersona <- resumenPersona[, c(1:2)]

colnames(resumenPersona) <- c("Cat","Datos") # le ponemos nombre a las columnas

# vamos a quedarnos nomas con el dato de nacimiento, nacionalidad y muerte
resumenPersona <- resumenPersona %>%
  filter(str_detect(string = Cat, pattern = c("Nacimiento|Nacionalidad|muerte")))

# y lo pasamos a formato ancho
resumenPersona <- pivot_wider(resumenPersona,
                              names_from = Cat, values_from = Datos)

# veamos como queda
resumenPersona

# para iterar creamos un objeto vacío que guarde los resultados
resultados <- tibble()

# podemos usar un for, o las funciones map, lapply
# también podríamos usar llamadas asincronicas con el paquete crul
lista_links <- tabla %>% filter(!is.na(links)) %>% pull(links)

# dentro del loop hacemos lo mismo que recién pero para cada link
# esto tarda un rato, pueden reemplazar length(lista_links) por la cantidad deseada de consultas
for (i in 1:length(lista_links)) { #podemos reemplazar nrow por el numero de links que querramos tomar
 
  x <- i
  
  cat(x," de ",length(lista_links),'\r') #esto es para tener un log de la iteracion
  
  resumenPersona <- read_html(lista_links[i]) %>%
    html_element("table[class='infobox biography vcard']")
  
  # hay links que no van a funcionar
  # en esos casos queremos que a esa fila le asigne "error"
  if(class(resumenPersona) %in% "xml_missing"){
    data.frame(resultado="Error")
    next # en un loop next indica saltar a la siguiente iteracion
  }
  
  resumenPersona <- resumenPersona %>%
    html_table()
  
  resumenPersona <- resumenPersona[, c(1:2)]
  
  colnames(resumenPersona) <- c("Cat","Datos")
  
  resumenPersona <- resumenPersona %>%
    filter(Cat %in% c("Nacimiento","Nacionalidad","Causa de la muerte"))
  
  resumenPersona <- pivot_wider(resumenPersona,
                                names_from = Cat,
                                values_from = Datos)
  
  # Buscamos si el genero
  genero <- read_html(lista_links[i]) %>%
    # el genero no esta en la tabla de biography card
    # usamos las etiquetas de final de pagina para tomar este dato, fij
    # usamos el selector para seleccion el codigo css o xtpath del elemento
    html_elements("#mw-normal-catlinks > ul > li:nth-child(1) > a") %>%
    html_text()
  
  # agregamos el genero como columna
  resumenPersona <- bind_cols(resumenPersona,data.frame(genero=genero))
  # agregamos el resultado de la iteracion a la tabla general
  resultados <- bind_rows(resultados, resumenPersona)
  
  resultados <- resultados %>% mutate(links = lista_links[i])
  
  # cerramos las conexiones
  closeAllConnections() 
  
  # Esperamos un minimo para no saturar
  Sys.sleep(.1)
}
  
  
# unimos la primera tabla y los resultados del loop

tabla_final <- left_join(tabla, resultados)
tabla_final %>% view()

# crear la columna nacimiento
tabla_final <- tabla_final %>%
  # esto es: extraer un patron de 4 numeros seguidos y dps convertilo a numerico
  mutate(yearNacimiento=as.numeric(str_extract(string = Nacimiento,pattern = "\\d{4}")))




