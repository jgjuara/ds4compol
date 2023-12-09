
# Scraper Precios Claros --------------------------------------------------

# Ejemplo tomado de https://martinmontane.github.io/ScrapingB
library("crul")
library(jsonlite)
library(tidyverse)
#library(rvest)
library(httr)

#  link <- "https://www.preciosclaros.gob.ar/#!/buscar-productos"

# primero exploremos la web con las herramientas de desarrollador del navegador

# la web de precios claros consulta una api
# los links a la api son estos:
api_sucursales <- "https://d3e6htiiul5ek9.cloudfront.net/prod/sucursales?lat=-31.42008329999999&lng=-64.18877609999998&limit=30&"
api_productos <- "https://d3e6htiiul5ek9.cloudfront.net/prod/productos?&id_categoria=01&array_sucursales=2003-1-7670,10-3-785,24-2-59,10-3-768,9-2-444,10-3-720,10-3-648,24-2-314,10-3-770,24-2-74,10-3-765,24-2-300,9-1-440,10-3-769,9-2-435,24-2-79,2011-1-143,10-3-610,2009-1-78,10-3-772,10-3-793,2011-1-126,24-2-131,10-3-607,9-2-441,24-2-58,24-2-66,10-3-790,10-3-776,24-2-101&offset=0&limit=50&sort=-cant_sucursales_disponible"

# podemos parsear la url
parse_url(api_sucursales)
parse_url(api_productos)

# hacemos las consultas

sucursales <- fromJSON(api_sucursales)
productos <- fromJSON(api_productos)

view(sucursales$sucursales)
view(productos$productos)

# tmb podemos ver que los mensajes de error son utiles

fromJSON('https://d3e6htiiul5ek9.cloudfront.net/prod/sucursales?jasdkjas')

# Los parámetros permitidos en esta consulta son: 
#  lat, lng, limit, offset, sucursal_provincia, sucursal_tipo, 
# comercio_bandera_nombre, comercio_razon_social, distancia_min,
# distancia_max, entorno.

fromJSON('https://d3e6htiiul5ek9.cloudfront.net/prod/productos?error')

# Los parámetros permitidos en esta consulta son:
# string, id_categoria, lat, lng, cant_sucursales,
# limit, offset, id_sucursal, array_sucursales, sort, entorno.

# fromJSON('https://d3e6htiiul5ek9.cloudfront.net/prod/productos?id_sucursal=24-2-157&limit=100') 


# La api de sucursales nos trae las sucursales mas cercanas
# ordenadas por distancia. Podemos scrapear todas las sucursales
# offset es el parametro que nos permite elegir a partir de fila tomar los resultados

# por ej. sucursales tiene un limite de 30 resultados por consulta
# entonces esto devuelve los primeros 30
"https://d3e6htiiul5ek9.cloudfront.net/prod/sucursales?lat=-31.42008329999999&lng=-64.18877609999998&limit=30&"
# y esto los siguientes 30:
view(fromJSON("https://d3e6htiiul5ek9.cloudfront.net/prod/sucursales?lat=-31.42008329999999&lng=-64.18877609999998&limit=30&offset=29")$sucursales)
# veamos que el primero y el ultimo coinciden

# podemos ir scrapeando de a 30
# en total eran

sucursales$total

#entonces

# offset <- seq(0,sucursales$total,by=30)
offset <- seq(0,60,by=30)


# son 119 llamadas.podemos armar una lista de urls con cada offset en particular

urls_sucursales <- paste0('https://d3e6htiiul5ek9.cloudfront.net/prod/sucursales?lat=-31.42008329999999&lng=-64.18877609999998&limit=30&offset=', offset)

# probemos uno

fromJSON(urls_sucursales[2])

# en vez de usar map, lapply o un loop conviene usar el paquete crul

async_sucursales <- Async$new(
  urls = urls_sucursales) #prepara llamada async

async_sucursales_rpta <- async_sucursales$get() # http GET

#res[[1]]$parse() #Parse the raw response content to text
df_sucursales <- map(async_sucursales_rpta, ~ .x$parse("UTF-8"))

df_sucursales <- map(df_sucursales, ~ fromJSON(.x))

df_sucursales <- map_dfr(df_sucursales, ~ .x$sucursales )

df_sucursales$id

# Iterar sobre df_sucursales$id ------------------------------

productos_offset0 <- fromJSON(glue::glue("https://d3e6htiiul5ek9.cloudfront.net/prod/productos?id_sucursal={df_sucursales$id[1]}&limit=100"))


offset_productos <- seq(0,productos_offset0$total,by=productos_offset0$maxLimitPermitido)

urls_productos_suc <- glue::glue("https://d3e6htiiul5ek9.cloudfront.net/prod/productos?id_sucursal={df_sucursales$id[1]}&limit=100&offset={offset_productos}")

async_prods <- Async$new(
  urls = urls_productos_suc) #prepara llamada async

async_prods_rpta <- async_prods$get() # http GET

#res[[1]]$parse() #Parse the raw response content to text
df_prods_suc <- map(async_prods_rpta, ~ .x$parse("UTF-8"))
df_prods_suc <- map(df_prods_suc, ~ fromJSON(.x))
df_prods_suc <- map_dfr(df_prods_suc, ~ .x$productos)

df_prods_suc <- df_prods_suc %>% 
  mutate(sucursal = df_sucursales$id[1],
         fecha = Sys.Date()) 



# comparacion  ------------------------------

offset <- seq(0,559,by=100)
lista <- map(offset,function(offsetNumber){
  cat("offset: ",offsetNumber,"\r")
  consulta <- paste("https://d3e6htiiul5ek9.cloudfront.net/prod/productos?&id_categoria=01&array_sucursales=15-1-5248,12-1-2,10-3-396,15-1-5232,15-1-52,15-1-24,23-1-6220,15-1-371,10-1-219,65-1-344,12-1-103,15-1-475,15-1-473,9-3-5263,10-3-420,15-1-492,15-1-302,19-1-00082,23-1-6224,10-3-687,15-1-627,10-3-539,15-1-490,15-1-540,15-1-588,10-3-561,10-3-622,15-1-219,10-3-608,10-3-526&offset=",offsetNumber,"&limit=100&sort=-cant_sucursales_disponible",sep="")
})


microbenchmark::microbenchmark(
  x <-  map(lista, function(x) {
    # cat("offset: ",offsetNumber,"\r")
    # consulta <- paste("https://d3e6htiiul5ek9.cloudfront.net/prod/productos?&id_categoria=01&array_sucursales=15-1-5248,12-1-2,10-3-396,15-1-5232,15-1-52,15-1-24,23-1-6220,15-1-371,10-1-219,65-1-344,12-1-103,15-1-475,15-1-473,9-3-5263,10-3-420,15-1-492,15-1-302,19-1-00082,23-1-6224,10-3-687,15-1-627,10-3-539,15-1-490,15-1-540,15-1-588,10-3-561,10-3-622,15-1-219,10-3-608,10-3-526&offset=",offsetNumber,"&limit=100&sort=-cant_sucursales_disponible",sep="")
    resultado <- fromJSON(x)
    resultado$productos
  }), times = 1
)



microbenchmark::microbenchmark(
  {cc <- Async$new(
    urls = unlist(lista)) #prepara llamada async
  res <- cc$get() # http GET
  #res[[1]]$parse() #Parse the raw response content to text
  x <- lapply(res, function(z) jsonlite::fromJSON(z$parse())$productos)},
  times = 1
  )

# Es imporantate tener en cuenta que dado que se ejecuntan múltimples consultas en simultano
# Resulta mucho más fácil llegar al límite de consultas por tiempo o saturar la páginas
# Hay que ajustar bien la cantidad de consultas o cómo iterar en funcion de cada API


