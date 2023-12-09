# selenium

library(RSelenium)

# se puede correr en docker
# docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1
# tambien se puede ejecutar directamente en el entorno local

# RSelenium y wdman usan clases R6 para trabajar con ellas
# son objetos que tienen forma de una lista que guarda atributos y funciones
# en realidad es una clase especial para la programación orientada a objetos en R
# ver https://adv-r.hadley.nz/r6.html 

# rsdriver llama internamente a la funcion wdman::selenium() y RSelenium::remoteDriver()
# esa funcion inicia el servidor selenium

# iniciamos el driver:
driver <- rsDriver( # se elige el browser (por default es chrome)
  browser = "firefox",
  chromever = NULL,
  # se pueden elegir versiones especificas del navegador a usar
  # chromever = "106.0.5249.61"
  # se elige el puerto, por default es 4567
  port= 1002L
)

# el driver tiene dentro server y cliente
# ahora se nos debería haber abierto una sesion de navegador

# el objeto creado tiene funciones propias
# preguntar estado
driver$client$getStatus() 

# cerrar la sesión
driver$client$close()

#abre una sesión en navegador
driver$client$open()


driver$client$navigate("https://www.argentina.gob.ar") #navegamos a urls 
driver$client$getCurrentUrl() #podemos pedir que nos devuelva la url de donde estamos
driver$client$navigate("https://cran.r-project.org/") #vamos a otra paginas
driver$client$goBack() #atras
driver$client$goForward() #adelante
driver$client$goBack() 

# vamos a tratar de levantar las respuestas del bot de argentina.gob.ar
# no podríamos hacer esto con los metodos vistos hasta ahora

# en particular los bots suelen estar metidos en "frames"
# son como paginas web adentro de otras

# inspeccionando la web buscamos como identificarlos
frame <- driver$client$findElement("name", 'Botmaker') 

# podemos hacer que resalte la seleccion
frame$highlightElement()

# hay que pedirle al cliente que se enfoque en ese 'frame'
driver$client$switchToFrame(Id = frame)

#miremos el codigo fuente
driver$client$getPageSource()

# buscamos el elemento del boton
boton <- driver$client$findElement(using = "id", value = "wc-button")

# veamos si está ok
boton$highlightElement()

# hacemos click
boton$clickElement()


# ahora tenemos que posarnos en la caja de texto
texto <- driver$client$findElement(using = 'id', 'wc-textarea')

# miremos
texto$highlightElement()

# vamos a esperar 1 seg a que se ejecute el script del bot
Sys.sleep(1)

# y podemos interactuar enviando texto
texto$sendKeysToElement(list("Hola!"))

# borramos el texto
texto$clearElement()

# escribimos y enviamos
texto$sendKeysToElement(list("Boletin Oficial",
                             key = 'enter'))

# podemos tomar elementos especificos del chat
menu <- driver$client$findElement('class', 'bm-webchat-rule-pills-pill')

# ver
menu$highlightElement()

# click
menu$clickElement()

# seguir interactuando por teclado
texto$sendKeysToElement(list("Salud",
                             key = 'enter'))

texto$sendKeysToElement(list("Calendario vacunas",
                             key = 'enter'))

texto$sendKeysToElement(list("Más de 65 años",
                             key = 'enter'))

# y luego vamos a tomar las respuestas del bot
# en particular en este bot las respuestas tienen clase 'bm-webchat-text-entry-left'
respuestas <- driver$client$findElements('class', 'bm-webchat-text-entry-left')

# respuestas tienen cada elemento de esa clase, luego tomamos el texto que contienen
lapply(respuestas, FUN = function(x) {x$getElementText()})
