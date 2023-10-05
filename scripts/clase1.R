# intalar si hace falta
install.packages("tidyverse")
library(tidyverse)

# Intro -------------------------------------------------------------------

# Todo este archivo es un script
# El texto que va despues de un # se llama 'comentario'
print('en general todo lo que no es un comentario es codigo ejecutable')

# Cada linea puede leerse como un bloque de instrucciones
options(scipen = 999)
10*(1/10+9/10)^10
(10*(1/10+9/10))^10


# Objetos -----------------------------------------------------------------

# Llamamos objeto en R a un nombre que hace referencia a datos 
# u otras cosas almacenadas en memoria de trabajo.
# Para crear un objeto usamos un operador de asignación: <-
# El operador lleva a izquierda el nombre de objeto y a derecha lo que va a referir.


mi_nombre <- "juan" # asigno la cadena de texto 'juan' al objeto mi_nombre
mi_nombre #  al invocar el objeto devuelve su valor en consola

# aca tengo este error: object 'Numero' not found
numero <- 47
numero
numero_2 <- numero*10
numero_2

# las funciones tambien son objetos, tienen un nombre y lo que esta almacenado son las instrucciones

print #si las llamamos sin usar parentesis va a mostrarnos en pantalla lo que refiere al codigo

print(numero) #print es una funcion que devuelve a pantalla(consola) objetos o datos
print(mi_nombre)

# cuando creamos funciones es igual que crear otro objeto cualquiera
funcion_cuadratica <- function(x) {x*x}

funcion_cuadratica(x = 9) #la probamos

funcion_cuadratica #al llamarla nos muestra lo que contiene este objeto

dos_cuadrado <- funcion_cuadratica(x = 2) #la probamos
dos_cuadrado
dos_cuadrado <- funcion_cuadratica(x = 4) #pisar objeto
dos_cuadrado

# Vectores ----------------------------------------------------------------

 
# los vectores son conjuntos de elementos de un mismo tipo
# se crean con una funcion: c()

numeros <- c(1,3,5)

letras <- c("a", "b", "c")

logicos <- c(TRUE, FALSE, TRUE)

numeros_numeros <- c(numeros, numeros)

numeros_letras <- c(numeros, letras)

str(numeros_letras)

numeros

numeros[3]*numeros[2]
numeros[3]*numeros
sum(numeros)




