# las funciones son instrucciones que toman un input de entrada y realizan algún proceso
# se definen con la palabra clave function
funcion_q_no_hace_nada <- function(argumentos) {
  
}

# en los parentesis a continuanción de function se definen los parametros de la funcion
# y entre llaves el procedimiento que debe realizar la funcion
suma_uno <- function(x) {
  x+1
}

# esta funcion tiene 2 parametros de entrada
# y devuelve una cadena de texto con la respuesta calculada
sumacalculadora <- function(N, pct = 100){
  # evalua la condicion de que pct sea menor o igual a 100
  if (pct <= 100) {
    val <- (pct*N)/100 # calcula el pct porciento de N
    valchar <- paste(as.character(pct),"%", sep="")
    paste("El", valchar, "de", N, "es", val)
  } else {
    cat("Sólo calcula hasta 100%")
  }
}

sumacalculadora(N = 50, pct = 90)
sumacalculadora(pct = 90, N = 50)


# for es una palabra clave para armar loops
# el loop for itera tomando cada valor de un vector o lista
# y ejecuta el codigo dentro de las llaves en cada iteración
resultados_largo <- list()
indice <- 1

# var_temp toma cada valor de discurso de a 1 x vez
for (var_temp in discursos$discurso) {
  
  # este codigo se ejecuta para cada var_temp
  # resultados_largo e indice están inicializados fuera del loop
  # resultados_largo almacena un valor en el item que indica indice en cada vuelta
  resultados_largo[indice] <- str_length(var_temp)
  # indice suma 1 en cada vuelta
  indice <- indice + 1
}

# while a diferencia de loop se va a repetir siempre que la condición entre parentisis sea cierta
indice <- 1
# después de cada vuelta evalúa si indice es menor a 25
# si es verdadero repite, sino termina el proceso
while (indice < 25) {
  print(indice)
  Sys.sleep(.5)
  indice <- indice +1
}

# la palabra clave break permite romper los loops for o while
while (indice < 25) {
  
  print(indice)
  if (indice == 13) {
    print("mala suerte")
    break
  }
  Sys.sleep(.5)
  indice <- indice +1
}
