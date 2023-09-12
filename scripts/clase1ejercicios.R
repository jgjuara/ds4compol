
# Vectores ----------------------------------------------------------------

# Los vectores son las estructuras de datos mas simples.
# Pueden almacenar muchos elementos pero siempre del mismo tipo.
# Los creamos con la funcion c()

vector1 <- c(TRUE, FALSE, TRUE) #lógico
vector1

vector2 <- c(1,2,3,4,5) #numerico
vector2

vector3 <- c("calle", "barrio", "ciudad") #character
vector3

# Si introducimos 2 tipos de datos diferentes en un vector, 
# R van a transformar los datos en la clase más abarcativa

# TRUE es transformado en 1 y FALSE en 0
vector4 <- c(TRUE, 3, FALSE) 
vector4

# los valores numéricos son llevados a character de forma literal
vector5 <- c(2,3,4, "casa", TRUE) 
vector5


# Ejercicio 1 -------------------------------------------------------------
# Crear un vector que contenga el nombre de los vicepresidentes desde el gobierno del Nestor Kirchner hasta el de Mauricio Macri
# Cada nombre debe ser un elemento independiente
vectorNombresVices <- c("Scioli", "Cobos","Boudou", "Michetti")

# Ejercicio 2 -------------------------------------------------------------
# Crear un vector que contenga la fecha de inicio de mandato de cada vicepresidente
inicio_mandatos <- c("25 de mayo de 2003", "10 de diciembre de 2007",
                      "10 de diciembre de 2011", "10 de diciembre de 2015")

inicio_mandatos

fechas_mandatos <- c(20030525, 20071210, 20111210, 20151210)

inicio_mandatos[1] < inicio_mandatos[2]

fechas <- as.Date(c("2003/05/25", "2007/12/10"))


# Ejercicio 3 -------------------------------------------------------------
#Crear un vector que contenga valores de verdadero o falso según si el vicepresidente perteneció al mismo partido político que el presidente
mismo_partido <- c(T, F, T, T)



# Indices --------------------------------------------------
# Podemos seleccionar elementos puntuales de los vectores usando indices
# Los vectores en R empiezan desde 1
# Podemos usar el signo menos (-) para excluir elementos por su indice

vector1[2] #elemento 2

vector4[3] #elemento 3

vector5[4] #elemento 4

vector5

vector5[-c(4, 5)] #todos los elementos menos el 4 y el 5
vector5[-c(1, 2)]

indice <- c(1,4)

vector5[indice]

vector5 <- ""

class(vector5)

vector1[5]

# Otros tipos de datos ----------------------------------------------------


## Factores ----------------------------------------------------------------

# Los factores son datos de tipo ordinal o datos cualitativos con un orden asignado
# Tienen dos componentes: etiquetas y niveles
# Por ejemplo pensemos en las palabras "mediano", "chico" y "grande"
# Les podemos asignar un orden de menor a mayor: "chico" "mediano "grande"
# Esto es importante porque va a facilitar su uso sobre todo en los gráficos

vector6 <- factor(x = c("chico", "mediano", "grande"))
vector6

# los niveles son asignados automaticamente por orden de alfabetico

# podemos definir el orden de los niveles de forma explicita
vector7 <- factor(x = c("chico", "mediano", "grande"),
                  levels = c("chico", "mediano", "grande"))
vector7


# Ejercicio 4 --------------------------------------------------------------

# Crear un vector tipo factor con el nombre de los vicepresidentes y ordenar los niveles según orden de sus mandatos

VectorFactorVices <- factor(x = c("Scioli", "Michetti", "Boudou", "Cobos"),
                            levels = c("Scioli", "Cobos", "Boudou", "Michetti"))
VectorFactorVices

sort(vectorNombresVices)
sort(VectorFactorVices)
