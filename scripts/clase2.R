# Crear una fecha actual
fecha_actual <- Sys.time()

print(fecha_actual)
class(fecha_actual)

class(as.Date("2023-06-30"))

strftime(fecha_actual, format = "%Y %b")
strftime(fecha_actual, format = "%B / %Y")

?strftime

# install.packages("lubridate")
library(lubridate)

# cálculos
# Sumar 3 días a una fecha
nueva_fecha <- fecha_actual + days(3)
print(nueva_fecha)

# Restar 1 semana a una fecha
fecha_anterior <- fecha_actual - weeks(1)
print(fecha_anterior)

# Calcular la diferencia en días entre dos fechas
diferencia_dias <- as.numeric(difftime(fecha_actual,
                                       fecha_anterior,
                                       units = "days"))
print(diferencia_dias)

# Ejemplos de uso de lubridate
fecha <- ymd("2023-09-01")  # Convertir una cadena en una fecha

week(fecha)  # Obtener la semana del año

month(fecha)  # Obtener el mes

month(fecha, label = T)




# Matrices -----------------------------------------------------------------

# tablas de n dimensiones con un unico tipo de dato
# Crear un vector
vector_numerico <- 1:100

# Convertir el vector en una matriz de 2 filas y 3 columnas
matriz <- matrix(vector_numerico, ncol = 10, byrow = F)
print(matriz)

# los elementos se seleccionan por posicion de fila y columna (en ese orden)
matriz[9, 2]
matriz[9,]
matriz[,2]

# Tmb pueden ser de otros tipos
matrix(data = letters, ncol = 4)

# Dataframes --------------------------------------------------------------

# Tmb son tablas pero pueden contener diferentes datos en cada columna
# Cada fila es un caso u observación
# cada columna una variable o atributo

# Crear un data frame con R base
df <- data.frame(
  nombre = c("Juan", "María", "Pedro", "Luis"),
  edad = c(25, 30, 22, 28),
  puntuacion = c(80, 95, 75, 88),
  inscripto = c(F,T,T,F)
)

df

View(df)


# Mostrar el data frame
print(df)

# Acceder a una fila específica
fila_1 <- df[1, ]
print(fila_1)

# Acceder a una columna específica por nombre
df$nombre

# Acceder a una columna específica por índice
df[, 2]

# Filtrar filas basadas en una condición
df[df$edad > 25, ]
df$edad > 25 # vector logico
df[c(T,F,T,F), ] # indice por vector lógico

# Calcular estadísticas resumen
summary(df)

# Agregar una columna
df$residencia <- c("Chubut", "La Pampa", NA, NA)
df
