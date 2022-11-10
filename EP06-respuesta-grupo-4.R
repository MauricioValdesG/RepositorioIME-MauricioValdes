#Ejercicio Práctico 6 (EP06)
# Grupo 04
# Integrantes:
# - ALEXANDRA CATALINA NAVARRO CALDERÓN
# - CARLOS FRANCISCO RETAMALES APARICIO
# - MAURICIO SEBASTIÁN VALDÉS GÁLVEZ 
# - MAURICIO FELIPE VICENCIO PLAZA

#Paquetes.
library(Hmisc)

#Enunciado
# Los siguientes datos se basan en un artículo publicado por Hart & Perlis (2019) (JAMA Internal Medicine, 179(9),
# 1285-1287) acerca de la proporción de mujeres autoras de artículos científicos en el área médica.

#Datos del enunciado.
datos <- data.frame(
  Especialidad = factor(c("Pediatría", "Obstetricia", "Dermatología","Psiquiatría", "M. Interna", "Oncología","Neurología", "Anestesiología", "Radiología")),
  Mujeres = c(54, 71, 35, 30, 45, 44, 56, 21, 17),
  Hombres = c(52, 66, 41, 42, 65, 62, 88, 40, 35)
)


################################################################################

# PREGUNTA 1

################################################################################

# Estudios previos habían determinado que la proporción de autoras en la 
# especialidad de pediatría era de 35%. ¿Respaldan estos datos tal estimación?

################################################################################

# La pregunta va asociado a una inferencia de una proporción con una muestra, la
# que podemos ahondar con el método de Wilson para una proporción.

# Las hipótesis van a ser las siguientes:
# H0: La proporción de autoras en el área de pediatría es de 35% (p = 0,35).
# HA: La proporción de autoras en el área de pediatría no es de 35% (p != 0,35).

# Definimos los valores conocidos.
valor.nulo <- 0.35
mujeres <- datos[["Mujeres"]][1]
hombres <- datos[["Hombres"]][1]
n <- mujeres + hombres

# Veremos las condiciones a verificar.
# Suponemos que los responsables del estudio fueron cuidadosos y las observaciones
# son independientes entre sí.

# La condición de éxito fracaso, es que se espera observar al menos 10 observaciones
# conforme al éxito (mujeres, en este caso) y al menos 10,
# conforme a los fracasos (hombres).
expec.success <- n * valor.nulo
expec.failure <- n * (1 - valor.nulo)

cat("\n Mujeres esperadas: ", expec.success, "Hombres esperados: ", expec.failure,
    "\n")

# Se puede apreciar que las cantidades esperadas de éxitos y fracasos
# superan el límite inferior de 10, entonces se verifican las condiciones.

# Dado que las condiciones se han verificado, continuamos con la prueba. 
# Se fija un nivel de significación dado los argumentos de confiar en el estudio.
# (alfa) de 0,05.
p.1 <- prop.test(x = mujeres, n = n, p = valor.nulo,
                 alternative = "two.sided", conf.level = 0.95,
                 correct = FALSE)

print(p.1)

# El valor p es de 0,509434, es mayor que el nivel de significación de
# 0,05, por lo que rechazamos la hipótesis alternativa en favor de la hipótesis
# nula. Así, podemos concluir, con 95% de confianza, que la proporción
# de autoras que publican artículos en el área de pediatría es de 35%.

################################################################################

# PREGUNTA 2

################################################################################

# Según estos datos, ¿es igual la proporción de autoras en las áreas de 
# anestesiología y pediatría?

################################################################################

# Esta pregunta pertenece a una inferencia acerca de la diferencia entre
# dos proporciones. Se puede volver a usar el método de Wilson, pero con la variación
# de diferencia entre dos proporciones.

#  Las hipótesis van a ser las siguientes:
# H0: La proporción de autoras es la misma en las áreas de anestesiología y
#     pediatría (p1 - p2 = 0).
# HA: La proporción de autoras es distinta en las áreas de anestesiología y
#     pediatría (p1 - p2 != 0).

# Datos del problema

mujeres.anestesiologia <- datos[["Mujeres"]][8]
hombres.anestesiologia <- datos[["Hombres"]][8]
mujeres.pediatria <- datos[["Mujeres"]][1]
hombres.pediatria <- datos[["Hombres"]][1]
# n
n.anestesiologia <- mujeres.anestesiologia + hombres.anestesiologia
n.pediatria <- mujeres.pediatria + hombres.pediatria

# Se analizan las condiciones
# Planteando lo mismo que la pregunta anterior, suponemos que los autores del 
# estudio inicial fueron rigurosos y que se cumplen las condiciones de independencia.


# La condición de éxito fracaso, es que se espera observar al menos 10 observaciones
# conforme al éxito (mujeres, en este caso) y al menos 10, conforme a los fracasos (hombres).
# En ambos casos se cumple la condicion, para anestesiologia 21 y 40 y pediatría 54 y 52.


# Dado que las condiciones se han verificado, continuamos con la prueba. 
# Se fija un nivel de significación dado los argumentos de confiar en el estudio.
# (alfa) de 0,05.

p.2 <- prop.test(x = c(mujeres.anestesiologia, mujeres.pediatria),
                 n = c(n.anestesiologia, n.pediatria),
                 alternative = "two.sided", conf.level = 0.95,
                 correct = FALSE)

print(p.2)
# El valor de p es 0.0388, es menor que el nivel de significación de 0.05, por
# lo cual, se falla al rechazar la hipotesis alternativa. Entonces se concluye con un
# 95% de confianza, que la proporción de autoras que publican artículos en las 
# áreas de anestesiología y pediatría no es la misma.


################################################################################

# PREGUNTA 3

################################################################################

# Suponiendo que la diferencia en la proporción de autoras en la especialidad de
# medicina interna y la de dermatología es de 0,15. ¿A cuántos autores  (hombres 
# y mujeres) deberíamos monitorear para obtener un intervalo de confianza del
# 97,5% y poder estadístico de 80%, si se intenta mantener aproximadamente la
# misma proporción de gente estudiada en cada caso?

################################################################################

# Otra vez se aborda la diferencia de proporciones entre dos
# grupos, pero en este caso, se solicita el tamaño de la muestra.

# Las hipótesis van a ser las siguientes (destacando que no se da información 
# para la dirección de HA):
# H0: Hay una diferencia de 15% en la proporción de autoras en las áreas de
#     medicina interna y dermatología (|p1 - p2| = 0,15).
# HA: No hay una diferencia de 15% en la proporción de autoras en las áreas de
#     medicina interna y dermatología (|p1 - p2| != 0,15).

# Proporciones observadas:
n.observado.medicina_interna <- datos[["Mujeres"]][5] + datos[["Hombres"]][5] 
p.observada.mujeres.medicina_interna <- datos[["Mujeres"]][5] / n.observado.medicina_interna


n.observado.dermatologia <- datos[["Mujeres"]][3] + datos[["Hombres"]][3]
p.observada.mujeres.dermatologia <- datos[["Mujeres"]][3] / n.observado.dermatologia

cat("\nCasos observados de medicina interna:", n.observado.medicina_interna, "\n")

cat("Proporción observada de mujeres de medicina interna:",  p.observada.mujeres.medicina_interna, "\n")

cat("\nCasos observados de dermatología:", n.observado.dermatologia, "\n")

cat("\nProporción observada de mujeres de dermatología:",p.observada.mujeres.dermatologia, "\n")

# Se definen las proporciones "esperadas" que se aproximen a las observadas, en este caso,
# con la diferencia establecida en la hipótesis.


p.esperada.mujeres.medicina_interna <- .36
p.esperada.mujeres.dermatologia <- .51
diferencia <- p.esperada.mujeres.medicina_interna - p.esperada.mujeres.dermatologia

# Se calcula el tamaño de las muestras, teniendo tamaños distintos.
alfa <- 0.025
poder <- 0.80
fraccion <- n.observado.medicina_interna / (n.observado.medicina_interna + n.observado.dermatologia)

result <- bsamsize(p1 = p.esperada.mujeres.medicina_interna,
                   p2 = p.esperada.mujeres.dermatologia,
                   fraction = fraccion,
                   alpha = alfa, power = poder)

cat("\nResultado función bsamsize():\n\n")
print(result)

# Se calcula los tamaños de las muestras, siendo números positivos
n.medicina_interna <- ceiling(result[1])
n.dermatologia <- ceiling(result[2])

cat("\nSe deberían considerar", n.medicina_interna, "autores en medicina interna y", n.dermatologia, "en dermatología")
