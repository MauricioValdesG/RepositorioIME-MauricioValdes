#Ejercicio Práctico 5 (EP05)
# Grupo 04
# Integrantes:
# - ALEXANDRA CATALINA NAVARRO CALDERÓN
# - CARLOS FRANCISCO RETAMALES APARICIO
# - MAURICIO SEBASTIÁN VALDÉS GÁLVEZ 
# - MAURICIO FELIPE VICENCIO PLAZA

#Paquetes.
library(ggpubr)
library(dplyr)
library(pwr)

#Enunciado

# Se sabe que una máquina que envasa detergentes industriales llena bidones con 
# un volumen de producto que sigue una distribución normal con desviación estándar
# de 1 litro. Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo
# de la planta requiere determinar si la máquina está llenando los bidones con
# una media de 10 litros.

# SE CONSIDERAN AMBOS ESCENARIOS DE PRUEBAS UNILATERALES (UNA COLA) Y BILATERALES
# (DOS COLAS).

#Definimos los valores conocidos.
dev.estandar <- 1 
n.muestra <- 100
c.inferior <- 9.5
c.superior <- 10.5

# Se calcula el error estándar.
SE <- dev.estandar /sqrt(n.muestra)

# Se genera a partir de un valor nulo, una distribución normal con 5000 valores.
valor.nulo <- 10
pts <-5000

#-------------------------------------------------------------------------------
#                                 BILATERALES
#-------------------------------------------------------------------------------

################################################################################

# PREGUNTA 1

################################################################################

# Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente 
# una media menor a 9,5 litros o mayor a 10,5 litros, ¿cuál es la probabilidad 
# de que cometa un error de tipo I?

################################################################################

# Dado los datos del enunciado, se nos presenta una prueba t para una muestra,
# con hipótesis bilateral.
# Siendo las hipótesis las siguientes:
# H0: El volumen medio de los bidones es de 10 litros 
# (mu = 10[L])
# H1: El volumen medio de los bidones es distinto de 10 litros 
# (mu != 10[L])

# La probabilidad de que se cometa un error tipo I, va asociado al nivel 
# de significación. Dicho nivel de significación se encuentra en el área de la
# región de rechazo de la distribución, estos deberían seguir las medias muestrales
# que están en la hipótesis nula.
# Se asume que trata de una prueba t para una muestra, pero por el hecho de 
# conocerse la desviación estándar, también se podría realizar la distribución 
# normal (prueba Z).


x <- seq(valor.nulo - 5.5 * SE, valor.nulo +5.5 * SE, length.out = pts)
y <- dnorm(x, mean = valor.nulo, sd = SE)
distribucion.p1 <- data.frame(x, y)

# Se grafica la distribución.

grafico.distribucion <- ggplot(data = distribucion.p1, aes(x))

# Se agrega la distribución normal.
grafico.distribucion  <- grafico.distribucion  + stat_function(fun = dnorm,
                                 args = list(mean = valor.nulo,
                                             sd = SE),
                                 colour = "Black", size = 1)


# Se agrega marcas y etiquetas al eje x.
grafico.distribucion <- grafico.distribucion + scale_x_continuous(name = "Volumen [L]",
                                      breaks = seq(c.inferior, c.superior,
                                                   0.5))
# Se Agregar la media bajo la hipótesis nula.
grafico.distribucion <- grafico.distribucion + geom_vline(xintercept = valor.nulo,
                              colour = "Blue", linetype = "longdash")

#Se agrega el título y se muestra el gráfico resultante.
grafico.distribucion <- grafico.distribucion + ggtitle("Distribución de las medias muestrales bajo Hipotesis Nula (H0)")

grafico.distribucion

# Se marcan las regiones de rechazo dados por el enunciado.

grafico.bilateral.1 <- grafico.distribucion + geom_area(data = subset(distribucion.p1, x < c.inferior),
                                    aes(y = y), colour = "Red",
                                    fill = "Red", alpha = 0.5)

grafico.bilateral.1 <- grafico.bilateral.1 + geom_area(data = subset(distribucion.p1,
                                                         x > c.superior),
                                           aes(y = y), colour = "Red",
                                           fill = "Red", alpha = 0.5)
# Se agrega el título del gráfico.
grafico.bilateral.1 <- grafico.bilateral.1 + ggtitle("Pregunta 1 - Hipótesis Bilateral")

#Se muestra el gráfico.
grafico.bilateral.1

# Calcular la probabilidad que suman las regiones de rechazo.
alfa.izquierdo <- pnorm(c.inferior, mean = valor.nulo, sd = SE,
                        lower.tail = TRUE)

alfa.derecho <- pnorm(c.superior, mean = valor.nulo, sd = SE,
                      lower.tail = FALSE)

bilateral.alfa <- alfa.izquierdo + alfa.derecho

cat("La probabilidad de que un error tipo I ocurra, es alfa =", bilateral.alfa,
    "\n\n")

################################################################################

# PREGUNTA 2

################################################################################

# Si el verdadero volumen medio de los bidones fuera de 10,3 litros, ¿cuál sería
# la probabilidad de que el ingeniero, que obviamente no conoce este dato, 
# cometa un error de tipo II?

################################################################################


# Se realiza un gráfico de la verdadera distribución y lo superponemos con la
# hipótesis nula.


bilateral.media <- 10.3

x1 <- seq(bilateral.media - 5.5 * SE,
          bilateral.media + 5.5 * SE, length.out = pts)

y1 <- dnorm(x1, mean = bilateral.media, sd = SE)
distribucion.p2 <- data.frame(x = x1, y = y1)

grafico.bilateral.2 <- grafico.bilateral.1 + stat_function(fun = dnorm, n = pts,
                                               args = list(mean = bilateral.media,
                                                           sd = SE),
                                               colour = "Blue", size = 1)

grafico.bilateral.2 <- grafico.bilateral.2 + geom_vline(xintercept = bilateral.media,
                                            colour = "Blue",
                                            linetype = "longdash")

# Un  error tipo II significaría no rechazar la hipótesis nula cuando esta es
# falsa.
# Siendo para este caso, no rechazar la idea de que la media de la población es 10 [L], 
# siendo que es 10,3 [L]. Este tipo de error ocurre si la media muestral esta
# fuera de las regiones críticas definidas por el ingeniero.


# Se Sombrea el área de la curva "verdadera" que está fuera de las regiones de
# rechazo de la curva correspondiente a la H0 (hipótesis nula).
grafico.bilateral.2 <- grafico.bilateral.2 + geom_area(
  data = subset(distribucion.p2, x >= c.inferior & x <= c.superior),
  aes(y = y), colour = "Blue", fill = "Blue", alpha = 0.5)

#Se agrega el título.
grafico.bilateral.2 <- grafico.bilateral.2 + ggtitle("Pregunta 2 - Hipótesis Bilateral")

#Se muestra el gráfico superpuesto con el anterior.
grafico.bilateral.2

# Se calcula la probablidad de esta región, siendo beta.
beta.superior <- pnorm(c.superior, mean = bilateral.media,
                       sd = SE, lower.tail = TRUE)

beta.inferior <- pnorm(c.inferior, mean = bilateral.media,
                       sd = SE, lower.tail = TRUE)

bilateral.beta <- beta.superior - beta.inferior

cat("La probabilidad de que un error tipo II ocurra, es beta =", bilateral.beta,
    "\n\n")


################################################################################

# PREGUNTA 3

################################################################################

# Como no se conoce el verdadero volumen medio, genere un gráfico del poder
# estadístico con las condiciones anteriores, pero suponiendo que el verdadero
# volumen medio podría variar de 9,3 a 10,7 litros.

################################################################################


# Al preguntarse por el poder estadístico, es decir, la probabilidad de detectar
# que Hipótesis Nula (H0) es falsa cuando efectivamente lo es, además se solicita
# una curva de poder para diferentes valores de la verdadera media.

# Creamos una función que calcule el poder a partir del razonamiento hecho en la
# pregunta anterior (considerando además el caso de hipótesis unilaterales).

poder.p3 <- function(media, SE.p3, limite.inferior = NULL, limite.superior = NULL) {
  poder.inferior <- 0
  poder.superior <- 1
  
  if(!is.null(limite.inferior)) {
    poder.inferior <- pnorm(limite.inferior, mean = media, sd = SE.p3,
                       lower.tail = TRUE)
  }
  
  if(!is.null(limite.superior)) {
    poder.superior <- pnorm(limite.superior, mean = media, sd = SE.p3,
                       lower.tail = FALSE)
  }
  
  poder.p3 <- poder.inferior + poder.superior
  return(poder.p3)
}

# Se generan puntos en el rango dado para graficar.
x3 <- seq(9.3, 10.7, 0.01)

y3 <- sapply(x3, poder.p3, SE.p3 = SE, limite.inferior = 9.3,
             limite.superior = 10.7)

#se le asignan los datos y características para graficar la distribución.

distribucion.p3 <- data.frame(x = x3, y = y3)
grafico.bilateral.3 <- ggplot(distribucion.p3, aes(x, y))
grafico.bilateral.3 <- grafico.bilateral.3 + geom_line(colour = "Orange")
grafico.bilateral.3 <- grafico.bilateral.3 + ylab("Poder estadístico")
grafico.bilateral.3 <- grafico.bilateral.3 + xlab("Volumen media verdadero [L]")
grafico.bilateral.3 <- grafico.bilateral.3 + theme_pubr()

grafico.bilateral.3 <- grafico.bilateral.3 + theme(
  axis.text.x = element_text(angle = 30, size = 10))

grafico.bilateral.3 <- grafico.bilateral.3 + ggtitle("Pregunta 3 - Hipótesis Bilateral")
grafico.bilateral.3


################################################################################

# PREGUNTA 4

################################################################################

# Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse 
# para conseguir un poder estadístico de 0,8 y un nivel de significación de 0,05?
  

################################################################################

# Se pregunta por el tamaño de la muestra para conseguir los valores para los
# factores de la prueba: alfa = 0,05 y  poder = 0,8.

# Se calcular tamaño del efecto usando el d de Cohen.
bilateral.efecto <- (bilateral.media - valor.nulo) / dev.estandar

# Al tratarse de una prueba z, se puede usar la función pwr.norm.test()
# del paquete pwr() y así calcular el tamaño de la muestra.
bilateral.poder.z <- pwr.norm.test(d = bilateral.efecto, sig.level = 0.05,
                                   power = .8, alternative = "two.sided")

cat("Resultado de pwr.norm.test():\n")
print(bilateral.poder.z)

bilateral.tam.z <- ceiling(bilateral.poder.z[["n"]])

cat("El tamaño de la muestra de una prueba z debe ser n =",
    bilateral.tam.z, "\n\n")

# En el caso de que fuese una prueba t, se usa la función pwr.t.test().
bilateral.poder.t1 <- pwr.t.test(d = bilateral.efecto, sig.level = 0.05,
                                 power = 0.8, type = "one.sample",
                                 alternative = "two.sided")

cat("Resultado de  pwr.t.test():\n")
print(bilateral.poder.t1)

bilateral.tam.t1 <- ceiling(bilateral.poder.t1[["n"]])

cat("El tamaño de la muestra de una prueba t debe ser n =",
    bilateral.tam.t1, "\n\n")

# Una opción es usar power.t.test(), es decir, considerar el tamaño del efecto
# en la escala de la variable.

# Se calcula tamaño del efecto.
bilateral.diferencia <- bilateral.media- valor.nulo

bilateral.poder.t2 <- power.t.test(delta = bilateral.diferencia,
                                   sd = dev.estandar,
                                   sig.level = 0.05, power = .8,
                                   type = "one.sample",
                                   alternative = "two.sided")

cat("El tamaño de la muestra para una prueba t con power.t.test():\n")
print(bilateral.poder.t2)

bilateral.tam.t2 <- ceiling(bilateral.poder.t2[["n"]])

cat("El tamaño de la muestra de una prueba t debe ser n =",
    bilateral.tam.t2, "\n\n")

################################################################################

# PREGUNTA 5

################################################################################

# ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de
# cometer un error de tipo I a un 1% solamente?

################################################################################

#Si nos damos cuenta, la pregunta se repite más arriba, solo que el alfa corresponde
# a 0,01.

# Prueba Z.
bilateral.poder.z <- pwr.norm.test(d = bilateral.efecto, sig.level = 0.01,
                                   power = .8, alternative = "two.sided")

cat("Resultado de pwr.norm.test():\n")
print(bilateral.poder.z)

bilateral.tam.z <- ceiling(bilateral.poder.z[["n"]])

cat("El tamaño de la muestra para una prueba z debe ser n =",
    bilateral.tam.z, "\n\n")

# Prueba t con pwr.t.test().
bilateral.poder.t1 <- pwr.t.test(d = bilateral.efecto, sig.level = 0.01,
                                 power = 0.8, type = "one.sample",
                                 alternative = "two.sided")

cat("Resultado de  pwr.t.test():\n")
print(bilateral.poder.t1)

bilateral.tam.t1 <- ceiling(bilateral.poder.t1[["n"]])

cat("El tamaño de la muestra de una prueba t debe ser n =",
    bilateral.tam.t1, "\n\n")

# Prueba t con power.t.test().
bilateral.poder.t2 <- power.t.test(delta = bilateral.diferencia,
                                   sd = dev.estandar,
                                   sig.level = 0.01, power = .8,
                                   type = "one.sample",
                                   alternative = "two.sided")

cat("Tamaño de la muestra de una prueba t con power.t.test():\n")
print(bilateral.poder.t2)

bilateral.tam.t2 <- ceiling(bilateral.poder.t2[["n"]])

cat("El tamaño de la muestra de una prueba t debe ser n =",
    bilateral.tam.t2, "\n\n")


#-------------------------------------------------------------------------------
#                                 UNILATERALES
#-------------------------------------------------------------------------------


################################################################################

# PREGUNTA 1

################################################################################

# Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente 
# una media menor a 9,5 litros o mayor a 10,5 litros, ¿cuál es la probabilidad 
# de que cometa un error de tipo I?

################################################################################

# Del enunciado realizamos una prueba t para una muestra, con una hipótesis unilateral:
# H0: El volumen medio de los bidones es de 10 litros (mu = 10 [L]).
# H1: El volumen medio de los bidones es mayor a 10 litros (mu > 10 [L]).

# La probabilidad de que se cometa un error tipo I, va asociado al nivel 
# de significación.Dicho nivel de significación se encuentra en el área de la
# región de rechazo de la distribución, estos deberían seguir las medias muestrales
# que están en la hipótesis nula.
# Usamos la distribución normal (prueba Z).

# Se toma como base el gráfico de la distribución normal y marcamos la región de
# rechazo definida por el enunciado.

unilateral.grafico.1 <- grafico.distribucion + geom_area(data = subset(distribucion.p1, x > c.superior),
                                     aes(y = y), colour = "Red",
                                     fill = "Red", alpha = 0.5)

unilateral.grafico.1 <- unilateral.grafico.1 + ggtitle("Pregunta 1 - Hipótesis Unilateral")
unilateral.grafico.1

# Calcular la probabilidad de la región de rechazo.
unilateral.alfa <- pnorm(c.superior, mean = valor.nulo,
                           sd = SE, lower.tail = FALSE)

cat("La probabilidad de cometer un error tipo I es alfa =", unilateral.alfa,
    "\n\n")


################################################################################

# PREGUNTA 2

################################################################################

# Si el verdadero volumen medio de los bidones fuera de 10,3 litros, ¿cuál sería
# la probabilidad de que el ingeniero, que obviamente no conoce este dato, 
# cometa un error de tipo II?

################################################################################


# Se realiza un gráfico de la verdadera distribución y lo superponemos con la
# hipótesis nula.

unilateral.media <- 10.3

x2 <- seq(unilateral.media- 5.5 * SE,
          unilateral.media + 5.5 * SE, length.out = pts)

y2 <- dnorm(x1, mean = unilateral.media, sd = SE)
distribucion.2 <- data.frame(x = x2, y = y2)

unilateral.grafico.2 <- unilateral.grafico.1 + stat_function(
  fun = dnorm, n = pts, args = list(mean = unilateral.media,sd = SE),
  colour = "Blue", size = 1)

unilateral.grafico.2 <- unilateral.grafico.2 + geom_vline(xintercept = unilateral.media,
                                              colour = "Blue",
                                              linetype = "longdash")

# Un error tipo II significaría no rechazar la hipótesis nula cuando esta es
# falsa.
# Siendo para este caso, no rechazar la idea de que la media de la población es 10 [L], 
# siendo que es 10,3 [L]. Este tipo de error ocurre si la media muestral esta
# fuera de las regiones críticas definidas por el ingeniero.


# Se sombrea el área de la curva "verdadera" que está fuera de las regiones de
# rechazo de la curva correspondiente a la H0 (hipótesis nula).
unilateral.grafico.2 <- unilateral.grafico.2 + geom_area(
  data = subset(distribucion.2, x <= c.superior), aes(y = y), colour = "Blue",
  fill = "Blue", alpha = 0.5)

unilateral.grafico.2 <- unilateral.grafico.2 + ggtitle("Pregunta 2 - Hipótesis Unilateral")
unilateral.grafico.2

# Se calcula la probabilidad de esta región, siendo beta.
unilateral.beta <- pnorm(c.superior, mean = unilateral.media,
                         sd = SE, lower.tail = TRUE)

cat("La probabilidad de cometer un error tipo II es beta =", unilateral.beta,
    "\n\n")



################################################################################

# PREGUNTA 3

################################################################################

# Como no se conoce el verdadero volumen medio, genere un gráfico del poder
# estadístico con las condiciones anteriores, pero suponiendo que el verdadero
# volumen medio podría variar de 9,3 a 10,7 litros.

################################################################################


# Al preguntarse por el poder estadístico, es decir, la probabilidad de detectar
# que Hipótesis Nula (H0) es falsa cuando efectivamente lo es, además se solicita
# una curva de poder para diferentes valores de la verdadera media.

# Se generan puntos en el rango dado para graficar, usando la función creada en
# bilateral.

x4 <- seq(9.3, 10.7, 0.01)

y4 <- sapply(x3, poder.p3, SE.p3 = SE, limite.inferior = NULL,
             limite.superior = 10.7)

#se le asignan los datos y características para graficar la distribución.

distribucion.4 <- data.frame(x = x4, y = y4)
unilateral.grafico.3 <- ggplot(distribucion.4, aes(x, y))
unilateral.grafico.3 <- unilateral.grafico.3 + geom_line(colour = "Orange")
unilateral.grafico.3 <- unilateral.grafico.3 + ylab("Poder estadístico")
unilateral.grafico.3 <- unilateral.grafico.3 + xlab("Volumen media verdadero [L]")
unilateral.grafico.3 <- unilateral.grafico.3 + theme_pubr()

unilateral.grafico.3 <- unilateral.grafico.3 + theme(
  axis.text.x = element_text(angle = 30, size = 10))

unilateral.grafico.3 <- unilateral.grafico.3 + ggtitle("Pregunta 3 - Hipótesis Unilateral")
unilateral.grafico.3

################################################################################

# PREGUNTA 4

################################################################################

# Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse 
# para conseguir un poder estadístico de 0,8 y un nivel de significación de 0,05?

################################################################################

# Se calcular tamaño del efecto usando el d de Cohen.
unilateral.efecto <- (unilateral.media - valor.nulo) / dev.estandar

# Al tratarse de una prueba z, se puede usar la función pwr.norm.test()
# del paquete pwr() y así calcular el tamaño de la muestra.
unilateral.poder.z <- pwr.norm.test(d = unilateral.efecto, sig.level = 0.05,
                                    power = .8, alternative = "greater")

cat("Resultado de pwr.norm.test():\n")
print(unilateral.poder.z)

unilateral.tam.z <- ceiling(unilateral.poder.z[["n"]])

cat("El tamaño de la muestra de una prueba z debe ser n =",
    unilateral.tam.z, "\n\n")


# En el caso de que fuese una prueba t, se usa la función pwr.t.test().
unilateral.poder.t1 <- pwr.t.test(d = unilateral.efecto, sig.level = 0.05,
                                 power = 0.8, type = "one.sample",
                                 alternative = "greater")

cat("Resultado de  pwr.t.test():\n")
print(unilateral.poder.t1)

unilateral.tam.t1 <- ceiling(unilateral.poder.t1[["n"]])

cat("El tamaño de la muestra de una prueba t debe ser n =",
    unilateral.tam.t1, "\n\n")

# Una opción es usar power.t.test(), es decir, considerar el tamaño del efecto
# en la escala de la variable.

# Se calcula tamaño del efecto.
unilateral.diferencia <- bilateral.media- valor.nulo

unilateral.poder.t2 <- power.t.test(delta = unilateral.diferencia,
                                   sd = dev.estandar,
                                   sig.level = 0.05, power = .8,
                                   type = "one.sample",
                                   alternative = "one.sided")

cat("El tamaño de la muestra para una prueba t con power.t.test():\n")
print(unilateral.poder.t2)

unilateral.tam.t2 <- ceiling(unilateral.poder.t2[["n"]])

cat("El tamaño de la muestra de una prueba t debe ser n =",
    unilateral.tam.t2, "\n\n")


################################################################################

# PREGUNTA 5

################################################################################

# ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de
# cometer un error de tipo I a un 1% solamente?

################################################################################

#Si nos damos cuenta, la pregunta se repite más arriba, solo que el alfa corresponde
# a 0,01

# Prueba Z.
unilateral.poder.z <- pwr.norm.test(d = unilateral.efecto, sig.level = 0.01,
                                   power = .8, alternative = "greater")

cat("Resultado de pwr.norm.test():\n")
print(unilateral.poder.z)

unilateral.tam.z <- ceiling(unilateral.poder.z[["n"]])

cat("El tamaño de la muestra para una prueba z debe ser n =",
    unilateral.tam.z, "\n\n")

# Prueba t con pwr.t.test().
unilateral.poder.t1 <- pwr.t.test(d = unilateral.efecto, sig.level = 0.01,
                                 power = 0.8, type = "one.sample",
                                 alternative = "greater")

cat("Resultado de  pwr.t.test():\n")
print(unilateral.poder.t1)

unilateral.tam.t1 <- ceiling(unilateral.poder.t1[["n"]])

cat("El tamaño de la muestra de una prueba t debe ser n =",
    unilateral.tam.t1, "\n\n")

# Prueba t con power.t.test().
unilateral.poder.t2 <- power.t.test(delta = unilateral.diferencia,
                                   sd = dev.estandar,
                                   sig.level = 0.01, power = .8,
                                   type = "one.sample",
                                   alternative = "one.sided")

cat("Tamaño de la muestra de una prueba t con power.t.test():\n")
print(unilateral.poder.t2)

unilateral.tam.t2 <- ceiling(unilateral.poder.t2[["n"]])

cat("El tamaño de la muestra de una prueba t debe ser n =",
    unilateral.tam.t2, "\n\n")


