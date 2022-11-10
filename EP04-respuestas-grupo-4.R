#Ejercicio Práctico 4 (EP04)
# Grupo 04
# Integrantes:
# - ALEXANDRA CATALINA NAVARRO CALDERÓN
# - CARLOS FRANCISCO RETAMALES APARICIO
# - MAURICIO SEBASTIÁN VALDÉS GÁLVEZ 
# - MAURICIO FELIPE VICENCIO PLAZA

# Fijamos espacio de trabajo.
setwd("E:\\OneDrive - usach.cl\\2022-2\\8°Semestre\\IME\\IME-EP\\EP04")

#Paquetes.
library(ggpubr)
library(dplyr)

# Datos.
datos = read.csv2("EP04 datos.csv")

#El Comité Olímpico de una gran potencia del atletismo está estudiando el 
#programa de entrenamiento de varones para la competencia de 100 metros planos,
# por lo que ha recopilado datos de diversos atletas:
#
#  - Id:        identificador único para cada atleta.
#
#  - Raza:      raza del atleta (categórica: Blanca, Negra, Oriental).
#
#  - Previo:    mejor tiempo registrado por el atleta antes de ingresar al 
#               programa de entrenamiento (numérica, en segundos).
#
#  - Posterior: mejor tiempo registrado por el atleta durante los primeros
#               6 meses del programa de entrenamiento (numérica, en segundos).


################################################################################

# Pregunta 1

################################################################################

# El Comité Olímpico cree que el mejor tiempo medio de los atletas orientales 
# antes de ingresar al programa de entrenamiento es superior a 18,83 segundos. 
# ¿Soportan los datos esta afirmación?


#Filtramos los datos, por la raza, en este caso, extrayendo la raza "Oriental" 
# y así obtener a los atletas orientales.

p_oriental <- datos %>% filter(Raza == "Oriental")

#Al indicarnos "antes" de ingresar al programa, se selecciona la variable "Previo".
#Usando "pull", esta función nos permite seleccionar una columna (en este caso "Previo")
#y transformarla a vector.

p_oriental.previo <- pull(p_oriental,Previo)
n.1 <- length(p_oriental.previo)
cat("Tamaño de la muestra:",n.1,"\n")

# Como la muestra es pequeña (menos de 30 observaciones), sería adecuado usar
# la prueba t de Student para una muestra. Pero antes debemos verificar las
# condiciones.

# Como se trata de atletas diferentes y la muestra representa menos del 10% de
# la población, podemos asumir que las observaciones son independientes entre
# sí.

# Debemos verificar si presentan una distribución cercana a la normal, de esta manera
# utilizando la prueba de normalidad de Shapiro-wilk se verificará.

valor_ShapiroWilk.oriental <- shapiro.test(p_oriental.previo)
print(valor_ShapiroWilk.oriental)

#Se grafica para observar esta distribución.
data.p_oriental <- data.frame(p_oriental.previo)
grafico.p1 <- ggqqplot(data.p_oriental,
                       x = "p_oriental.previo",
                       color = "Red",
                       xlab = "Teórico",
                       ylab = "Muestra",
                       title = "Gráfico Q-Q Muestra v/s Distribución Normal")
grafico.p1

# Como el valor p obtenido es alto (0.4826>0.05), podemos suponer razonablemente que esta 
#condición se verifica, por lo que podemos aplicar la prueba  seleccionada.

# Como no hay indicios de que tengamos que ser cautelosos con los resultados,
# indicamos un número de significación 0.05, dado que tiene menos 
# de un 5 % de probabilidad de que ocurra producto de la casualidad.

n_significacion <- 0.05

# A continuación formamos nuestra hipótesis nula y nuestra hipótesis alternativa,
# siendo H0 y H1 respectivamente.
# H0  mu <= 18,83
# HA  mu > 18,83 
# Valor nulo = 18.83
# Nivel de significación = 0.05 (Nivel de confianza 95%). 

#Asignamos nuestro valor nulo.
valor_nulo.p1 <- 18.83

#Verificado el valor, se realiza la prueba t student.
prueba_T.Oriental_previa <- t.test(p_oriental.previo,
                                   alternative = "greater",
                                   mu = valor_nulo.p1,
                                   conf.level = 1 - n_significacion)
prueba_T.Oriental_previa

# Como el valor de p obtenido es menor que el nivel de significación (0.009118 < 0,05),
# por lo que rechazamos la hipótesis nula en favor de la hipótesis alternativa.

# En consecuencia, podemos concluir con 95% de confianza que, en promedio, la
# mejor marca de los atletas orientales en los 100 metros planos antes del
# entrenamiento es superior a 18,83 segundos.Entonces los datos soportan esta afirmación.

################################################################################

# Pregunta 2

################################################################################

# ¿Sugieren los datos que la mejor marca de los atletas negros se reduce en 
# 5,82 segundos tras el entrenamiento?

# Filtramos la población de atletas por la raza, siendo la variable de la 
# raza "Negra". Después separamos en vectores las columnas "Previo" y "Posterior".

p_negra <- datos %>% filter (Raza == "Negra")
p_negra.previo <- pull(p_negra,Previo)
p_negra.post <- pull(p_negra,Posterior)
n.2 <- length(p_negra.previo)
cat("Tamaño de la muestra:",n.2,"\n")

# Dado que la muestra es pequeña (menos de 30 observaciones), sería adecuado 
# utilizar la prueba t de Student para dos muestras pareadas. Pero antes debemos
# verificar las condiciones.

# Como se trata de atletas diferentes, menor al 10% de la población, podemos
# suponer que las observaciones son independientes entre sí.

# Ahora debemos verificar si las diferencias presentan una distribución
# cercana a la normal. Mediante un gráfico Q-Q podremos verificar esta distribución.

# Se genera un nuevo vector por el tiempo (previo-post).
# Para graficarlo lo transformamos en un dataframe y ver su distribución.

p_negra.diferencia_tiempos <- p_negra.previo - p_negra.post
data.p_negra.diferencia <- data.frame(p_negra.diferencia_tiempos)
grafico.p2 <- ggqqplot(data.p_negra.diferencia,
                       x = "p_negra.diferencia_tiempos",
                       color = "Blue",
                       xlab = "Teórico",
                       ylab = "Muestra",
                       title = "Gráfico Q-Q  v/s distribución normal (Raza Negra)")
grafico.p2

# La forma de los datos en el gráfico no se aleja tanto de una recta.
# Como no hay indicios de que tengamos que ser cautelosos con los resultados,
# indicamos un número de significación 0.05, dado que tiene menos 
# de un 5 % de probabilidad de que ocurra producto de la casualidad.

n_significacion <- 0.05

valor_ShapiroWilk.negra <- shapiro.test(p_negra.diferencia_tiempos)
valor_ShapiroWilk.negra

# Obteniendo un valor de  p=0.6622, siendo superior a nuestro
# nivel de significación (0.6622>0.05), se puede afirmar la relativa confianza 
# de la población donde proviene la muestra cumple con una distribución normal.

#Realizamos la pruebas t student para comprobar la hipótesis.

# Ahora debemos formular las hipótesis:
# H0: Tras el entrenamiento, la media de las mejores marcas de los atletas
#     negros en los 100 metros planos se reduce en 5,82 segundos
#     (mu.antes-después = 5,82 [s]).
# HA: Tras el entrenamiento, la media de las mejores marcas de los atletas
#     negros en los 100 metros planos se reduce en menos de 5,82 segundos
#     (mu.antes-después < 5,82 [s]).


#Asignamos nuestro valor nulo.
valor_nulo.p2 <- 5.82

prueba_T.negra <- t.test(p_negra.diferencia_tiempos, alternative="two.sided",mu=valor_nulo.p2,
                            conf.level = 1-n_significacion)
prueba_T.negra

# El valor p obtenido es menor que el nivel de significación (0.001659 < 0,05),
# por lo que rechazamos la hipótesis nula en favor de la alternativa

# En consecuencia, podemos concluir con 95% de confianza que, en promedio, la
# mejor marca de los atletas negros en los 100 metros planos  se reduce en menos
# 5,82 segundos tras el entrenamiento.

################################################################################

# pregunta 3

################################################################################

# ¿Es posible afirmar que, en promedio, los atletas negros superan a los
# orientales por menos de 3,64 segundos después del entrenamiento?

# Para responder esta pregunta debemos inferir acerca de la diferencia entre
# las medias de dos muestras independientes.

# Muestras con las que trabajaremos.
negros <- datos[datos[["Raza"]] == "Negra",][["Posterior"]]
orientales <- datos[datos[["Raza"]] == "Oriental",][["Posterior"]]

# Tamaño de las muestras.
n.3.negros <- length(negros)
n.3.orientales <- length(orientales)
cat("Tamaño de las muestras:", n.3.negros, "y", n.3.orientales, "\n")

# Como las muestras son pequeñas verificaremos las condiciones para trabajar 
# con la prueba t de Student.

# Como en el caso de ambas muestras se trata de atletas diferentes, menor al
# 10% de la población respectiva, podemos suponer que las observaciones son
# independientes entre sí.

# Verificaremos si cada una de las muestras presenta una distribución cercana
# a la normal
cat("\nComprobación de la normalidad de los datos:\n")
normalidad.3.negros <- shapiro.test(negros)
normalidad.3.orientales <- shapiro.test(orientales)
cat("Primera muestra:\n")
print(normalidad.3.negros)
cat("Segunda muestra:\n")
print(normalidad.3.orientales)

# Obteniendo un valor de  p = 0.1537 y p = 0.8099, siendo superior a nuestro 
# nivel de significación (0.1537>0.05 y 0.8099>0.05), se puede afirmar  la 
# relativa confianza de la población donde proviene la muestra cumple 
# con una distribución normal.

# Hipótesis:
# ¿Es posible afirmar que, en promedio, los atletas negros superan a los 
# orientales por menos de 3,64  segundos después del entrenamiento?
# H0: Tras el entrenamiento, los atletas negros superan a los orientales, 
#     en promedio, por 3,64 segundos (media.orientales - media.negros = 3,64 [s]).
# HA: Tras el entrenamiento, los atletas negros superan a los orientales, en 
#     promedio, por menos de 3,64 segundos (media.orientales - media.negros < 3,64 [s]).

# Prueba t de Student:
valor_nulo.3 <- 3.64
prueba.3 <- t.test(x = orientales, y = negros, alternative = "less",
                   mu = valor_nulo.3, paired = FALSE, conf.level = 1-n_significacion)

cat("Prueba de hipótesis:\n")
print(prueba.3)

# El valor p obtenido es menor que el nivel de significación (8.819x10^-11 < 0,05),
# se falla al rechazar la hipótesis nula en favor de la alternativa.

# En consecuencia, podemos concluir con un 95% de confianza que la media de
# los tiempos de los atletas negros es menor a 3.64[s] en comparación a los atletas orientales.

  
