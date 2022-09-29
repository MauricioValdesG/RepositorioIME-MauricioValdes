#Ejercicio Práctico 4 (EP04)
# Grupo 04
# Integrantes:
# - ALEXANDRA CATALINA NAVARRO CALDERÓN
# - CARLOS FRANCISCO RETAMALES APARICIO
# - MAURICIO SEBASTIÁN VALDÉS GÁLVEZ 
# - MAURICIO FELIPE VICENCIO PLAZA

#Fijamos espacio de trabajo.
setwd("C:\\Users\\Mauri\\Desktop\\U\\semestre 2-2022\\IME 2-2022")

#Paquetes.
library(ggpubr)
library(dplyr)

#Datos.
datos = read.csv2("EP04 datos.csv")
# Pregunta 1

# El Comité Olímpico cree que el mejor tiempo medio de los atletas orientales antes de ingresar al programa
#de entrenamiento es superior a 18,83 segundos. ¿Soportan los datos esta afirmación?

# A continuación formamos nuestra hipótesis nula y nuestra hipótesis alternativa,
# siendo H0 y H1 respectivamente.
# H0  mu <= 18,83
# HA  mu > 18,83 
# Valor nulo = 18.83
# Nivel de significación = 0.05 (Nivel de confianza 95%). 

#Filtramos los datos, por la raza, en este caso, extrayendo la raza "Oriental" 
# y así obtener a los atletas orientales
p_oriental <- datos %>% filter(Raza == "Oriental")
p_oriental


#Al indicarnos "antes" de ingresar al programa, se selecciona la variable "Previo".
#Usando "pull", esta función nos permite seleccionar una columna (en este caso "Previo")
#y transformarla a vector.

p_oriental.previo <- pull(p_oriental,Previo)
p_oriental.previo

#Asignamos el nivel de significación y el valor nulo.
#Al indicar un número de significación 0.05, tiene menos 
#de un 5 % de probabilidad de que ocurra producto de la casualidad.
n_significacion <- 0.05
valor_nulo.p1 <- 18.83

#Al ver que los valores que estamos trabajando no afectan a otra muestra, se puede asumir 
#que son independientes entre sí. 
#Realizando una prueba de Shapiro - Wilk podemos observar el comportamiento de la distribución de la muestra.

valor_ShapiroWilk.oriental <- shapiro.test(p_oriental.previo)
print(valor_ShapiroWilk.oriental)

#Obteniendo un valor de p = 0.4826, siendo superior a nuestro nivel de significación (0.4826>0.05), se puede afirmar
#la relativa confianza de la población donde proviene la muestra cumple con una distribución normal.

#Se grafica para observar esta distribución.
data.p_oriental <- data.frame(p_oriental.previo)
grafico.p1 <- ggqqplot(data.p_oriental,
                       x = "p_oriental.previo",
                       color = "Red",
                       xlab = "Teórico",
                       ylab = "Muestra",
                       title = "Gráfico Q-Q Muestra v/s Distribución Normal")
grafico.p1

#Verificado el valor, se realiza la prueba t student.
prueba_T.Oriental_previa <- t.test(p_oriental.previo,
                                   alternative = "greater",
                                   mu = valor_nulo.p1,
                                   conf.level = 1 - n_significacion)
prueba_T.Oriental_previa

#Pregunta 2

#¿Sugieren los datos que la mejor marca de los atletas negros se reduce en 5,82 segundos tras el
#entrenamiento?

#Filtramos la población de atletas por la raza, siendo la variable de la raza "Negra".
#Después separamos en vectores las columnas "Previo" y "Posterior".
#Definimos nuestras hipotesis (nula y alternativa).
# H0: mu <= 5.82 HA: mu >5.82
# Valor nulo = 5.82
# Nivel de significación = 0.05 (95%, con el mismo motivo que se expuso arriba)

p_negra <- datos %>% filter (Raza == "Negra")
p_negra.previo <- pull(p_negra,Previo)
p_negra.post <- pull(p_negra,Posterior)

#Se genera un nuevo vector por el tiempo (previo-post).
#Para graficarlo lo transformamos en un dataframe y ver su distribución.

p_negra.diferencia_tiempos <- p_negra.previo - p_negra.post
data.p_negra.diferencia <- data.frame(p_negra.diferencia_tiempos)
grafico.p2 <- ggqqplot(data.p_negra.diferencia,
                       x = "p_negra.diferencia_tiempos",
                       color = "Blue",
                       xlab = "Teórico",
                       ylab = "Muestra",
                       title = "Gráfico Q-Q  v/s distribución normal (Raza Negra)")
grafico.p2

valor_ShapiroWilk.negra <- shapiro.test(p_negra.diferencia_tiempos)
valor_ShapiroWilk.negra

#Obteniendo un valor de  p=0.6622, siendo superior a nuestro nivel de significación (0.6622>0.05), se puede afirmar
#la relativa confianza de la población donde proviene la muestra cumple con una distribución normal.

#Asignamos nuestro valor nulo.
valor_nulo.p2 <- 5.82

#Realizamos la pruebas t student para comprobar la hipótesis.

prueba_T.negra_p1 <- t.test(p_negra.diferencia_tiempos,
                            alternative = "greater",
                            mu = valor_nulo.p2,
                            conf.level = 1 - n_significacion)
prueba_T.negra_p1


prueba_T.negra_p2 <- t.test(x=p_negra.previo,
                            y=p_negra.post,
                            paired=TRUE,
                            alternative="greater",
                            mu=valor_nulo.p2,
                            conf.level = 1 - n_significacion)

prueba_T.negra_p2

# pregunta 3

# ¿Es posible afirmar que, en promedio, los atletas negros superan a los orientales por menos de 3,64
# segundos después del entrenamiento?

cat("\nPregunta 3\n")

# Para responder esta pregunta debemos inferir acerca de la diferencia entre las medias de dos
# muestras independientes.

# Muestras con las que trabajaremos.
negros <- datos[datos[["Raza"]] == "Negra",][["Posterior"]]
orientales <- datos[datos[["Raza"]] == "Oriental",][["Posterior"]]

# Tamaño de las muestras.
n.3.negros <- length(negros)
n.3.orientales <- length(orientales)
cat("Tamaño de las muestras:", n.3.negros, "y", n.3.orientales, "\n")

# Como las muestras son pequeñas verificaremos las condiciones para trabajar con la prueba t de Student.

# Como en el caso de ambas muestras se trata de atletas diferentes, menor al
# 10% de la población respectiva, podemos suponer que las observaciones son
# independientes entre sí.

# Verificaremos si cada una de las muestras presenta una distribución cercana a la normal
cat("\nComprobación de la normalidad de los datos:\n")
normalidad.3.negros <- shapiro.test(negros)
normalidad.3.orientales <- shapiro.test(orientales)
cat("Primera muestra:\n")
print(normalidad.3.negros)
cat("Segunda muestra:\n")
print(normalidad.3.orientales)

# Obteniendo un valor de  p = 0.1537 y p = 0.8099, siendo superior a nuestro nivel de significación (0.1537>0.05 y 0.8099>0.05), se puede afirmar
# la relativa confianza de la población donde proviene la muestra cumple con una distribución normal.

# Hipótesis:
# ¿Es posible afirmar que, en promedio, los atletas negros superan a los orientales por menos de 3,64
# segundos después del entrenamiento?
# H0: Tras el entrenamiento, los atletas negros superan a los orientales, en promedio, por 3,64 segundos
#     (media.orientales - media.negros = 3,64 [s]).
# HA: Tras el entrenamiento, los atletas negros superan a los orientales, en promedio, por menos de 3,64 segundos
#     (media.orientales - media.negros < 3,64 [s]).

# Prueba t de Student:
valor_nulo.3 <- 3.64
prueba.3 <- t.test(x = orientales, y = negros, alternative = "less",
                   mu = valor_nulo.3, paired = FALSE, conf.level = 1-n_significacion)

cat("Prueba de hipótesis:\n")
print(prueba.3)

# El valor p obtenido es menor que el nivel de significación (8.819x10^-11 < 0,05),
# se falla al rechazar la hipótesis nula en favor de la alternativa.

# En consecuencia, podemos concluir con un 95% de confianza que la media de los tiempos de los atletas negros 
# es menor a 3.64[s] en comparación a los atletas orientales.
