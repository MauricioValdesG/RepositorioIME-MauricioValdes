#Ejercicio Práctico 7 (EP07)
# Grupo 04
# Integrantes:
# - ALEXANDRA CATALINA NAVARRO CALDERÓN
# - CARLOS FRANCISCO RETAMALES APARICIO
# - MAURICIO SEBASTIÁN VALDÉS GÁLVEZ 
# - MAURICIO FELIPE VICENCIO PLAZA

#Paquetes.
library(dplyr)
library(RVAideMemoire)
library(tidyverse)
library(rcompanion)

################################################################################

# PREGUNTA 1

################################################################################

# Estudios científicos han descubierto que la inteligencia musical está altamente
# relacionada con la inteligencia matemática. Pensando en mejorar la oferta de 
# actividades culturales y recreativas, una Universidad ha examinado la preferencia
# de talleres de un grupo de 8 estudiantes de carreras científicas y 11 de carreras 
# humanistas, encontrando que 6 de los primeros y 5 de los segundos participaron 
# de talleres musicales. ¿Existe relación entre el tipo de carrera que cursan los 
# estudiantes y su participación en talleres musicales?

################################################################################

# En este caso usaremos pruebas para muestras pequeñas 

# Como los datos son independientes, es decir, no pareados, corresponde usar la 
# prueba exacta de Fisher.
# Las hipótesis son las siguientes:
# HO: La participación  en talleres musicales es independiente de la carrera.
# H1: La participación en talleres musicales depende de la carrera.

# Los datos son los siguientes.

cientifica <- c(6,2)
humanista <- c(5,6)
tabla.1 <- rbind(humanista,cientifica)
colnames(tabla.1) <- c("Sí","No")
print(tabla.1)

# Se establece un nivel de significación
alfa.p1 <- 0.05

# Se realiza la prueba exacta de Fisher.

prueba.1 <- fisher.test(tabla.1, conf.level = 1-alfa.p1)
cat("\nResultado de la prueba exacta de Fisher:\n")
print(prueba.1)

# El valor de p es de 0.3521, es mayor al nivel de significación de 0.05, de esta
# manera, se falla al rechazar la hipótesis nula. Así, se concluye, con 95% de
# confianza, que la proporción de participación en talleres musicales no depende
# de la carrera que escojan (humanista o científica).

################################################################################

# PREGUNTA 2

################################################################################

# Siempre tenaz en su lucha para erradicar a los vampiros de la faz de la tierra,
# Van Helsing desea probar una vacuna que, según él, causará una grave enfermedad
# en estos seres una vez que beban la sangre de sus víctimas. Para ello, 
# ha almacenado una gran cantidad de dosis de su propia sangre, separadas en dos grupos:
# uno de ellos contiene el químico de la vacuna, mientras el otro está completamente 
# limpio. Adicionalmente, Van Helsing cuenta con 13 vampiros cautivos, a los que 
# alimentó con sangre limpia por una semana. Luego de un periodo de limpieza 
# (durante el cual los vampiros fueron alimentados con su dieta normal, por lo que
# eliminaron todo rastro de la sangre de Van Helsing), repitió el experimento con
# la sangre que contiene la vacuna. Para ambos casos, registró cuántos vampiros
# enfermaron, con los siguientes resultados:
# ▪ 2 vampiros no presentaron enfermedad alguna con ninguna de las dietas de Van Helsing.
# ▪ 3 vampiros enfermaron tras ambas dietas de Van Helsing.
# ▪ 1 vampiro enfermó con la sangre limpia de Van Helsing, pero no con la sangre
#   que contiene la vacuna.
# ▪ 9 vampiros enfermaron con la sangre que contiene la vacuna, pero no con la 
#   sangre limpia de Van Helsing.

# ¿Es posible decir que la vacuna de Van Helsing causa una enfermedad en los vampiros?

################################################################################

# Primero se construye el data frame.
Vampiro_Vacuna <- c("No Enfermo", "No Enfermo", "Enfermo","Enfermo","Enfermo",
                    "No Enfermo","Enfermo","Enfermo","Enfermo","Enfermo",
                    "Enfermo","Enfermo","Enfermo","Enfermo","Enfermo")

Vampiro_Limpio <- c("No enfermo","No enfermo","Enfermo","Enfermo","Enfermo",
                    "Enfermo","No enfermo","No enfermo","No enfermo","No enfermo",
                    "No enfermo","No enfermo","No enfermo","No enfermo","No enfermo")

tabla2 <- tabla(data.frame(Vampiro_Vacuna,Vampiro_Limpio))                    
print(tabla2)

# Usaremos una prueba para muestras pequeñas. Además, sabemos que en este caso las observaciones
# están pareadas, por lo que corresponde usar la prueba de McNemar con las
# H0: La vacuna no causa la enfermedad en los vampiros 
# HA: La vacuna causa la enfermedad en los vampiros 

# Establecemos un nivel de significación.
alfa2 <- 0.05

# Se aplica la prueba de McNemar.
cat("\nResultado de la prueba de McNemar:\n")
prueba2 <- mcnemar.test(tabla2)
print(prueba2)

# Como el valor p obtenido es 0.026 < 0.05, por lo que se rechaza la hipótesis nula en favor de la alternativa.
# Por lo que, con el 95% de confianza se puede concluir que la vacuna de Van Helsing causa la enfermedad. 
################################################################################

# PREGUNTA 3

################################################################################

# El 21 de marzo de 2022 se realizó un estudio acerca de la aprobación al 
# presidente Gabriel Boric en una comunidad universitaria, obteniéndose los
# resultados que se muestran en la tabla. ¿Existe relación entre el
# estamento de la comunidad y la aprobación del presidente?

################################################################################
# H0: El estamento de la comunidad y la aprobación del presidente no tienen relación.
# HA: El estamento de la comunidad y la aprobación del presidente están relacionadas.

aprueba <- c(96, 103, 21)
desaprueba <- c(119, 132, 34)
tabla3 <- rbind(aprueba, desaprueba)

#Se supone que las observaciones son independientes y que el estudio llevado a cabo es rigurosa,

# Ahora debemos comprobar cuántas observaciones se esperan en cada grupo.
margen.fila <- apply(tabla3, 1, sum)
margen.columna <- apply(tabla3, 2, sum)
n3 <- sum(tabla3)
tabla3.2 <- margen.fila %*% t(margen.columna) / n3
cat("\nFrecuencias esperadas:\n")
print(tabla3.2)

# Puesto que en cada caso se esperan más de 5 observaciones, podemos proceder
# sin problemas con la prueba seleccionada. Consideremos un nivel de
# significación de 0,05.
prueba3 <- chisq.test(x = tabla3)
cat("\nResultado de la prueba chi-cuadrado de independencia:\n")
print(prueba3)

#Con un p-valor de 0.6845 > 0.05, se falla en rechazar la hipótesis nula en favor de la alternativa.




################################################################################

# PREGUNTA 4

################################################################################

# La Facultad de Ingeniería desea saber si existe diferencia significativa en el 
# desempeño de los estudiantes en asignaturas críticas de primer semestre. 
# Para ello, le ha entregado un archivo de datos que, para 3 asignaturas, 
# indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir
# la Facultad?
# Indicación: obtenga la muestra a partir del archivo EP07 Datos.csv, usando la
# semilla 592. Considere un nivel de significación α=0,05.

################################################################################

# Se tiene la variable independiente, siendo el estudiante que tiene
# 3 observaciones pareadas de una variable de respuesta dicotómica, es decir,
# si reprueba o aprueba cada una de las asignaturas. La herramienta a usar es 
# la Q de Cochran.
# Las hipótesis son las siguientes:
# H0: La tasa de aprobación es la misma para Cálculo, Álgebra y Física.
# HA: Al menos una de las asignaturas (Cálculo, Álgebra y Física) tiene una tasa
#     de aprobación distinta.

# Se cargan los datos y el espacio de trabajo.
setwd("E:/OneDrive - usach.cl/2022-2/8°Semestre/IME/IME-EP/EP07")
datos <- read.csv2(file = "EP07 Datos.csv", stringsAsFactors = TRUE)

# Sacamos la muestra y le asignamos la semilla correspondiente.
set.seed(592)
muestra <- sample_n(datos, size = 50, replace = FALSE)

#Verifacamos las condiciones
# Presenciamos variable de respuesta dicotómica en los Aprueba y Reprueba, y 
# la variable independiente siendo la asignatura, es categórica con los niveles
# de Cálculo, Álgebra y Física.
# Dado que la muestra de estudiantes seleccionada fue al azar y el tamaño de dicha
# muestra, equivale al 10% de los estudiantes que han cursado la asignatura(de 
# manera histórica), se puede asumir que las observaciones son independientes 
# entre sí.
# También la muestra tiene 50 observaciones y 3 niveles en la variable independiente
# entonces tambien se cumple 50 * 3 = 150 >=24
# Entonces, se puede utilizar la prueba Q de Chochran para este problema.


muestra <- muestra %>% pivot_longer(c("Calculo", "Algebra", "Fisica"),
                                    names_to = "Curso", values_to = "Situacion")

muestra[["Curso"]] <- factor(muestra[["Curso"]])

# Se realiza la prueba Q de Cochran. Recalcar que esta prueba comprueba la 
# igualdad de todas las proporciones, siendo una prueba ómnibus.

alfa <- 0.05
prueba.4 <- cochran.qtest(Situacion ~ Curso | Id, data = muestra, alpha = alfa)
cat("Resultado de la prueba Q de Cochran\n")
print(prueba.4)

# El valor p obtenido es menor que el nivel de significación fijado para la
# prueba (0.2035<0.05), por lo que rechazamos la hipótesis nula en favor 
# de la hipótesis alternativa. Se concluye, con 95% de confianza, que a lo 
# menos una de las  asignaturas (Cálculo, Álgebra o Física) tiene una tasa de
# aprobación distinta a las demás.

# Siendo que la prueba ómnibus encontró diferencias estadísticamente significativas,
# tenemos que llevar a cabo un procedimiento post-hoc para determinar
# cuáles son esas diferencias. Dado que tiene un mayor poder estadístico,
# usaremos la corrección de Holm.

post.hoc <- pairwiseMcnemar(Situacion ~ Curso | Id, data = muestra,
                            method = "holm")

cat("\nResultado del procedimiento post-hoc usando la corrección de Holm")
print(post.hoc)

# El resultado  nos muestra que se observa una diferencia significativa
# entre Álgebra y Cálculo, pero no se evidencia una diferencia importante
# entre estas dos asignaturas y Física.




