#Grupo 5
#integrantes: 

require(dplyr)

dir <- "C:\\Users\\Mauri\\Desktop\\U\\semestre 2-2022\\IME 2-2022"
basename <- "EP01-DatosCovid.csv"
file <- file.path(dir, basename)
datos <- read.csv2(file = file)

# ¿Qué variables se han cargado?

# 

# ¿Qué tipo tiene cada una de estas variables?

#

# ¿Qué escala parecen tener estas variables?

#

#################### Pregunta 1 ####################
# ¿Qué día se produjo el mayor número de casos con síntomas en la región del Maule entre el 01-mar-2021 y el 31-ago-2021?

maule <- datos %>% filter(Region == "Maule")
intervalo <- maule %>% select(ends_with("01.03.2021"):ends_with("31.08.2021"))
max <- max(intervalo)

# 2. ¿Cuál fue el total de casos con síntomas para cada mes de este periodo?

marzo2021 <- rowSums(intervalo %>% select(ends_with("01.03.2021"):ends_with("31.03.2021")))
cat("Casos con síntomas Marzo-2021: ", marzo2021)

abril2021 <- rowSums(intervalo %>% select(ends_with("01.04.2021"):ends_with("30.04.2021")))
cat("Casos con síntomas Abril-2021: ", abril2021)

mayo2021 <- rowSums(intervalo %>% select(ends_with("01.05.2021"):ends_with("31.05.2021")))
cat("Casos con síntomas Mayo-2021: ", mayo2021)

junio2021 <- rowSums(intervalo %>% select(ends_with("01.06.2021"):ends_with("30.06.2021")))
cat("Casos con síntomas Junio-2021: ", junio2021)

julio2021 <- rowSums(intervalo %>% select(ends_with("01.07.2021"):ends_with("31.07.2021")))
cat("Casos con síntomas Julio-2021: ", julio2021)

agosto2021 <- rowSums(intervalo %>% select(ends_with("01.08.2021"):ends_with("31.08.2021")))
cat("Casos con síntomas Agosto-2021: ", agosto2021)
  