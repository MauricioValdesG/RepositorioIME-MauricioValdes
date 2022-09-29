#Grupo 5
#integrantes: 
# Mauricio Valdés
# Andrés Haussmann Garin
# Luis González
# Mauricio Vicencio

# librerias
require(dplyr)
require(ggpubr)
# 1. directorio y datos
dir <- "C:\\Users\\Mauri\\Desktop\\U\\semestre 2-2022\\IME 2-2022"
basename <- "EP02 Datos Casen 2017.csv"
file <- file.path(dir, basename)
datos <- read.csv2(file = file)

########### pregunta grupo 5 ##################

# ¿Son similares los ingresos registrados en las diferentes provincias de la RM?

# 4. Discutir y consensuar qué medidas estadísticas (media, mediana, moda, etc.) y qué forma gráfica ayudaría a responder la pregunta asignada.

# Se ocupará la "media" y se compararán cada una de las medias obtenidas en las distintas provincias.
# Para graficar se utilizará uno de barras, con la función ggbarplot, y se compararán donde el eje X serán las provincias y en el eje Y se encontrarán los valores (sueldos).

# 5. desarrollo script
region <- datos %>% filter(region == "Región Metropolitana de Santiago")

provincias <- region %>% filter(provincia != "")

#Se agrupan las provincias y se les asigna la media de ingresos respectiva
ingresosprovincias <- group_by(provincias, provincia) %>%
  summarise (count = n(), ingresos = mean(ytot))

#Se separan los ingresos y las provincias y luego se crea un dataframe con los datos
ingresos <- ingresosprovincias[["ingresos"]]
provincias <- ingresosprovincias[["provincia"]]
datos <- data.frame(provincias,ingresos)

#Se crea el gráfico de barras para ver los ingresos por provincia
g <- ggbarplot (datos,
                x = "provincias",
                y = "ingresos",
                title = "Ingresos por provincia",
                xlab = " Provincias ",
                ylab = " Ingresos [clp]")

print(g)
print(ingresosprovincias)

# 6. respuesta pregunta:

# Dado el gráfico obtenido, diremos que los ingresos registrados en las provincias de la región metropolitana no son similares, debido a que se observa
# una diferencia muy notoria entre las provincias de Santiago y Melipilla, ya que estas son las provincias con los ingresos medios máximos y mínimos respectivamente
# haciendo un simple cálculo se obtiene que los ingresos medios de Santiago son 2.33 veces más altos que en Melipilla.
