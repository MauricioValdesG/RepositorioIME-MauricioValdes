#Trabajo grupo 3
#Integrantes:
#FABRIZIO ANTONIO MONTUSCHI ALMENDRAS
#BENJAMÍN CRISTOPHER SAGREDO DURÁN
#DYLLAN IGNACIO SALGADO ESPINOZA
#MAURICIO SEBASTIÁN VALDÉS GÁLVEZ

#ENUNCIADO
#------------------------------------------------------------------------------
#Ñami-Ñam, compañía dedicada a la elaboración y comercialización de golosinas, 
#se prepara para lanzar una nueva línea de productos al mercado. Para asegurar 
#el éxito comercial, ha solicitado a varias empresas de diseño la creación de 
#un empaque para cada uno de los nuevos productos. A fin de decidir qué envase 
#es mejor para cada producto y evaluar un contrato permanente con una de las 
#empresas de diseño, Ñami-Ñam ha reclutado a 2.000 voluntarios de todo el país,
#seleccionados aleatoriamente entre los participantes de un concurso efectuado 
#por Ñami-Ñam el año anterior. Cada participante debe puntuar las distintas 
#alternativas de envase para un producto (seleccionado al azar) mediante una 
#escala Likert de 7 puntos, donde: 1: el envase es muy poco atractivo y 7: el 
#envase es muy atractivo. Los datos recolectados contemplan las siguientes 
#variables:

#▪ Id: identificador único de cada participante.
#▪ Edad: rango etario del participante. Variable categórica con los niveles 
#  Niño, Joven, Adulto.
#▪ Producto: producto para el cual se evalúan los empaques. Variable categórica 
#  con los niveles Alfajor, Caramelos, Chocolate, Cuchuflí, Galletas, Queque.
#▪ Diseno: empresa que diseñó el envase. Variable categórica con los niveles 
#  DisenoColor, KoolDesign, LaKajita,PackPro.
#▪ Puntaje: puntuación obtenida por el envase. Entero [1-7].

# Importar los paquetes, instalándolos de ser necesario.
if (!require(tidyverse)) {
  install.packages("tidyverse", dependencies = TRUE)
  require(tidyverse) }

# Se setea el workspace
setwd("C:\\Users\\Dyllan\\Desktop\\TrabajosIME\\EP-10")
# Se lee el archivo con los datos
datos <- read.csv2("EP10 Datos.csv",encoding = "UTF-8")

#Se establece un nivel de significación para las 2 preguntas.

alfa <- 0.05

#1. ¿Existe diferencia en la puntuación obtenida por los envases diseñados por
#   LaKajita según las evaluaciones realizadas por niños y jóvenes?

#Hipotesis:

#H0: No hay diferencia en la puntuación obtenida por los envases diseñados por
#    LaKajita según las evaluaciones realizadas por niños y jóvenes.

#HA: Si hay diferencia en la puntuación obtenida por los envases diseñados por
#    LaKajita según las evaluaciones realizadas por niños y jóvenes.

# Se obtienen los envases diseñados por LaKajita solicitado por el enunciado.
diseñoLaKajita <- datos %>% filter (Diseno=="LaKajita")
# Ahora se obtienen los datos de ninos y joven que evaluaron los envases
# de LaKajita
ninosLaKajita <- diseñoLaKajita %>% filter (Edad=="Nino")
jovenesLaKajita <- diseñoLaKajita %>% filter (Edad=="Joven")

# Para responder a la pregunta 1 se debe utilizar la Prueba de rangos de 
# Wilcoxon, ya que nos estan solicitando inferir con medias no parametricas,
# ademas no se puede utilizar la prueba t de Student, por que al trabajar
# con calificaciones no todas las escalas de Likert pueden asegurar que tengan
# igual intervalo. 

# Para el uso de Wilcoxon se deben verificar 2 puntos importantes:

# 1. Las observaciones de ambas muestras son independientes.
# Este punto si se verifica, ya que el enunciado nos dice que los participantes
# han sido seleccionado de forma aleatoria y ademas cada participante cuenta
# con un ID unico.

# 2. La escala de medición empleada debe ser a lo menos ordinal, de modo que 
# tenga sentido hablar de relaciones de orden (“igual que”, “menor que”, 
# “mayor o igual que”).
# Este punto tambien se verifica, ya que la pregunta nos solicita verificar
# si existe diferencia entre la puntuación o calificacion obtenidas. Por lo que
# se debe utilizar la información de Puntaje que en este caso si tiene una
# escala ordinal, ya que permite establecer una relación de orden que va de 
# 1 a 7, donde 1 es envase muy poco atractivo y 7 el envase es muy atractivo.

# Una vez verificado que se puede utilizar la Prueba de rangos de Wilcoxon,
# se debe utilizar la función wilcox.test, por lo que los datos
# ninosLaKajita y jovenesLaKajita se deben pasar a vectores para su uso, si 
# no se cambia a vector este mostrará un mensaje de error.

ninosLaKajitaVector <- as.vector(ninosLaKajita$Puntaje)
jovenesLaKajitaVector <- as.vector(jovenesLaKajita$Puntaje)

#Una vez pasados los datos a vector se procede a realizar la función
# de wilcox.test.
wilcoxon <- wilcox.test(ninosLaKajitaVector, jovenesLaKajitaVector, 
                              alternative = "two.sided", 
                              conf.level = 1 - alfa)
print(wilcoxon)

#Se puede apreciar que el P-value obtenido con wilcoxon corresponde a 
# 0.51, lo cual es mucho mayor al nivel de significación establecido de 0.05,
# por lo que p-value > alfa, por ende se falla en rechazar la hipótesis nula 
# en favor de la hipótesis alternativa, por lo que se concluye con un 95% de 
# confianza que no hay diferencia en la puntuación obtenida por los 
# envases diseñados por LaKajita según las evaluaciones realizadas por niños y 
# jóvenes.


#2. ¿Existen diferencias entre las puntuaciones obtenidas para los diferentes 
#   envases de chocolate? De ser así, ¿cuál(es) envase(s) se diferencia(n) de 
#   los demás? 


