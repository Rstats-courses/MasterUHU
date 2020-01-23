# Tecnicas estadisticas - Master UHU 2019


# 1 - Tratamiento de datos --------------------------------------------------

# Usaremos tres conjuntos de datos para responder una serie de preguntas basicas,
# y en el proceso, ejemplificar como se organizan datos diversos para su analisis

# 1.1 Exploracion de los datos originales ---------------------------------

# en el repo: un_agricultural_land.xlsx, terrestrial_protected_areas.xlsx,UN_GDP_infantmortality.csv

# ¿nombres de variables? ¿codificación de números? ¿valores nulos?

# 1.2 preguntas de investigacion ------------------------------------------

# (A) graficar el porcentaje de uso del suelo agricola y areas protegidas para 5 paises
# (B) graficar el cambio en porcentaje de area agricola desde 1990 para todos los paises
# (C) graficar la relacion entre GDP y porcentaje de areas protegidas

# 1.2.1 leer, limpiar y unir los tres conjuntos de datos ---------------------------
setwd("~/Data/Work/documents/docencia/MasterUHU_2019") # just in case, switch if necessary
library(tidyverse)

agric <- read.csv2(file = "materiales/datasets/agricultural_land.csv",header=TRUE,stringsAsFactors = FALSE)
head(agric)
# vamos a deshacer la chorrada de poner espacios como separador de miles
?gsub
agric$agricultural_area_2013_fixed <- gsub(" ","",agric$agricultural_area_2013)
head(agric)
str(agric)
# sigue siendo una columna de tipo caracter, la convertimos en numerica
agric$agricultural_area_2013_fixed <- as.numeric(agric$agricultural_area_2013_fixed)
str(agric)
summary(agric)
# por comodidad, borramos la columna original y ponemos nombres sencillos
agric$agricultural_area_2013 <- NULL
names(agric) <- c("country","agric_change_1990","agric_proportion_2013","agric_area_2013")
head(agric)
# además, hay valores NA que fueron introducidos como "..."
# R no los lee adecuadamente, y cree que las columnas son caracteres
# en vez de numericas
str(agric)
agric$agric_change_1990 <- as.numeric(agric$agric_change_1990)
agric$agric_proportion_2013 <- as.numeric(agric$agric_proportion_2013)
summary(agric)
# por ultimo, nos quedamos solo con paises que tienen informacion completa
agric_clean <- agric[complete.cases(agric),]

protected <- read.csv2(file = "materiales/datasets/protected_areas.csv",header=TRUE,stringsAsFactors = FALSE)
# el mismo error

gdp <- read.csv(file = "materiales/datasets/UN_GDP_infantmortality.csv",header = TRUE,stringsAsFactors = FALSE)
# por fin unos datos limpios :)

# uniremos los datos en un solo dataframe usando la variable comun "country"
# hay varios paises que no tienen nombres equivalentes en los tres datasets
# fijaos también que en "protected" y "agric" los nombres compuestos
# van separados por espacios (otra vez ¡MAL!)
# este trabajo es aburrido pero hay que hacerlo a mano
# para la clase, nos quedaremos solo con los paises cuyos nombres concuerdan

# primero unimos agric y protected
?left_join

# donde los nombres de paises no concuerdan, la funcion devuelve NA


# 1.2.2 escoger variables -------------------------------------------------

# ¿que informacion necesitamos?

# 1.2.3 responder preguntas de investigacion ------------------------------

# (A)

?sample
?gather

# (B)
# (C)













