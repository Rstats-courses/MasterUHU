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
# vamos a deshacer la estupidez de poner espacios como separador de miles
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
protected$area_2014_fixed <- gsub(" ","",protected$protected_area_2014)
protected$area_2014_fixed <- as.numeric(protected$area_2014_fixed)
protected$protected_area_2014 <- NULL
names(protected)[4] <- "protected_area_2014"
# igualmente, limpiar NAs
protected$protected_proportion_1990 <- as.numeric(protected$protected_proportion_1990)
protected$protected_proportion_2014 <- as.numeric(protected$protected_proportion_2014)
# parece que no hay, pero nos aseguramos
protected_clean <- protected[complete.cases(protected),]

gdp <- read.csv(file = "materiales/datasets/UN_GDP_infantmortality.csv",header = TRUE,stringsAsFactors = FALSE)
# por fin unos datos limpios :)

# uniremos los datos en un solo dataframe usando la variable "country"
# hay varios paises que no tienen nombres equivalentes en los tres datasets
# fijaos también que en "protected" y "agric" los nombres compuestos
# van separados por espacios (otra vez ¡MAL!)
# este trabajo es aburrido pero hay que hacerlo a mano
# para la clase, nos quedaremos solo con los que concuerdan

# primero unimos agric y protected
agpro <- dplyr::left_join(agric_clean,protected_clean,by = "country")
summary(agpro)

country_data <- dplyr::left_join(agpro,gdp,by = "country")
head(country_data)

# donde los nombres de paises no concuerdan, la funcion devuelve NA
# podemos ver cuales son estos paises
na.data <- subset(country_data,is.na(gdp))
head(na.data)
unique(na.data$country)

# y, ahora con todos los datos que necesitamos, repasamos de nuevo los datos
str(country_data)
summary(country_data)

# los archivos tratados, si los vamos a reutilizar,
# debemos guardarlos con nombre diferente a los archivos originales
# para saber en cada momento qué cambios hemos hecho
# write.csv2(country_data,"path",row.names = FALSE)

# 1.2.2 escoger variables -------------------------------------------------

# ¿que informacion necesitamos?
country_data <- country_data[,c("country","agric_change_1990","agric_proportion_2013","protected_proportion_2014","gdp")]
# de nuevo, mas limpieza 
country_data <- country_data[complete.cases(country_data),]

# 1.2.3 responder preguntas de investigacion ------------------------------

# (A)

mysample <- sample(x = nrow(country_data),size = 5,replace = FALSE)
mycountries <- country_data[mysample,]

# nos detenemos un poco en como graficar...
mycountries_long <- gather(mycountries,key = "area_type",value = "proportion",agric_proportion_2013,protected_proportion_2014)

proportion_plot <- ggplot(mycountries_long,aes(x = country,y = proportion))+
  # geom_col(aes(fill = area_type))+
  geom_col(aes(fill = area_type),color = "black",position="dodge")+
  # y lo hacemos algo mas bonito
  theme_bw()+
  scale_fill_manual(name="Proportion of area:",
                      labels=c("in agricultural use", "under protection"),
                      values = c("#E69F00","#009E73"))+
  NULL
proportion_plot

# (B)

agric_change_plot <- ggplot(country_data,aes(x = country,y = agric_change_1990))+
  geom_point() + 
  geom_abline(slope = 0, color = "darkgrey", linetype = "dotted")+
  NULL
agric_change_plot

# (C)

protected_gdp_plot <- ggplot(country_data,aes(x = protected_proportion_2014,y = gdp))+
  geom_point()+
  stat_smooth(method = "lm")+
  NULL
protected_gdp_plot











