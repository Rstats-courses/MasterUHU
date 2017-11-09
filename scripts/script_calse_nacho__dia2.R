# Analizar comunidades de herbivoros marinos 
# usando NMDS y PERMANOVAS

#leer datos----

#read.table
herbivory <- read.csv(file = "hervivory.csv")
head(herbivory)
str(herbivory)
summary(herbivory)
hist(herbivory$Mass)


#cargar librerias----
#install.packages("vegan") #si no tienes la libreria, instalala primero
library("vegan")

#simplify objects
head(herbivory)
herb_community <- herbivory[,c(5:11)]



?dist
?vegdist
?betadiver
v <- vegdist(herb_community, "horn")
head(v, 20)
herb_community[c(1,13),]
vegdist(herb_community[c(1,13),], "horn")

mds <- metaMDS(herb_community, autotransform = FALSE)
mds <- metaMDS(v)

plot(mds$points, col = herbivory$Habitat)
legend(x = -0.4, y = 0,  
       unique(herbivory$DayNight),
       col = 1:2, cex = 0.5, pch = 16)

mds

#Test PERMANOVA

model <- adonis(formula = herb_community ~ herbivory$Habitat)
summary(model)
model

beta <- betadisper(d = vegdist(herb_community), 
           group = herbivory[,"Habitat"],
           type = "centroid")
beta
anova(beta)
boxplot(beta, las = 2)
TukeyHSD(beta)



model <- adonis(herb_community ~ herbivory$Habitat + herbivory$Mass)
model
boxplot(herbivory$Mass ~ herbivory$Habitat)


plot(mds$points, col = herbivory$Habitat)

ordiplot(ord = mds, type = "n")
ordihull(ord = mds, 
         groups = herbivory$Habitat,
         draw = "polygon",
         col = "grey",
         label = FALSE)

points(mds$points, cex = 0.5, 
       pch = 16,
       col = "darkgrey")
ordiellipse(ord = mds,
            groups = herbivory$Habitat,
            col = "red")
