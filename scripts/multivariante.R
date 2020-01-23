#This scripts introduce multivariate analysis

library(vegan)

#Correlation
data("iris")
View(iris)
summary(iris)
str(iris)
head(iris)
iris

plot(iris)

iris$Sepal.Length
iris[,"Sepal.Length"]
iris[,1]

plot(x = iris$Sepal.Length,
     y = iris$Petal.Length,
     col = "red")

iris$color <- iris$Species
levels(iris$color) <-
  c("red", "purple", "orange")

plot(x = iris$Sepal.Length,
     y = iris$Petal.Length,
     col = as.character(iris$color))


plot(iris$Sepal.Length ~ iris$Petal.Length)
scatter.smooth(iris$Sepal.Length ~ iris$Petal.Length)
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris$Sepal.Length, iris$Petal.Length, method = "spearman")
cor.test(x = iris$Sepal.Length,
         y = iris$Petal.Length)

setosa_only <- subset(iris, Species == "setosa")
#> < mayor/menos que
# == igual que
# != diferente de
# & y
# | o

pairs(iris)
cor(x = iris) #fails
str(iris)

cor(x = iris[,1:4])
cor(x = iris[,c(1,2,3,4)])
cor(iris[,-5])

#PCA----

#prepare and explore the data
ir <- iris[, 1:4]
ir_species <- iris[, 5]
pairs(ir)
cor(ir)

#run pca
#princomp

pca <- prcomp(x = ir)
summary(pca) #PC contributions
plot(pca)
biplot(pca)
biplot(pca, choices = c(1,3))
pca #loadings o pesos
pca$x #scores o valores de cada observacion

pca2 <- prcomp(x = ir, center = TRUE, scale. = TRUE)
summary(pca2) #PC contributions
plot(pca2)
biplot(pca2, choices = c(1,2))
pca2 #loadings o pesos
pca2$x #scores o valores
summary(iris)

#predicción
losdatosdelcolega <- data.frame(
  Sepal.Length = c(4.3, 5.4),
  Sepal.Width = c(2.5, 3.0),
  Petal.Length = c(1.8, 2.0),
  Petal.Width = c(0.5, 1.3))


prediccion <- predict(pca2,
                      newdata = losdatosdelcolega)

prediccion

names(pca2)
#pca2$center
#pca2$scale

biplot(pca2)
points(x = prediccion[,1],
       y = prediccion[,2],
       col = "red", pch = 19)

#a nicer plot:
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              groups = ir_species, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)


#NMDS and PERMANOVA----
#The data
Herbivores <- read.csv(file = "datasets/Herbivore_specialisation.csv", header = TRUE)
head(Herbivores)
str(herbivory)
summary(herbivory)
hist(herbivory$Mass)

#simplify objects to use
Habitat <- Herbivores$Habitat
DayNight <- Herbivores$DayNight
#select the community
Herb_community <- Herbivores[,5:11]

#cargar librerias----
#install.packages("vegan") #si no tienes la libreria, instalala primero
library("vegan")

#The basic is the distance measure you use:
#distance selection!----
?dist
?vegdist
?betadiver

v <- vegdist(Herb_community, "horn")
head(v, 20)
Herb_community[c(1,13),]
vegdist(Herb_community[c(1,13),], "horn")

#A few words:
#binary:
#Jackard
#Sorensen (This coefficient weights matches in species composition
#between the two samples more heavily than mismatches)
#quantitative
#Euclidian: simple distance, good for e.g. distance between sites
#bray: 0-1 The Bray-Curtis measure ignores cases in which the species
#is absent in both community samples, and it is dominated
#by the abundant species so that rare species add very little to the
#value of the coefficient
#morisita: 0-1 independent of sample size. Only for counts. Recomended.
#kulczynski: Weigth more rare species.
#gower (allows factors!)

#Best is Legendre book numerical ecology
#(only found Krebs online): http://www.zoology.ubc.ca/~krebs/downloads/krebs_chapter_12_2014.pdf

#NMDS
Herb_community.mds <- metaMDS(comm = Herb_community,
                              distance = "bray",
                              trace = FALSE, autotransform = FALSE)
#Herb_community.mds <- metaMDS(v) #idem
plot(Herb_community.mds$points, col = Habitat, pch = 16)

Habitat.uni <- unique(Habitat)
legend(x = -2.5, y = -1.5, Habitat.uni, pch=16, col = 1:5, cex = 0.8)

plot(Herb_community.mds$points, col = DayNight, pch = 16)
legend(x = -2.5, y = -1.5, c("Day","Night"), pch=16, col = 1:5, cex = 0.8)

#assumptions:
Herb_community.mds$stress

#If the stress value is greater than 0.2, it is advisable to include an
#additional dimension, but remember that human brains are not very well
#equipped to visualise objects in more than 2-dimensions.

#Transformation and standardisation. Transforming data sets prior to
#creating a MDS plot is often desirable, not to meet assumptions of normality,
#but to reduce the influence of extreme values. For example,

Herb_community.sq <- sqrt(Herb_community)
Herb_community.sq.mds <- metaMDS(comm = Herb_community.sq,
                                 distance = "bray", autotransform = FALSE)
plot(Herb_community.sq.mds$points, col = Habitat, pch = 16)

#alternative plot
ordiplot(Herb_community.mds,type="n")
points(Herb_community.mds$points, cex = 0.5,
       pch = 16,
       col = "darkgrey")
ordihull(Herb_community.mds,groups=Habitat,draw="polygon",col="grey90",
         label=FALSE)
orditorp(Herb_community.mds,display="species",col="red",air=0.01)
ordiellipse(ord = Herb_community.mds,
            groups = Habitat,
            col = "red")


#Testing hypothesis PERMANOVA----
#First test: Are centroids different?
adonis(Herb_community ~ Habitat, method = "bray")
adonis(Herb_community ~ DayNight, method = "bray")
#Second test: Is the spread different?
b <- betadisper(vegdist(Herb_community, method = "bray"), group = Habitat)
anova(b)
boxplot(b)
TukeyHSD(b)

b <- betadisper(vegdist(Herb_community, method = "bray"), group = DayNight)
anova(b)
boxplot(b)
TukeyHSD(b)

