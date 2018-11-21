#This scripts introduce multivariate analysis

library(vegan)

#Correlation
data(iris)
head(iris)

plot(iris$Sepal.Length ~ iris$Petal.Length)
scatter.smooth(iris$Sepal.Length ~ iris$Petal.Length)
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris$Sepal.Length, iris$Petal.Length, method = "spearman")


#PCA----

#prepare and explore the data
ir <- iris[, 1:4]
ir_species <- iris[, 5]
pairs(ir)
cor(ir)
#run pca
#princomp
pca <- prcomp(ir, center = TRUE,
              scale. = TRUE)
pca
summary(pca)
plot(pca, type = "l")
biplot(pca)
#or nicer:
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              groups = ir_species, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)
#more tricks
predict(pca, newdata=(tail(ir, 2)))

#NMDS and PERMANOVA----
#The data
Herbivores <- read.csv(file = "datasets/Herbivore_specialisation.csv", header = TRUE)
head(Herbivores)

#simplify objects to use
Habitat <- Herbivores$Habitat
DayNight <- Herbivores$DayNight
#select the community
Herb_community <- Herbivores[,5:11]

#The basic is the distance measure you use:
#distance selection!----
?dist
?vegdist
?betadiver

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
plot(Herb_community.mds$points, col = Habitat, pch = 16)

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
                                 distance = "bray", trace = FALSE)
plot(Herb_community.sq.mds$points, col = Habitat, pch = 16)

#alternative plot
ordiplot(Herb_community.mds,type="n")
ordihull(Herb_community.mds,groups=Habitat,draw="polygon",col="grey90",
         label=FALSE)
orditorp(Herb_community.mds,display="species",col="red",air=0.01)

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

