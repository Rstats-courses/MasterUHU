#This scripts introduce multivariate analysis

#Introducing a problem.
data(iris)
head(iris)

plot(iris$Sepal.Length, iris$Petal.Length)
setosa <- subset(iris, Species == "setosa")
plot(setosa$Sepal.Length, setosa$Petal.Length)
plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species)
plot(iris$Sepal.Length, iris$Petal.Length, col = 2)
plot(iris$Sepal.Length, iris$Petal.Length, col = "red")

cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris$Sepal.Length, iris$Petal.Length, method = "spearman")
cor.test(iris$Sepal.Length, iris$Petal.Length)

#undertand all the data----
pairs(ir)

cor(iris) #error
cor(iris[,1:4])
cor(iris[,-5])

#PCA----

#prepare and explore the data
ir <- iris[, 1:4]
ir_species <- iris[, 5]
cor(ir)
summary(ir) #check ranges...

#run pca
#princomp
pca <- prcomp(ir, center = TRUE,
              scale. = TRUE)
pca
summary(pca)
plot(pca, type = "l")
biplot(pca)
names(pca)
loadings(pca)
pca$x #scores or loadings...


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

# tipos de datos----

x <- 3.14     # numeric
y <- "hello"  # character
z <- TRUE     # logical

# Y si dudamos que estructura tienen nuestros datos?
str(z)

# crear vectores: seq y rep
myvector <- seq(1, 5)
myvector <- rep(1, 5)
myvector

# operar con vectores (c, log y sum)
myvector * 2
myvector + 2
myvector * myvector
c(myvector, myvector)
log(myvector)
sum(myvector)

# ojo, R recicla valores
1:5 * 1:15 #util, pero peligroso
1:5 + 1:4

#data no available
myvector <- c(1, , 3, 4, 5) #fails
myvector <- c(1, NA, 3, 4, 5)
# R reconoce los NA's! (is.na)
is.na(myvector)

# pero las funcionen pueden fallar cuando hay NA's (sum)
x <- c(1, 2, 3, NA)
sum(x) # NA
sum(x, na.rm = TRUE)

#Otros: NAN, NULL Inf...NA puede ser evaluado por funciones NULL no.

# preguntando a R (which)
which(is.na(myvector)) # == TRUE
which(myvector == 3)

#matrices
(m <- matrix(myvector, nrow = 5, ncol = 2, byrow = FALSE)) #fijate que aquí
#reciclamos el vector!

#index
m[2,1]
m[,1]

#lists
l <- list(m, myvector)
l[[1]][1,2]


#NMDS and PERMANOVA----

library(vegan)

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

