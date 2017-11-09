#Este archivo sera un ejemplo de analisis 
 #multivarainte

#correlaciones----

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
     col = as.character(iris$color))

plot(x = iris$Sepal.Length, 
     y = iris$Petal.Length, 
     col = "red")
str(iris$Species)

iris$color <- iris$Species
levels(iris$color) <- 
  c("red", "purple", "orange")

setosa_only <- subset(iris, Species == "setosa")
#> < mayor/menos que
# == igual que
# != diferente de
# & y 
# | o

cor(x = iris$Sepal.Length,
    y = iris$Petal.Length, method = "spearman")

cor.test(x = iris$Sepal.Length,
         y = iris$Petal.Length)


pairs(iris)
cor(x = iris)
str(iris)

cor(x = iris[,1:4])
cor(x = iris[,c(1,2,3,4)])
cor(iris[,-5])

#preparar variables----

ir <- iris[,1:4]
ir_species <- iris[,5]
cor(ir)

#PCA----

pca <- prcomp(x = ir)
summary(pca) #PC contributions
plot(pca)
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
losdatosdelcolega <- data.frame(
  Sepal.Length = c(4.3, 5.4), 
  Sepal.Width = c(2.5, 3.0), 
  Petal.Length = c(1.8, 2.0), 
  Petal.Width = c(0.5, 1.3))

  
prediccion <- predict(pca2, 
        newdata = losdatosdelcolega)

names(pca2)
#pca2$center
#pca2$scale

biplot(pca2)
points(x = prediccion[,1],
       y = prediccion[,2],
       col = "red", pch = 19)

summary(ir)

#princomp







