iris <- read.csv("datasets/iris.csv")

library(dplyr)
setosa <- filter(iris, Species == "setosa")

## relacion petal length ~ width
library(ggplot2)
ggplot(setosa, aes(x = Petal.Width, y = Petal.Length)) +
  geom_point()


## modelo lineal
m1 <- lm(Petal.Length ~ Petal.Width, data = setosa)
summary(m1)
library(visreg)
visreg(m1)

# asunciones
hist(residuals(m1))
plot(m1)

# si width = 0.39
# y = a + bx
# x= 0.39
1.327 + 0.546*0.39


## obs vs pred
obs <- setosa[, "Petal.Length"]
pred <- fitted(m1)
plot(pred ~ obs)
abline(a = 0, b = 1)




####
iris <- read.csv("datasets/iris.csv")

m2 <- lm(Petal.Length ~ Species, data = iris)
summary(m2)

library(effects)
allEffects(m2)
summary(allEffects(m2, confidence.level = 0.99))
plot(allEffects(m2))

library(visreg)
visreg(m2)

## residuals
plot(m2)


### predictores continuos y categoricos a la vez
m3 <- lm(Petal.Length ~ Species + Petal.Width,
         data = iris)
summary(m3)
allEffects(m3)
plot(allEffects(m3))
plot(m3)

# obs vs pred
obs <- iris[, "Petal.Length"]
plot(fitted(m3) ~ obs)
abline(a = 0, b = 1)


####

