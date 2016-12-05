trees <- read.csv("datos/trees.csv")
library(ggplot2)
ggplot(trees, aes(x= dbh, y=height)) + geom_point()
m1 <- lm(height ~ dbh, data = trees)
summary(m1)
hist(residuals(m1))
nuevosdbh <- c(10, 20, 30, 40, 50)
predict(m1, se.fit = TRUE,
        newdata = data.frame(dbh = nuevosdbh))
plot(m1)
library(effects)
plot(allEffects(m1))
library(visreg)
visreg(m1)

ggplot(trees, aes(x= dbh, y=height)) +
  geom_point() +
  geom_smooth()



################

## altura diferente entre parcelas?
ggplot(trees, aes(x = factor(plot), y = height)) + geom_boxplot()
m2<- lm(height ~ factor(plot), data = trees)
summary(m2)
allEffects(m2)
factor(trees[,"plot"])
library(dplyr)
trees<- mutate(trees, parcela = factor(plot))
m2<- lm(height ~ parcela, data = trees)
allEffects(m2)
summary(allEffects(m2))
plot(allEffects(m2))



### Predecir altura ~ parcela + dbh
ggplot(trees, aes(x=dbh, y=height)) +
  geom_point(aes(colour=parcela))

m3 <- lm(height~parcela + dbh, data = trees)
summary(m3)
allEffects(m3)
plot(allEffects(m3))

m3 <- lm(height~parcela*dbh, data = trees)
summary(m3)
allEffects(m3)
plot(allEffects(m3))
visreg(m3)
