# Generalised Linear Models (GLMs)

#libraries
library(dplyr)
library(effects)
library(DHARMa)
library(visreg)



## Q: Survival of passengers on the Titanic ~ Class

#Read `titanic_long.csv` dataset.

u <- url("http://www.amstat.org/publications/jse/datasets/titanic.dat.txt")
titanic <- read.table(file = u)
titanic <- read.table("datasets/titanic.txt")
names(titanic) <- c("class", "age", "sex", "survived")
titanic$class <- factor(titanic$class, labels = c("crew", "first", "second", "third"))
titanic$age <- factor(titanic$age, labels = c("child", "adult"))
titanic$sex <- factor(titanic$sex, labels = c("female", "male"))
write.csv(titanic, file = "datasets/titanic_long.csv", row.names=FALSE, quote=FALSE)

titanic <- read.csv("datasets/titanic_long.csv")
head(titanic)

## Let's fit linear model:

m5 <- lm(survived ~ class, data = titanic)
layout(matrix(1:4, nrow=2))
plot(m5)
par(def.par)
## Weird residuals!

hist(resid(m5))

#How many passengers travelled in each class?
tapply(titanic$survived, titanic$class, length)

#How many survived?
tapply(titanic$survived, titanic$class, sum)

#What proportion survived in each class?
as.numeric(tapply(titanic$survived, titanic$class, mean))

#Alternativelly:
titanic %>%
  group_by(class, survived) %>%
  summarise(count = n())

summarise(group_by(titanic, class, survived), count = n())

## Or graphically...

plot(factor(survived) ~ class, data = titanic)

## Fitting GLMs in R: `glm`
tit.glm <- glm(survived ~ class, data=titanic, family=binomial)
summary(tit.glm)

#These estimates are in logit scale!
coef(tit.glm)

#We need to back-transform**: apply *inverse logit*
plogis(coef(tit.glm)[1])

#Looking at the data, the proportion of crew who survived is
sum(titanic$survived[titanic$class == "crew"]) / nrow(titanic[titanic$class == "crew", ])

## Q: Probability of survival for 1st class passengers?

plogis(coef(tit.glm)[1] + coef(tit.glm)[2])

#Needs to add intercept (baseline) to the parameter estimate. Again this value matches the data:
sum(titanic$survived[titanic$class == "first"]) /
  nrow(titanic[titanic$class == "first", ])

## Model interpretation using `effects` package

allEffects(tit.glm)

## Effects plot
plot(allEffects(tit.glm))


## Logistic regression: model checking

layout(matrix(1:4, nrow=2))
plot(tit.glm)
par(def.par)
#Not very useful.


## Binned residual plots for logistic regression

predvals <- predict(tit.glm, type="response")
arm::binnedplot(predvals, titanic$survived - predvals)

## Residual diagnostics with DHARMa

simulateResiduals(tit.glm, plot = TRUE)

#See https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html


# Q: Did men have higher survival than women?


## Plot first
plot(factor(survived) ~ sex, data = titanic)

## Fit model
tit.sex <- glm(survived ~ sex, data = titanic, family = binomial)
summary(tit.sex)

## Effects
allEffects(tit.sex)
plot(allEffects(tit.sex))


# Q: Did women have higher survival because they travelled more in first class?

## Let's look at the data
tapply(titanic$survived, list(titanic$class, titanic$sex), sum)

## Fit model with both factors (interactions)
tit.sex.class <- glm(survived ~ class * sex, data = titanic, family = binomial)
arm::display(tit.sex.class)
summary(tit.sex.class)
## Effects
allEffects(tit.sex.class)
plot(allEffects(tit.sex.class))
#So, women had higher probability of survival than men, even within the same class.

# GLMs for count data: Poisson regression

## Example dataset: Seedling counts in 0.5 m2 quadrats

seedl <- read.csv("datasets/seedlings.csv")
head(seedl)
summary(seedl)

## EDA
table(seedl$count)
hist(seedl$count)

## Q: Relationship between Nseedlings and light?

plot(seedl$light, seedl$count, las = 1, xlab = "Light (GSF)", ylab = "Seedlings")

seedl.lm <- lm(count ~ light, data = seedl)
summary(seedl.lm)
plot(seedl.lm)

## Let's fit model (Poisson regression)

seedl.glm <- glm(count ~ light, data = seedl, family = poisson)
summary(seedl.glm)

## Interpreting Poisson regression output {.build}

#Parameter estimates (log scale):
coef(seedl.glm)

#**We need to back-transform**: apply the inverse of the logarithm
exp(coef(seedl.glm))

## So what's the relationship between Nseedlings and light?

allEffects(seedl.glm)
plot(allEffects(seedl.glm))

## Using visreg
visreg(seedl.glm, scale = "response")


## Poisson regression: model checking
layout(matrix(1:4, nrow=2))
plot(seedl.glm)
par(def.par)

## Is there pattern of residuals along predictor?

plot(seedl$light, resid(seedl.glm))

## Residuals diagnostics with DHARMa

simulateResiduals(seedl.glm, plot = TRUE)

# Poisson regression: Overdispersion

## Always check overdispersion with count data

simres <- simulateResiduals(seedl.glm, refit = TRUE)
testOverdispersion(simres)

## Accounting for overdispersion in count data
#Use family `quasipoisson`
seedl.overdisp <- glm(count ~ light, data = seedl, family = quasipoisson)
summary(seedl.overdisp)

## Mean estimates do not change after accounting for overdispersion
allEffects(seedl.overdisp)
allEffects(seedl.glm)
## But standard errors may change

plot(allEffects(seedl.overdisp))
plot(allEffects(seedl.glm))


