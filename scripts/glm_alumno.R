# Generalised Linear Models (GLMs)

#libraries
library(dplyr)
library(effects)
library(DHARMa)
library(visreg)



## Q: Survival of passengers on the Titanic ~ Class

#Read `titanic_long.csv` dataset.

titanic <- read.csv(file = "datasets/titanic_long.csv")
head(titanic)
str(titanic)

## Let's fit linear model:
m <- lm(formula = survived ~ class,
        data = titanic)
summary(m)
hist(residuals(m))
plot(m)

#How many passengers travelled in each class?

#How many survived?

#What proportion survived in each class?

#Alternativelly:
titanic %>%
  group_by(class) %>%
  summarise(nombre_var = mean(survived))

summarise(group_by(titanic, class, survived), count = n())

## Or graphically...

plot(factor(survived) ~ class, data = titanic)

## Fitting GLMs in R: `glm`
m.glm <- glm(survived ~ class, titanic,
             family = "binomial")
summary(m.glm)


#These estimates are in logit scale! (coef)
coef(m.glm)
#We need to back-transform**: apply *inverse logit* (plogis)
plogis(coef(m.glm))

#Looking at the data, the proportion of crew who survived is
sum(titanic$survived[titanic$class == "crew"]) / nrow(titanic[titanic$class == "crew", ])

## Q: Probability of survival for 1st class passengers?

plogis(coef(tit.glm)[1] + coef(tit.glm)[2])

#Needs to add intercept (baseline) to the parameter estimate. Again this value matches the data:
sum(titanic$survived[titanic$class == "first"]) /
  nrow(titanic[titanic$class == "first", ])

## Model interpretation using `effects` package (allEffects)
allEffects(mod = m.glm)

## Effects plot


## Logistic regression: model checking

layout(matrix(1:4, nrow=2))
plot(m.glm)
par(def.par)
#Not very useful.

## Residual diagnostics with DHARMa (simulateResiduals)


#See https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html


# Q: Did men have higher survival than women?

## Fit model
m <- glm(survived ~ sex, titanic, family = "binomial")
plot(simulateResiduals(m))
summary(m)
plogis(coef(m)[1])
plogis(coef(m)[1]+coef(m)[2])
plogis(coef(m))
allEffects(m)


## Effects


# Q: Did women have higher survival because they travelled more in first class?

## Let's look at the data
tapply(titanic$survived, list(titanic$class, titanic$sex), sum)

## Fit model with both factors (interactions)

## Effects







# GLMs for count data: Poisson regression----

## Example dataset: Seedling counts in 0.5 m2 quadrats (seedlings.csv)

seedlings <- read.csv("datasets/seedlings.csv")
head(seedlings)
## Q: Relationship between Nseedlings and light?

#plot
plot(count ~ light, data = seedlings)

#lm

## Let's fit model (Poisson regression)
m.poi <- glm(count ~ light, data = seedlings,
             family = "quasipoisson")
plot(m.poi)
r <- simulateResiduals(m.poi, n = 1000)
plot(r)
summary(m.poi)

## Interpreting Poisson regression output {.build}

#Parameter estimates (log scale) (coef):
coef(m.poi)
#**We need to back-transform**: apply the inverse of the logarithm (exp)
exp(coef(m.poi))

## So what's the relationship between Nseedlings and light? (allEffects)
allEffects(m.poi)
plot(allEffects(m.poi))
## Using visreg
visreg(m.poi, scale = "response")


## Poisson regression: model checking
layout(matrix(1:4, nrow=2))
plot(seedl.glm)
par(def.par)

## Is there pattern of residuals along predictor?

plot(seedl$light, resid(seedl.glm))

## Residuals diagnostics with DHARMa (simulateResiduals)


# Poisson regression: Overdispersion

## Always check overdispersion with count data

simres <- simulateResiduals(m.poi, refit = TRUE)
testOverdispersion(simres)

## Accounting for overdispersion in count data
#Use family `quasipoisson`

## Mean estimates do not change after accounting for overdispersion (allEffects)

## But standard errors may change (plot(allEffects))



