## R and Rstudio overview
# esto es un comentario
### hola yo me llamo Paco ##
## Rstudio projects

## R as calculator
# (arithmetic operations, log, sqrt...)
4 + 8
50 - 12
20 * 3
40 / 4
log(4)
sqrt(50)


## Assignment
x <- 3
x
log.3 <- log(3)
log.x <- log(x)
y <- c(1, 2, 5, 12)
log(y)
log.y <- log(y)

## Vectors (numeric, characters)
# sum, length...
nombres <- c("Maria", "Pedro", "Pablo", "Ali")
sum(nombres)
sum(4, 3, 1)
sum(log.y)
sum()
length(nombres)



## Indexing, logical subsetting (and, or), which
nombres
nombres[1]
nombres[3]
nombres[c(3,4)]
nombres[c(1, 2)]
nombres[-2]

x <- c(2, 5, 1)
x[2]
1:5
4:10

x[2:3]

x > 4

x[x < 4]

sum(x[x < 4])

zz <- 1:200


## NAs
edades <- c(28, 31, 24, 21, 25, NA, 32, NA)
edades > 30
edades == 20
edades == 31
edades == 31 | edades == 25

## Calling functions (sum, mean, log)
sum(edades)
edades.conocidas <- edades[!is.na(edades)]
is.na(edades)
sum(edades.conocidas)
sum(edades, na.rm = TRUE)
mean(edades, na.rm = TRUE)

## Getting help: F1, ?, ??, google, stackoverflow, Rdocumentation, Rseek, cheatsheets




## Packages

# rio, readr, dplyr


## Reading in data
tinyurl.com/treesdata

# rio
library(rio)
trees <- import("datasets/trees.csv")

# readr
# library(readr)
# trees <- read_csv("datasets/trees.csv")


## Data frames (summary, str)
summary(trees)
str(trees)



## dplyr (filter, select, mutate, arrange, distinct, slice, group_by, summarise...)

library(dplyr)

## Seleccionar solo arboles vivos
vivos <- filter(trees, dead == 0)
muertos <- filter(trees, dead == 1)
muertos <- filter(trees, dead > 0)
vivos <- filter(trees, dead < 1)

# solo arboles con dbh > 10 cm
grandes <- filter(trees, dbh > 10)

vivos.grandes <- filter(trees, dbh > 10, dead == 0)

tree.medidas <- select(trees, dbh, height)

trees2 <- select(trees, -sex)

#trees <- mutate(trees, height.cm = height*100)
trees <- mutate(trees,
                vivo = ifelse(dead == 0, "si", "no"),
                height.cm = height*100)

mean.height.plot <- trees %>%
  group_by(plot) %>%
  summarise(mean(height))

trees %>%
  group_by(plot) %>%
  summarise(sum(dead))


