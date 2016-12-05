## R and Rstudio overview
# esto es un comentario
## Rstudio projects

## R as calculator
3 + 2
10 - 8
10000 * 2
log(34)
sqrt(12456667898)

log(34) - sqrt(12456667898)


## Assignment
objeto <- log(34)
miraiz <- sqrt(4567890)
miraiz
diff <- (objeto - miraiz)/2
rm(diff)


## Vectors
mivector <- c(1,2,3, 4, 5, 8)
nombres <- c("Patricia", "Manuel", "Agustin")
masnumeros <- c(5,3,7,13,21,2)
dif <- mivector + masnumeros
dif
sqrt(mivector)


## Indexing, logical subsetting, which
mivector
mivector[6]
mivector[c(3,4,6)]
mivector > 4
mivector[mivector > 4]
mivector*2

## NAs
nuevosvals <- c(45, 32, NA, 12, NA, NA)
nuevosvals*2


## Calling functions (sum, mean, log)
mivector
misuma <- sum(mivector)
mean(mivector)
log(mivector)
sum(nuevosvals, na.rm = FALSE)
sum(1, 2, 3, 4, 5)
sum(1:5)
1:5
1:50

## Getting help: ?, ??, google, stackoverflow, Rdocumentation, Rseek, cheatsheets
aleatorios <- runif(5, min = 0, max = 100)
round(aleatorios)
round(runif(5, min = 0, max = 100))

n <- 23
runif(n, min = 0, max = 50)






## Reading in data
tinyurl.com/treesdata

trees <- read.csv("datasets/trees.csv")

## Data frames
summary(trees)
str(trees)

## Packages

## dplyr
library(dplyr)
bigfemales <- filter(trees, height > 30,
                      sex == "female", dbh > 25)
bigfemales <- mutate(bigfemales, suma = height + dbh)

solovivos <- select(trees, -dead)
solosex <- select(trees, sex)

trees %>%
  group_by(plot) %>%
  summarise(max(dbh) + max(height))


##

miedad <- c(35, 23, 30, 28, 34, 31, 32, 33, 33, 30, 31)
mean(miedad)
hist(miedad)



## ggplot2

trees <- read.csv("datasets/trees.csv")
library(ggplot2)

ggplot(trees, aes(x = dbh, y = height)) +
  geom_point() + theme(plot.title = element_text(size = 30,
    hjust = 0.5), panel.background = element_rect(fill = "darkorange")) + labs(title = "mi titulo", x = "diametro")

myplot <- ggplot(trees, aes(x = dbh, y = height))
myplot <- myplot + geom_point()
myplot


# boxplot
ggplot(trees, aes(x=factor(plot),y = height))+
  geom_boxplot(aes(colour = sex))+
  labs(x="Study plot",y="Height (m)", title="Tree height per plot")


# violinplot
ggplot(trees, aes(x=factor(plot),y = height))+
  geom_violin(fill="orange")+
  labs(x="Study plot",y="Height (m)", title="Tree height per plot") +
  geom_point()



## multipanel

ggplot(trees, aes(x = dbh, y = height)) +
  geom_point() +
  facet_wrap(~sex)

library(ggplot2)
trees<-read.csv("datasets/trees.csv")
ggplot(trees, aes(x=height))+
  geom_histogram(aes(colour=sex))+
  facet_wrap(~plot) +
  labs(x = "Heights", y = "Number of trees", title = "Ole")



