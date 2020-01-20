## R and Rstudio overview

# esto es un comentario

## Rstudio projects

## R as calculator
# (arithmetic operations, log, sqrt...)



## Assignment


## Vectors (numeric, characters)
# sum, length...


## Indexing, logical subsetting (and, or), which



## NAs


## Calling functions (sum, mean, log)


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




## dplyr (filter, select, mutate, arrange, distinct, slice, group_by, summarise...)

library(dplyr)

## Seleccionar solo arboles vivos


# solo arboles con dbh > 10 cm


#trees <- mutate(trees, height.cm = height*100)


