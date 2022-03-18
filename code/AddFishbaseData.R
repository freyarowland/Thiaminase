## Add in family to master spreadsheet

# libraries
library(rfishbase)
library(dplyr)

# load traits data
dat2 <- read.csv("data/AllData.csv", header = TRUE)

# make a vector of fish names
fish <- dat2[,2]

## exploring where data are stored
ecofish <- ecology(fish)
