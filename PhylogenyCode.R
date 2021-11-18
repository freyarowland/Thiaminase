# fishbase library?!

library(rfishbase)
library(ape)
library(phytools)

# read in the data from https://fishtreeoflife.org/taxonomy/
my.tree <- read.tree('fishorder_skeletal.tre')

# pairwise taxa-taxa distance matrix
d <- cophenetic(my.tree)
d

# plot tree
plot(my.tree, edge.width = 2)

# drop off orders I don't have?
# get names of tips
my.tree$tip.label

# trim a phylogeny
drop.tip

# infer the number of times a character evolved
make.simmmap

# visualize MCC using function plotTree.wBars in the R package phytools

# phylogenetic ANOVA to test for difference in body size using
# phy.anova in geiger package
