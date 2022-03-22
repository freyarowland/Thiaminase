## Trimming the Betancur tree to order only

## Helpful advice from Ricardo Betancur
# You’d need to prune the tree yourself to keep only one sp. per order. 
# There are plenty of packages in R for that (e.g., drop.tip in APE) and also many tutuorials.
# Just export the tip labels from the tree file (e.g., convert the tree to nexus format,
# that way you’ll see one tip species per line) to create two lists using a text editor:
# one list will have all the tip labels from my tree, the second list will have all 
# the tips you want to drop to retain only one per order. 
# 
# For the second list, the easiest is to choose the spp. you want to retain, add 
# them to the complete list, and remove all duplicate lines in BBEdit or some other text editor.

# libraries
library(ape)
library(phytools)
library(dplyr)

# read in Betancur data
my.tree <- read.tree('data/Betancur2017.tre')
keep <- read.csv("data/tipsOrder.csv")


# eliminate entries that are not in database
# 108 families overlap between Betancur and my data
keep2 <- filter(keep, InData == 1)
names <- keep2[,2]

## check to see which taxa are being removed
setdiff(my.tree$tip.label, names)   

## remove the taxa not included in csv file
remove_taxa <- setdiff(my.tree$tip.label, names)

# pruned tree
pruned_tree <- drop.tip(my.tree, remove_taxa)
pruned_tree$tip.label

# get tree statistics ----
summary(pruned_tree)


## prep data for plotting with thiaminase presence/absence ----
# rename fish names by keeping only family
# rename tip labels
fish.data <- keep2
fish.data$Name<-sapply(strsplit(fish.data$Name,"_"),function(x) x[1])

# fish.data <- keep2[, c(2, 4)]
row.names(fish.data) <- fish.data$Name
fmode <- as.factor(setNames(fish.data[, 4], rownames(fish.data)))


# stochastic mapping ----

# rename tip labels
pruned_tree$tip.label<-sapply(strsplit(pruned_tree$tip.label,"_"),function(x) x[1])

smap.trees <- make.simmap(pruned_tree, fmode,
                          model = "ER", nsim = 500)
summary(smap.trees)



pdf('figures/family_phylogeny.pdf', height = 15, width = 8)
cols <- setNames(c("black", "white"), c("present", "absent"))
plot(summary(smap.trees), direction = "rightwards", colors = cols, spread.labels = TRUE)
legend("topleft", c("present", "absent"), pch = 21, pt.bg = cols)
dev.off()


