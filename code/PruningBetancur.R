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

# read in Betancur data
my.tree <- read.tree('data/Betancur2017.tre')
keep <- read.csv("data/tips.csv")

# # get the tip labels for Betancur
# tips <- my.tree$tip.label
# # export just the tips for eliminating all one entry per family
# # write.csv(tips, "data/tips.csv")

# eliminate entries that are not in database
# 108 families overlap between Betancur and my data
keep2 <- filter(keep, InData == 1)
names <- keep2[,2]

# get tip labels
tips <- my.tree$tip.label

# # export just the tips for eliminating all one entry per family
# write.csv(tips, "data/tips.csv")
