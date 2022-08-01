## Betancur et al. order-level fish phylogeny 

# libraries
library(ape)
library(phytools)
library(dplyr)

# read in Betancur data
my.tree <- read.tree('data/Betancur2017.tre')
# keep <- read.csv("data/tipsFamily.csv")
keep <- read.csv("data/tipsOrder.csv")

# # get the tip labels for Betancur
# tips <- my.tree$tip.label
# # export just the tips for eliminating all one entry per family
# # write.csv(tips, "data/tips.csv")

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

## basic plot
# dotTree(pruned_tree,
#         fmode,
#         colors=setNames(c("white","red"),
#                         c("absent","present")),
#         legend = FALSE,
#         data.type = "discrete",
#         edge.width = 1,
#         ftype="i",
#         fsize=0.55,
#         mar = c(5.1, 4.1, 1.1, 1.1), cex = 0.55)
# axis(1)

# stochastic mapping ----

# rename tip labels
pruned_tree$tip.label<-sapply(strsplit(pruned_tree$tip.label,"_"),function(x) x[1])

smap.trees <- make.simmap(pruned_tree, fmode,
                          model = "ER", nsim = 500)
summary(smap.trees)


# save tree as PDF
pdf('figures/family_phylogeny.pdf', height = 15, width = 8)
cols <- setNames(c("black", "white"), c("present", "absent"))
plot(summary(smap.trees), direction = "rightwards", colors = cols, spread.labels = TRUE)
legend("topleft", c("present", "absent"), pch = 21, pt.bg = cols)
dev.off()

