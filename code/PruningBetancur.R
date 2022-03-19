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
fish.data$Name<-sapply(strsplit(fish.data$Name,"_"),function(x) x[1])

fish.data <- keep2[, c(2, 4)]
row.names(fish.data) <- fish.data$Name
fmode <- as.factor(setNames(fish.data[, 2], rownames(fish.data)))

## basic plot
dotTree(pruned_tree,
        fmode,
        colors=setNames(c("white","red"),
                        c("absent","present")),
        legend = FALSE,
        data.type = "discrete",
        edge.width = 1,
        ftype="i",
        fsize=0.55,
        mar = c(5.1, 4.1, 1.1, 1.1), cex = 0.55)
axis(1)

# stochastic mapping ----

# rename tip labels
pruned_tree$tip.label<-sapply(strsplit(pruned_tree$tip.label,"_"),function(x) x[1])

smap.trees <- make.simmap(pruned_tree, fmode,
                          model = "ER", nsim = 500)
summary(smap.trees)

png('figures/family_phylogeny.png', height = 1200, width = 600)
cols <- setNames(c("red", "black"), c("present", "absent"))
plot(summary(smap.trees), spread.labels = TRUE, size = 1, lwd = 1.5, cex = 0.8, pt.cex = 0.3)
legend("topleft", c("present", "absent"), pch = 21, pt.bg = cols, pt.cex = 1)
dev.off()



#ggsave(filename = "family_phylogeny.png", dpi = 300, height = 10, width = 4)

# make simulation of probability of thiaminase ----
## equal probability across the whole tree? LOL
thia.trees<-make.simmap(pruned_tree,
                        fmode,
                        nsim=200)
obj<-densityMap(thia.trees,states=c("present","absent"),plot=FALSE)
plot(cols = c("black", "white"), obj,lwd=2,outline=TRUE,fsize=c(0.7,0.9),legend=50)
