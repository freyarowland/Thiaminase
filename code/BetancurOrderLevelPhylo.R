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

# rename tip labels to just family
pruned_tree$tip.label<-sapply(strsplit(pruned_tree$tip.label,"_"),function(x) x[1])

# get tree statistics ----
summary(pruned_tree)


## prep data for plotting with thiaminase presence/absence ----
# rename fish names by keeping only family
# rename tip labels
fish.data <- keep2
fish.data$Name<-sapply(strsplit(fish.data$Name,"_"),function(x) x[1])

# fish.data <- keep2[, c(2, 4)]
row.names(fish.data) <- fish.data$Order
fmode <- as.factor(setNames(fish.data[, 4], rownames(fish.data)))


# stochastic mapping ----



# # rename them to order level
# new_tiplabels <- c(
#   "Rajiformes",
#   "Chimaeriformes",
#   "Coelacanthiformes",
#   "Ceratodontiformes",
#   "Polypteriformes",
#   "Acipenseriformes",
#   "Lepisosteiformes",
#   "Elopiformes",
#   "Albuliformes",
#   "Anguilliformes",
#   "Osteoglossiformes",
#   "Clupeiformes",
#   "Gonorynchiformes",
#   "Cypriniformes",
#   "Gymnotiformes",
#   "Siluriformes",
#   "Characiformes",
#   "Esociformes",
#   "Salmoniformes",
#   "Osmeriformes",
#   "Aulopiformes",
#   "Gadiformes",
#   "Holocentriformes",
#   "Cichliformes",
#   "Beloniformes",
#   "Cyprinodontiformes",
#   "Atheriniformes",
#   "Mugiliformes",
#   "Gobiesociformes",
#   "Blenniformes",
#   "Ovalentaria",
#   "Pleuronectiformes",
#   "Carangiformes",
#   "Carangaria",
#   "Synbranchiformes",
#   "Anabantiformes",
#   "Centrarchiformes",
#   "Perciformes",
#   "Lophiiformes",
#   "Tetraodontiformes",
#   "Acanthuriformes",
#   "Eupercaria",
#   "Kurtiformes",
#   "Gobiiformes",
#   "Syngnathiformes",
#   "Scombriformes",
#   "Squaliformes",
#   "Batrachoidiformes",
#   "Ophidiiformes"
# ) 
# pruned_tree$tip.label <- new_tiplabels

smap.trees <- make.simmap(pruned_tree, fmode,
                          model = "ER", nsim = 100)
summary(smap.trees)


## clunky way to rename tip labels
# smap.trees$tip.label[smap.trees$tip.label=="Bythitidae"] <- "Ophidiiformes" # didn't work
## Function for renaming tips
# rename.tips.phylo <- function(tree, names) {
#   tree$tip.label <- new_names[match(tree$tip.label,old_names)]
#   return(tree)
# }
# 
# old_names <- c("Bythitidae", "Batrachoididae", "Stromateidae", "Scombridae")
# new_names <- c("Ophidiiformes", "Batrachoidiformes", "Squaliformes", "Scombriformes")
# 
# smap.trees_renamed <- lapply(smap.trees, rename.tips.phylo,
#                                names = new_names)

# orig_tiplabels <-  c(
#   "Adrianichthyidae",
#   "Albulidae",
#   "Ambassidae",
#   "Anabantidae",
#   "Anguillidae",
#   "Apogonidae",
#   "Atherinopsidae",
#   "Bagridae",
#   "Batrachoididae",
#   "Blenniidae",
#   "Bythitidae",
#   "Callorhinchidae",
#   "Carangidae",
#   "Catostomidae",
#   "Centrarchidae",
#   "Chaetodontidae",
#   "Chanidae",
#   "Characidae",
#   "Cichlidae",
#   "Clupeidae",
#   "Cynoglossidae",
#   "Cyprinodontidae",
#   "Eleotridae",
#   "Fistulariidae",
#   "Gobiesocidae",
#   "Gymnotidae",
#   "Holocentridae",
#   "Latimeriidae",
#   "Lepisosteidae",
#   "Lophiidae",
#   "Lotidae",
#   "Mastacembelidae",
#   "Megalopidae",
#   "Mormyridae",
#   "Moronidae",
#   "Mugilidae",
#   "Osmeridae",
#   "Percidae",
#   "Polyodontidae",
#   "Polypteridae",
#   "Protopteridae",
#   "Rajidae",
#   "Salmonidae",
#   "Scombridae",
#   "Stromateidae",
#   "Synodontidae",
#   "Tetraodontidae",
#   "Toxotidae",
#   "Umbridae"
# )
# 
# 
# new_tiplabels <- c(
#   "Beloniformes",
#   "Albuliformes",
#   "Ovalentaria",
#   "Anabantiformes",
#   "Anguilliformes",
#   "Kurtiformes",
#   "Atheriniformes",
#   "Siluriformes",
#   "Batrachoidiformes",
#   "Blenniiformes",
#   "Ophidiiformes",
#   "Chimaeriformes",
#   "Carangiformes",
#   "Cypriniformes",
#   "Centrarchiformes",
#   "Acanthuriformes",
#   "Gonorynchiformes",
#   "Characiformes",
#   "Cichliformes",
#   "Clupeiformes",
#   "Pleuronectiformes",
#   "Cyprinodontiformes",
#   "Gobiiformes",
#   "Syngnathiformes",
#   "Gobiesociformes",
#   "Gymnotiformes",
#   "Holocentriformes",
#   "Coelacanthiformes",
#   "Lepisosteiformes",
#   "Lophiiformes",
#   "Gadiformes",
#   "Synbranchiformes",
#   "Elopiformes",
#   "Osteoglossiformes",
#   "Eupercaria",
#   "Mugiliformes",
#   "Osmeriformes",
#   "Perciformes",
#   "Acipenseriformes",
#   "Polypteriformes",
#   "Ceratodontiformes",
#   "Rajiformes",
#   "Salmoniformes",
#   "Scombriformes",
#   "Squaliformes",
#   "Aulopiformes",
#   "Tetraodontiformes",
#   "Carangaria",
#   "Esociformes"
# )
# 
# d <- as.data.frame(cbind(label=orig_tiplabels, nlabel=new_tiplabels))
# 
# pruned_tree$tip.label<-d[[2]][match(pruned_tree$tip.label, d[[1]])]
# pruned_tree$tip.label<-sapply(pruned_tree$tip.label, function(x) parse(text=x))





pdf('figures/order_phylogeny.pdf', height = 8, width = 6)
cols <- setNames(c("black", "white"), c("present", "absent"))
plot(summary(smap.trees), direction = "rightwards", colors = cols, tip.labels = new_tiplabels, spread.labels = TRUE)
legend("topleft", c("present", "absent"), pch = 21, pt.bg = cols)
dev.off()


