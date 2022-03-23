# fishbase library?!

#library(rfishbase)
library(ape)
library(phytools)

# read in the data from https://fishtreeoflife.org/taxonomy/
my.tree <- read.tree('data/fishorder_skeletal.tre')

# pairwise taxa-taxa distance matrix
d <- cophenetic(my.tree)
d


# compute total tree height
h <- max(nodeHeights(my.tree))


# plot tree
plot(my.tree, edge.width = 1)
plot(my.tree, font = 1, cex = 0.45)

# generate sampling points
interval <- 30 # every 30 million years
dd <- seq(h,0, by =-interval)

# plot with millions of years
plotTree(my.tree, mar = c(5.1, 4.1, 1.1, 1.1), cex = 0.35)
axis(1)
xlab("Millions of years")
abline(v = dd, lty = "dotted", col = "grey")

# trim a phylogeny
# drop.tip
no_data <- c("Acanthuriformes", "Alepocephaliformes",
             "Argentiniformes", "Ateleopodiformes",
             "Beryciformes", "Ephippiformes",
             "Galaxiiformes", "Hiodontiformes",
             "Istiophoriformes", "Lampridiformes",
             "Lepidogalaxiiformes", "Myctophiformes",
             "Notacanthiformes", "Pempheriformes",
             "Percopsiformes", "Pholidichthyiformes",
             "Polymixiiformes", "Spariformes",
             "Stomiatiformes", "Stylephoriformes",
             "Uranoscopiformes", "Zeiformes")
plot(drop.tip(fish.tree, no_data))

# pruned tree
my.tree$tip.label

# taxa to keep
keep_taxa <- c("Polypteriformes", "Acipenseriformes",
                "Lepisosteiformes", "Amiiformes",
                "Elopiformes", "Anguilliformes",
                "Albuliformes", "Osteoglossiformes",
                "Clupeiformes", "Gonorynchiformes",
                "Cypriniformes", "Gymnotiformes",
                "Siluriformes", "Characiformes",
                "Salmoniformes", "Esociformes",
                "Osmeriformes", "Aulopiformes",
                "Gadiformes", "Holocentriformes",
                "Ophidiiformes", "Batrachoidiformes",
                "Labriformes", "Tetraodontiformes",
                "Lophiiformes", "Incertae_sedis_in_Eupercaria",
                "Perciformes", "Centrarchiformes",
                "Gobiiformes", "Kurtiformes",
                "Syngnathiformes", "Pleuronectiformes",
                "Incertae_sedis_in_Carangaria", 
                "Carangiformes", "Mugiliformes",
                "Incertae_sedis_in_Ovalentaria",
                "Cichliformes", "Scombriformes",
                "Atheriniformes", "Cyprinodontiformes",
                "Beloniformes", "Synbranchiformes")

# check to see which taxa are being removed
setdiff(my.tree$tip.label, keep_taxa)      

# remove
remove_taxa <- setdiff(my.tree$tip.label, keep_taxa)

# pruned tree
pruned_tree <- drop.tip(my.tree, remove_taxa)
pruned_tree$tip.label

# get tree statistics ----
summary(pruned_tree)

# how to plot with thiaminase presence/absence ----
fish.tree<-read.tree("data/fishorder_skeletal.tre")
fish.data<-read.csv("data/OrderPresAbsNA.csv",row.names=1)
fmode<-as.factor(setNames(fish.data[,1],rownames(fish.data)))
dotTree(drop.tip(fish.tree, no_data),
        fmode,
        colors=setNames(c("white","red"),
                                       c("absent","present")),
        legend = FALSE,
        edge.width = 1,
        ftype="i",
        fsize=0.55,
        mar = c(5.1, 4.1, 1.1, 1.1), cex = 0.55)
axis(1)

## anova ----
# won't work because response is yes/no
phylANOVA(tree = pruned_tree, x= pruned_tree$tip.label, y = fmode, nsim = 200, posthoc = TRUE, p.adj = "holm")

# make simulation of probability of thiaminase ----
# equal probability across the whole tree? LOL
# thia.trees<-make.simmap(drop.tip(fish.tree, no_data),
#                         fmode,
#                         nsim=200)
# obj<-densityMap(thia.trees,states=c("present","absent"),plot=FALSE)
# plot(obj,lwd=4,outline=TRUE,fsize=c(0.7,0.9),legend=50)

# stochastic mapping ----
smap.trees <- make.simmap(pruned_tree, fmode, 
                          model = "ER", nsim = 500)
summary(smap.trees)

pdf('figures/order_phylogeny.pdf', height = 8, width = 6)
cols <- setNames(c("black", "white"), c("present", "absent"))
plot(summary(smap.trees), colors = cols)
legend("topleft", c("present", "absent"),
       pch = 21, pt.bg = cols, pt.cex = 2)
dev.off()


# visualize MCC using function plotTree.wBars in the R package phytools
# this is if have continuous something

# phylogenetic ANOVA to test for difference in body size using
# phy.anova in geiger package
