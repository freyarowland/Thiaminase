setwd("PATH_TO_FOLDER_HERE")

require(phytools)

#Load function

#graft.subtree function that builds on Ape's bind.tree
graft.subtree <- function(Backbone, Subtree, Keynode)
  
{
  branching_times_subtree <- as.list(branching.times(Subtree))
  crown_age_subtree <- branching_times_subtree[[1]]
  grafted_tree <- bind.tree(Backbone, Subtree, where = Keynode, position = crown_age_subtree, interactive = FALSE)
  grafted_tree <- drop.tip(grafted_tree, Keynode)
  return(grafted_tree)
}


#Rescale

#read backbone tree - Fish Tree of Life backbone onto which the subtree (Cypriniformes) will be grafted
backbone <- read.tree("Backbone.tre")

#Identify two tips that encompas the mrca of the subtree of interest (79 & 90 in this case)
backbone$tip.label

#Get node number
keynode <- fastMRCA(backbone,backbone$tip.label[79],backbone$tip.label[90])

#Extract clade
subtree_backbone <- extract.clade(backbone,keynode)

#Get branching times of KeyClade and save first element (i.e., crown age of clade)
branching_times_subtree <- as.list(branching.times(subtree_backbone))
crown_age_subtree <- branching_times_subtree[[1]]

#read subtree - phylogram of Cypriniformes (Stout et al., 2016) plus outgroups
subtree_with_outgroups <- read.tree("Phylogram_Stout_et_al_Cypriniformes_outgroups.tre")

#Identify two tips that encompas the mrca of the subtree of interest (4 & 172 in this case)
subtree_with_outgroups$tip.label

#Get node number
keynode <- fastMRCA(subtree_with_outgroups,subtree_with_outgroups$tip.label[4],subtree_with_outgroups$tip.label[172])

#Extract clade
subtree <- extract.clade(subtree_with_outgroups,keynode)

#rescale using Ape's chrono's function - Molecular Dating by Penalised Likelihood and Maximum Likelihood (outperforms chronopl)
#Adjust lambda so the branch length patterns are similar 
subtree_chrono <- chronos(subtree,lambda = 0.05, model = "correlated", makeChronosCalib(subtree,age.min= crown_age_subtree, age.max= crown_age_subtree, node = "root"))
subtree_chrono$edge.length <- subtree_chrono$edge.length*crown_age_subtree

#check tree - compare two trees to ensure patterns in branch length are similar - otherwise adjust lambda value
plot(subtree_chrono)
plot(subtree)

write.tree(subtree_chrono, file= "Chronogram_Stout_et_al_Cypriniformes.tre")


#Graft

#read backbone
backbone <- read.tree("Backbone.tre")
#read ultrametric subtree
subtree <- read.tree("Chronogram_Stout_et_al_Cypriniformes.tre")


#identify all subtree species in backbone
backbone$tip.label

#Prune all BUT the first subtree species in backbone - need to actually type tip values (79-103 in this case)
#Empty string "c()" if backbone includes a single subtree species
pruned_backbone <- drop.tip(backbone, c(backbone$tip.label[79:103]))

#Check that the first (sole) species of subtree in backbone still has the same tip value ()
#Ignore if backbone includes a single subtree species
pruned_backbone$tip.label[[78]]

#Create grafted tree - type tip value of subtree species (78 in this case)
Grafted <- graft.subtree(pruned_backbone, subtree, 78)

write.tree(Grafted, file= "New_Backbone.tre")


