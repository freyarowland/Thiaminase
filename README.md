# Thiaminase

## Authors
- Freya Rowland
- Rachel Munds
- Cathy Richter
- Don Tillitt
- David Walters

A repository for exploring the evolutionary and ecological factors driving thiaminase production in fishes

## Data files

- [AllData.csv](<data/AllData.csv>) includes all ecological fish data
- [fishorder_skeletal.tre](<data/fishorder_skeletal.tre>) is the evolutionary phylogeny from [Rabosky et al. 2018](<https://www.nature.com/articles/s41586-018-0273-1>) downloaded from <https://fishtreeoflife.org/>
- [BecanturTree.tre](<data/BecanturTree.tre>) is the tree I want to use for the phylogeny from [Betancur et al. 2017](<https://bmcecolevol.biomedcentral.com/articles/10.1186/s12862-017-0958-3>) who also has a GitHub repository https://github.com/projectdigest/betancur_r-fish-tree/
- [OrderPresAbs.csv](<data/OrderPresAbs.csv>) is a csv file with presence of absence of thiaminase within each fish order in the Rabosky phylogeny
- [OrderPresAbsNA.csv](<OrderPresAbsNA.csv>) includes NA for orders not present in our dataset but are in the Rabosky phylogeny
- [data/Orders.csv](<data/Orders.csv>) a clean up file for matching orders from <https://fishbase.de> to orders in Rabosky. Should delete.

## R Scripts

- [Models.R](<code/Models.R>) includes Bayesian models for predicting thiaminase based on ecological fish characteristics from https://www.fishbase.de/
- [PhylogenyCode.R](<code/PhylogenyCode.R>) order-level phylogeny based on https://fishtreeoflife.org/taxonomy/ and analysis to see if node distance can predict thiaminase presence/absence
- [code/PruningBetancur.R](<code/PruningBetancur.R>) is the family-level code for pruning the Betancur et al. data to family-level and plotting the character simulations at each node.

## Figures

Figures for publication.

### Family-level phylogeny. No evolutionary relationship between probability of thiaminase expression within a family.
![Family_phylogeny](figures/family_phylogeny_v2.png)

### Order-level phylogeny has same pattern. No evolutionary relationship
![order_phylogeny](figures/order_phylogeny.png)

### Multiple regression
![multreg](figures/multreg_plot.png)

### all significant plots
![Allsig](figures/allsigplots.png)
