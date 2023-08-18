# Evolutionary and ecological correlates of thiaminase in fishes

## Authors
- Freya Rowland
- Cathy Richter
- Don Tillitt
- David Walters

A repository for exploring the evolutionary and ecological factors driving thiaminase production in fishes

## data
### Includes all data files (as .csv or .tre) used in the study

- [AllData.csv](<data/AllData.csv>) includes all ecological fish data from https://www.fishbase.de/ paired with thiaminase presence/absence
- [fishorder_skeletal.tre](<data/fishorder_skeletal.tre>) is the evolutionary phylogeny from [Rabosky et al. 2018](<https://www.nature.com/articles/s41586-018-0273-1>) downloaded from <https://fishtreeoflife.org/>
- [BecanturTree.tre](<data/BecanturTree.tre>) family-level phylogeny from [Betancur et al. 2017](<https://bmcecolevol.biomedcentral.com/articles/10.1186/s12862-017-0958-3>) who also has a GitHub repository https://github.com/projectdigest/betancur_r-fish-tree/
- [OrderPresAbs.csv](<data/OrderPresAbs.csv>) is a csv file with presence of absence of thiaminase within each fish order in the Rabosky phylogeny
- [OrderPresAbsNA.csv](<OrderPresAbsNA.csv>) includes NA for orders not present in our dataset but are in the Rabosky phylogeny

## code
### R scripts fpr analysis and figures

- [Models.R](<code/Models.R>) includes Bayesian models for predicting thiaminase based on ecological fish characteristics from https://www.fishbase.de/
- [PhylogenyCode.R](<code/PhylogenyCode.R>) order-level phylogeny based on https://fishtreeoflife.org/taxonomy/ and analysis to see if node distance can predict thiaminase presence/absence
- [code/PruningBetancur.R](<code/PruningBetancur.R>) is the family-level code for pruning the Betancur et al. data to family-level and plotting the character simulations at each node

## Figures
### All figures generated for the study.
