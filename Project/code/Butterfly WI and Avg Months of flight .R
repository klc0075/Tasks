setwd('/Users/kaylacoffman/Desktop/Evolution/Tasks/Project/data')
# read in data
cleandata <- read.csv('cleanedupbutterflydata.csv')
Dat <- cleandata[,c(4,5)]
rownames(Dat) <- cleandata$Taxon

library(phytools)
tree <- read.tree("tree.tre")

# make data match
Drop <- setdiff(tree$tip.label, cleandata$Taxon)
tree <- drop.tip(tree, Drop)
Dat <- Dat[tree$tip.label,]

# regression
x <- Dat$WIn
names(x) <- rownames(Dat)
y <- Dat$FMo_Average
names(y) <- rownames(Dat)
matDat <- cbind(x, y)
Reg <- phyl.RMA(x, y, tree)
coef(Reg)
plot(Reg)
corObj <- phyl.vcv(matDat, vcv(tree), 1)
r.tree <-cov2cor(corObj$R)["x","y"]
#pearsons correlation ^ 0.06257945 (with phylogeny)
#regression plot and normal plot wing index (x axis) and average months of 
#flight (y axis) 


