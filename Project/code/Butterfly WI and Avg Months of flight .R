setwd('/Users/kaylacoffman/Desktop/Evolution/Tasks/Project/data')
# read in data
cleandata <- read.csv('cleanedupbutterflydata.csv')
Dat <- cleandata[,c(4,5)]
rownames(Dat) <- cleandata$Taxon

Spear <- cor(Dat[,1], Dat[,2], method="spear")
#correlation = -0.02791059 
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
pdf("Phylogenic Regression.pdf")
par(mar=c(4,4,1,1), las=1, tck=-0.01)
plot(x, y, xlab="Wing Index", ylab="Average Months of Flight", pch=21, bg="gray70")
abline(coef(Reg)[1], coef(Reg)[2], col='red')
dev.off() 

corObj <- phyl.vcv(matDat, vcv(tree), 1)
r.tree <-cov2cor(corObj$R)["x","y"]
#pearsons correlation ^ 0.06257945 (with phylogeny)
#regression plot and normal plot wing index (x axis) and average months of 
#flight (y axis) 
?phyl.RMA
cor(matDat)

setwd('/Users/kaylacoffman/Desktop/Evolution/Tasks/Project/code')
pdf ("X-Y Scatter Plot.pdf ")
plot(x,y, xlab="Wing Index", ylab="Average Months of Flight")
linek <- lm(y~x)
abline(linek)
dev.off()

RStudio.Version()
