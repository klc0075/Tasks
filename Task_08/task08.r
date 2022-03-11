library (phytools) 
library(ape) 
text.string <- "(((((((cow, pig), whale), (bat,(lemur,human))), (robin, iguana)), coelacanth), (gold_fish, trout)),shark);"
vert.tree <- read.tree (text=text.string)
plot (vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
#Question 1: A shark is more closely related to a goldfish than a human
vert.tree
#Question 2: No 
str(vert.tree)
tree <- read.tree(text="(((A,B), (C,D)), E);")
plotTree (tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge 
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
setwd('/Users/kaylacoffman/Desktop/Evolution/Tasks/Task_08')
AnolisTree <- force.ultrametric(read.tree("anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0,50), xlim=c(0,6))
tipEdges <- which(AnolisTree$edge[,2] <=Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label 
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
#Question 3 
plot(AnolisTree, cex=0.25, show.tip.label=FALSE)
#Question 4 
plot(AnolisTree, type="fan", cex=0.25)
#Question 5 
plot(AnolisTree, cex=0.25, tip.color='red')


?which
which(AnolisTree == 'minedge.length')
AnolisTree[which(AnolisTreeedge.length==shortest)]
AnolisTree[which.min('edge.length','numeric')]
?numeric
which.min('edge.length')
AnolisTree[which("edge.length")]
which(AnolisTree$edge.length == 'shortest')
?which.min
#Question 6-8 
which.min(AnolisTree$edge.length)
NewAnolisTree <- drop.tip(AnolisTree, 82)
plot(NewAnolisTree, cex=0.25)
ltt(AnolisTree)
abline(0,1,lwd=2, col='red', lty=2)
#Question 9 
#The line continues to go up and never goes down. It never
#goes down because diversity is increasing and over time. 
# There seems to be a period of stabilization towards the end. 
# The slope will not always be the same, if there is an extinction 
#event the slope could stabilize but it will never be negative. 
?fit.bd
#Question 10 
fit.bd(AnolisTree, rho=0.2)
#rate new species form = 0.8031
#rate new species disappear = 0 

