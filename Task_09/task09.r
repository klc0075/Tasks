setwd('/Users/kaylacoffman/Desktop/Evolution/Tasks/Task_08')
library(phytools)
library(ape)
tree <- force.ultrametric(read.tree("anolis.tre"))
plot(tree, type="fan")
?plot.phylo
Ntip(AnolisTree)
tree$tip.label
tree$edge.length
#Question 1: There are 82 tips and yes there are branch lengths present, 162 
#branches 
setwd('/Users/kaylacoffman/Desktop/Evolution/Tasks/Task_09')
data <- read.csv("svl.csv", stringsAsFactors=F, row.names=1)
dim(data)
#Question 2: Data shows different lizard species 
#and their corresponding svls. The dimensions are 82:1  
setwd('/Users/kaylacoffman/Desktop/Evolution/Tasks/Task_08')
svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
?fastAnc
#Question 3: estimated value stored in the nodes, CI elements is a logical value indicating whether or not 
#to compute 95-percent confidence intervals, CI95 is the 95 % confidence interval. 
#
#Question 4- Assumes state compound for root node=MLE of root node and there is a 
#95 confidence interval 
par(mar=c(0.1, 0.1, 0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label = F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree,svl,plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7,0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger","Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes","Anolis_angusticeps", "Anolis_angusticeps"))
fossilNodes <- c()
nodeN <- c()
head(fossilData)
#Question 5
  for(i in 1:6) {
Node<- fastMRCA(tree,fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes[i] <- fossilData[i,"svl"]
nodeN[i] <- Node 
}
i
names(fossilNodes) <- nodeN 
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
Ancestors_withoutFossils <- fastAnc(tree,svl,CI=TRUE, var=TRUE)
plot(Ancestors_withFossils$ace, Ancestors_withoutFossils$ace, xlab='with fossils', ylab='without fossils')
#Question 7- The fossils increased the estimated ancestral size
head(Ancestors)
plot(Ancestors_withFossils)
#Questions 8-10 
install.packages("geiger")
library(geiger)
deltaFit <- fitContinuous(tree, data, model = "delta") #AIC= -6.106526
lambdaFit <- fitContinuous(tree, data, model="lambda") #AIC= -4.512027
kappaFit <- fitContinuous(tree, data, model="kappa") #AIC= -4.512027
OuFit <- fitContinuous(tree, data, model="OU") #AIC= -4.512027
ebFit <- fitContinuous(tree, data, model="EB") #AIC= -7.235124
brownFit <- fitContinuous(tree, data) #AIC= -6.512027 
meantrendfit <- fitContinuous(tree, data, model="mean_trend") #AIC= -4.512027
whiteFit <- fitContinuous(tree, data, model="white") #AIC= 91.391102
ratetrendfit <- fitContinuous(tree, data, model="rate_trend")#AIC = -6.981365
#The Early burst (ebFit) model fits the data the best because it has the lowest AIC. 
?fitContinuous
?fastAnc
#fitContinuous compares continuous comparative data while FastAnc performs 
#fast estimation of ML ancestral states but both assume continuous traits 
#and the same confidence interval.
