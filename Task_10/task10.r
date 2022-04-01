library (phytools)
#Question 1-3 
trees <- list()
births <- vector()
Fractions <- vector()
netdr <- vector() 
speciationrate <- vector()
Avgbranchlength <- vector()

    for (i in 1:100) { 
       births [i]<- runif(1, 0, 1)
       Fractions [i] <- runif(1,0,1)
      trees [[i]]<- pbtree(n=100, b= births [i], d=Fractions [i] * births[i])
      netdr[[i]] <- (births [i] - Fractions[i]* births[i]) 
      speciationrate[i] <- births[i]
      Avgbranchlength[[i]] <- mean(trees[[i]]$edge.length)
      
  }

?pbtree
#Question 4- As net diversification increases the log of total number 
#of tips increases
netdr <- (births [i] - Fractions[i]* births[i]) 
totaltips <- log(sapply(trees, Ntip))
pdf("Question 4 net diversification vs log of total number of tips.pdf")
plot(netdr, totaltips)
line <- lm(totaltips ~ netdr)
abline(line)
dev.off()

?abline
#Question 5- As speciation rate increases, the average branch length decreases 
pdf("Question 5 speciation rate and average branch length")
plot(speciationrate, Avgbranchlength)
dev.off()
#Question 6= -0.5327031
SRbranchlength <-cor(speciationrate, Avgbranchlength)
#Question 7 
?Ntip
trees[17]
TreeK<- trees[[17]]
pdf("Question 7 Largest Tree.pdf")
plot(TreeK)
dev.off()
rates <- vector()
traits <- list()

for (i in 1:100) {
  rates[i] <- runif(1)
  traits[[i]]<- fastBM(TreeK, sig2=rates[i])
  
}
#Question 8 
?fastBM
?mean
?which
?list
?fastBM
mtraitsK <- sapply(traits, mean)
mtraitsratecor<-cor(mtraitsK, rates)
plot(mtraitsK, rates)
setwd('/Users/kaylacoffman/Desktop/Evolution/Tasks/Task_10')
pdf("Question 8 Mean of Traits vs Variance.pdf")
plot(mtraitsK,rates)
dev.off()
#There is a essentially no correlation between mean of traits and rates (-0.1341506)
#Question 9 
vtraitsk <- sapply(traits, var)
vtraitsratecor<- cor(vtraitsk,rates)
#There is a large positive correlation between the variance of traits and rates
# (0.6467732)
pdf("Question 9 Variance of traits and rates.pdf")
plot(vtraitsk, rates)
dev.off()
#Question 10 
element1<-sapply(traits,"[[",1)
element2<-sapply(traits,"[[",2)
traitMat<- cbind(element1,element2)
element1and2cor <- cor(element1,element2)
pdf("Q10 First element vs Second element of Traits.pdf")
plot(element1,element2)
dev.off()
#There is a positive correlation of 0.2663275 between element 1 and element 2 of the traits.
#There is a slight correlation due to relatedness in the phylogeny tree. 
#We account for the phylogeny because
#living organisms cannot have independent variables. No this is not significant  
#because elements of the traits are generated randomly and each time
#there is a slight positive correlation between the first and second element. 
#This shows that all of the traits are positively correlated in some way
#meaning the fist and second element having a small positive correlation 
#doesn't actually hold any significance.

#Extra credit 
TreeK2 <- pbtree(n=100)
X <- fastBM(TreeK2, nsim=2)
pdf("extra credit.pdf")
phylomorphospace(TreeK2, X, xlab="element1", ylab="element2")
dev.off()



