library(learnPopGen)
??learnPopGen
coalescent.plot(n=20, ngen=30, col.order="alternating")
cp1 <- coalescent.plot(n=20, ngen=30, col.order="alternating")
#n=number of haploid ind or gene copies 
cp1
plot(cp1)
#we can see that at the end of the 30 generations we end up with 
#only green and orange (the blue, light blue, other color did
#not keep reproducing. The dots at generation 30 can be traced back 
#to the very beginning) - this was example i was messing around with
coalescent.plot(5,10) 
coalescent.plot(5,10)
coalescent.plot(5,10)
#Question 1- The simulation begins with 5 alleles because 
#each circle represents a haploid individual,  this can be changed 
#by changing the n 
mean(c(3, 8, 5))
#Question 2- It took an average of 6 generations for one allele
#to go to fixation 
var(c(3, 8, 5))
#Question 3- Each haploid has an average number of 1 offspring, the 
#variance is 6.3 
#Question 4- Fitness does not play a role in this scenario because
# they are random 
#Question 5- no the focal locus is not alive in generation in 
#zero, there is a closer common ancestor 
install.packages("coala")
library(coala)
install.packages("phytools")
library(phytools)
install.packages("rehh", dep=T)
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
model <- coal_model(sample_size=5, loci_number=10, loci_length = 500, ploidy = 2)+
  feat_mutation(10) + 
  feat_recombination(10) +
  sumstat_trees() + 
  sumstat_nucleotide_div()
simulate(model) 
stats <- simulate(model, nsim = 1)
Diversity <- stats$pi
#no the numbers are not the same, recombination causes genetic diversity 
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#SNP - single-nucleotide polymorphism- a variation at a single 
#position in a DNA seq among individuals 
#Question 6 There are 5 individuals but 10 tips because 
#each individual has 2 copies of each allele 
Age1 <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]] [1])
plot(t2)
axisPhylo()
Age2 <- max(nodeHeights(t2))
#the first SNP of the second locus is farther back and is a 
#different age than the 1st SNP. The first SNP was 0.214
#while the second was 0.561
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot (t2)
axisPhylo()
#t1 is on the left, t2 on the right 
#Question 7- no they do not match each other. t2 extends more
#than t1, meaning t2 is older than t1. 
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
#graph on left is comparison of t1 and t1 while graph on left
#compares t1_1 and t1_2 
for (locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for (n in 1:ntrees) {
    if (locus == 1 && n==1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else { 
      outPhy<- ape:::c.phylo(outPhy,read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
#now well plot trees all at once 
par(mfrow=c(1,1))
densityTree(outPhy)
model3 <- coal_model(10,50) + 
  feat_mutation(par_prior("theta", sample.int(100,1))) + 
  sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi)
plot(mean_pi, theta)
Line <- lm(mean_pi ~ theta)
abline(Line)
#Extra Credit 
?feat_mutation
?feat_size_change
modelk <- coal_model(c(10,20),5) +
  feat_size_change(.3, population=2, time = "1") + 
  feat_selection(strength_A = 0.9, population = 2, time=1, locus_group = "all") +
  feat_selection(strength_A=1.1, population=1, time=0, locus_group = "all")+
  feat_mutation(par_prior("theta"), sample.int(100,1))+
  feat_migration(0.5, 1, 2) +
  feat_migration(1, 2, 1) + 
  feat_growth(10, time=0) + 
  feat_growth(5, time=1) + 
  sumstat_nucleotide_div()
#new model 
modelk <- coal_model(c(10,20),9) + 
  feat_size_change(.3, population=2, time = "1")
  feat_mutation(par_prior("theta", sample.int(100,1))) + 
  feat_migration(0.5, 1, 2) +
  feat_migration(1, 2, 1) + 
  feat_growth(10, time=0) + 
  feat_growth(5, time=1) + 
  feat_selection(strength_A = 0.9, population = 2, time=1, locus_group = "all") +
  feat_selection(strength_A=1.1, population=1, time=0, locus_group = "all") +
  sumstat_nucleotide_div()
  #newest Model - this one works 
modelk<- coal_model(c(10,20),9) + 
  feat_size_change(.3, population=2, time = "1") + 
  feat_mutation(par_prior("theta", sample.int(100,1))) + 
  feat_migration(0.5, 1, 2) + 
  feat_migration(1, 2, 1) + 
  feat_growth(10, time=0) + 
  feat_growth(5, time=1) + 
  feat_selection(strength_A = 0.9, population = 2, time=1, locus_group = "all") +
  feat_selection(strength_A=1.1, population=1, time=0, locus_group = "all") +
  sumstat_nucleotide_div()
  
  

  
  
  
  

check_model(modelk)
Output <- simulate (modelk,nsim=100)
Pis <- sapply(Output, function(x) x$pi)
#this model worked! 


#activate_msms(jar = NULL, java = NULL, priority = 200, download = TRUE)
?feat_selection
?sumstat
simulate (modelk)

statsk <- simulate (modelk, nsim = 2)
population <- statsk$pi
activate_scrm(priority = 400)
install.packages("phyclust")
library("phyclust")
activate_ms(priority = 500)


for(i in 1:100) {
  population1 <- length(statsk$population[p:1[i]])
  population2 <- length(statsk$population[p:2[i]])
} 
jar msms3.2rc-b163.jar
  
  
