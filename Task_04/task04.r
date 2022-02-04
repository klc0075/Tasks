setwd("/Users/kaylacoffman/Desktop/Evolution/Tasks/Task_04")
source("fxn05.R")
#inital_p is starting freq for a allele, h=heritability s=selection coef
#for allele b 
Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p=0.5, h=1, s=0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0,1), type = "l", xlab="generation", ylab="allele freq." , lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd=2, col='red')
legend("topleft", legend = c("a", "b"), col=c("black", "red"), lwd=2, bty="n")
plotFit(nruns = 10, n = 50, ngens=100, init_p = 0.5, h=1, s=0)
#diff colors representing freq of alleles of each nrun (10)
Expectation <- c(10,10,10,10)
Observed <- c(40,0,0,0)
Chisq <- sum(((Expectation-Observed)^2)/Expectation)
barplot(rbind(Expectation, Observed), beside = T, main=bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))
#x^2=10 when observed is 15, 15, 5, 5 
#x^2 value is 85 when observed values are 5 0 0 35 
#x^2 value is 0 when all observed values are 10 
#I set the observations to only one category and got an X^2 value of 120 which 
#was larger than previous values meaning the observed and expected are not even
#the lower the x^2 the more even the bars are, when x^2 is 0 the bars are
#all even 




