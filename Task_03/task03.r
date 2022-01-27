#make our populations
trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
#taking sample of each pop
Size <- 50 
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
boxplot(Sample1, Sample2)
#sample and population were different from each other, the y-value ranges differ
source("http://jonsmitchell.com/code/simFxn04.R")
Matgrandma <- makeFounder("grandma_mom")
head(Matgrandma)
nrow(Matgrandma)
Matgrandpa <- makeFounder("grandpa_mom")
head(Matgrandpa)
PatGrandma <- makeFounder("grandma_da")
head(PatGrandma)
PatGrandpa <- makeFounder("grandpa_da")
head(PatGrandpa)
Alan <- makeBaby(PatGrandma, PatGrandpa)
head(Alan)
Brenda <- makeBaby(Matgrandma, Matgrandpa)
head(Brenda)
Focus <- makeBaby(Brenda, Alan)
head(Focus)
#Brenda should have shared 50% of her genes with Focus 
ToMom <- length(grep("mom", Focus)) /length(Focus)
#my prediction is the genes Focus shares with his maternal gp is 0.25
ToMomMom <- length(grep("grandma_mom" , Focus)) /length (Focus)
ToMomDad <- length(grep("grandpa_mom" , Focus)) /length (Focus)
#Actually it is 0.09955 Focus's mothers dad and 0.40045 to his
#mothers mom 
ToDadMom <- length(grep("grandma_da", Focus)) /length (Focus)
ToDadDad <- length(grep("grandpa_da", Focus))/ length (Focus)
#Focus is not equally related to his paternal or maternal grandparents 
# This is not what I expected and the average relatedness is 25%
Sibling_01 <- makeBaby(Brenda, Alan)
#I expect Focus to share 50% of DNA with sibling and no it was 0.6804
ToSib <- length(intersect(Focus, Sibling_01)) /length(Focus)
#Focus shares around 50% with his siblings? Each sibling varies in the amount
#of genes shared which is shown 
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/ length(Focus))
quantile(ManySiblings)
?quantile
head(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
#most siblings share around 50% of genes but it can range, degree of relatedness
#may differ, This could be due to genetic recombination
HWE <- function(p)  {
  aa <- p^2
  ab <- 2 * p * (1-p)
  bb <- (1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1,1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from=0, to=1, by=0.01)
GenoFreq <- t(sapply(p,HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#the freq of aa individuals increase as freq a allele increases in pop
#aa frequency decreases as a allele decreases in pop
#time and geographic space is not shown on plot 
#add in other genotypes
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
?bty
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#the frequency of the aa genotype is close to the expectation of Hardy-Weinberg
#but not exact
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
Pop <- simPop(5)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/5, pch=22, bg="blue")
Pop <- simPop(1e3)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/1e3, pch=22, bg="red")
#As the popultion becomes smaller the data is more spread out 
#and there is more deviation compared to a larger sample 
?pch
#
install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=200, pause=0.01)
x <- genetic.drift(Ne=50, nrep=200, pause=0.01)
x <- genetic.drift(Ne=5, nrep=200, pause=0.01)
#I changed the Ne to 50 and noticed that when the number of
#individuals was smaller(50) the lines covered much more of the y axis and when 
#the Ne was lowered to 5 the lines were much less dense as time went on 
PopSizes <- 5:50 
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x,500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm(tExt~Samples + 0)
summary(Line2)
Line2$coef
plot (Samples, tExt)
abline (Line2)
#Line is slightly above Line 2, +0 means there is no intercept (only samples shown)
#As pop size increases, the points get farther away from the line. 
#This means that as sample/pop size increases there is more variation shown in the
#data and possible outliers 
#Extra Credit 
install.packages ("caret")
library (caret) 
tExt2 <- BoxCoxTrans(Samples,tExt)
Samples2 <- cbind(Samples, tExt_ = predict(Text2, Samples, tExt))
head(Samples2)
tExt3 <- lm(tExt_ ~ Samples, tExt= Samples2)
#second try at EC 
df <- data.frame(x1=Samples, y=tExt)
head(df)
ols <- lm(y~x1, data=df)
plot(df$y, rstandard(ols), ylab='Standardized Residuals', abline (h=0))
install.packages("MASS")
library(MASS)
robust <- rlm(y~x1, data=df)
summary(ols)$sigma
summary(robust)$sigma
plot(robust)
#third try at EC
Line3 <- rlm(tExt ~ Samples)
Line4 <- lm(tExt ~ Samples)
abline(Line3)
Line3$coef
summary(Line3)
coef(Line3)
coef(Line4)
abline(Line4)
#Line 3 (robust to heteroskedasticity) is below both Line 1 and Line 2. The slope for the 
#corrected Line 3 (2.38) is less than the slope for the normal linear model 
#(2.87) which is represented by Line 4 