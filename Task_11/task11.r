x <- rnorm(100, 5, 2)
y <- (x*5) + 2 + runif(100, min=0, max=0.1)
lm(y~x)
#The slope is 5.001 and the y intercept is 2.047. The slope and y intercept 
#are dependent on the restrictions of the mean and variance.
?rnorm 
slope <- c()
yint <- c() 
x <- c()
y <- c()
z <- c()

 for (i in 1:100) {
   x[i] <- rnorm(100, 5, 2)
   z[i] <- rnorm(1)
   number[i] <- x * z
   y[i] <- (number*5) + 2 + runif(100, min=0, max=0.1)
   mod<- lm(y~x)
   cf <- coef(mod)
   slope[i] <- cf["x"]
 }
plot(z, slope)
#This plot reveals a stable/constant relationship between z and the slope. 

#Question 2 
library(dplyr)
library(ggplot2)
doors <- 1:3
sample_doors <- function() { return(sample(doors, size = 1000, replace = TRUE))}
games <- data.frame(prize = sample_doors(), pick = sample_doors())
games$strategy <- factor(ifelse(games$prize == games$pick, 'stay', 'switch'))
monte_show <- function(prize, pick) {
  remaining <- setdiff(doors, c(prize, pick))
  return(ifelse(length(remaining)==1,
                remaining,
                sample(remaining, 1)))
}
games <- games %>%
  rowwise %>%
  mutate(shown = monte_show(prize, pick),
         stay = pick,
         switch = setdiff(doors, c(pick, shown)),
         strategy = factor(ifelse(prize == stay, 'stay', 'switch')))
print(summary(games$strategy) / nrow(games))
qplot(strategy, data = games, fill = strategy, geom = 'bar') + 
  xlab('Winning Strategy') +
  ggtitle('Monty Hall Problem Simulation')  
setwd('/Users/kaylacoffman/Desktop/Evolution/Tasks/Task_11')
pdf("Monty Hall Plot.pdf")
dev.off()

#Question 3 
install.packages("meme")
library(meme)
u <- "https://i.kym-cdn.com/photos/images/original/001/369/654/68b.jpg"
x <- meme(u, "Me about to run the R code I spent 10 minutes typing in", "The comma I accidentally typed in thats gonna give me an error", size=0.9, vjust=0.29)
plot(x)



