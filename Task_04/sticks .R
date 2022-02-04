setwd("/Users/kaylacoffman/Desktop/Evolution/Tasks/Task_04")
source("fxn05.R")

results <- read.csv('biol112labresults.csv')
counts <- results[,c("yellow", "red", "green","blue","black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd","black")
calcChi(counts[1,]) #chi square stat for first row only 
Chisqs <- apply(counts, 1, calcChi) #chi square all the rows at once 
plotChis(counts) #when the observed and expected are even the chi square is 
#closer to zero. The greater diff between them, the larger the x^2 
#More even the bars= lower the x^2 
#less even the bars= higher x^2 
Avg <- mean(Chisqs)
#the average Chi-squared is 60.99081, This means the bars are not very even, the 
#expected and observed differ by a lot 
#the avg chi-squared is larger than the critical value 
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
# yes each of the background averages are different 
propSig <- length(which(Chisqs > 11.70) /length(Chisqs))
percSig <- round(100 * propSig)
#percSig= # of trials that had sig p-value. It was 50,000, I didnt expect it to 
#be that large
#no there must be something else besides natural selection driving it 
par(las = 1, mar = c(4 ,4 ,1 ,1), mgp = c(2,0.5,0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-squared values", ylab="frequency")
par(las = 1, mar = c(4 ,4 ,1 ,1), mgp = c(2,0.5,0), tck = -0.01, cex.axis=1)
plot(1,1, xlim=c(0,400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt = "n")
axis (2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds) {
  Data <- Chisqs[which(results[,3] ==i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter + 1 
}

abline(v = 11.70, lty=2, lwd =2 , col='black') #represents critical value
#The red background seems to extend the most meaning it is more right of the 
#critical value number (more signinficant trials)
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v = 11.70, lty=2, lwd =2)
#Around 95% of the time the selection free simulation has a significant result
#There is no natural selection meaning the observed is closer to the expected 
#compared to the other initial counts where selection occured 
#no fitness differences 
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6 
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
#one tooth pick type selected against 
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6 
Simulation3 <- simDraws(1e4, w = Fit) 
addHist(Y=8, Dat=Simulation3, Color=rgb(0, 0, 0,0.25))
#3 tooth pick types selected against 
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit) 
addHist(Y=8, Dat=Simulation4, Color=rgb(0, 0, 0,0.25))
#five selected against
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit) 
addHist(Y=8, Dat=Simulation5, Color=rgb(0, 0, 0,0.25))
#insane selection 
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names (Fit) <- 1:6 
Simulation6 <- simDraws(1e4, w = Fit) 
addHist(Y=8, Dat=Simulation6, Color=rgb(0, 0, 0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat= Simulation7, Color=rgb(0, 0, 1, 0.25))
?addHist 
#the mixture is not as quite to the right of the critical value compared to 
#he student generated data but it does have more peaks 
# i would say the students did have strong selection but not as strong 
#as what the mixture simulation generated 
#selection was relatively high across all groups in the labs 
#Inference 
#natural section
#In the computer simulations natural selection and other evolutionary processes 
#like genetic drift or mutation can occur 
# the graphs tell us the evolutionary processes the student are stimulating
#are relatively strong 
# Comparing the student numbers to the stimulated numbers gave more perspective
#than the single critical value 
#if a toothpick mutated to a different type, the chi squared number
#would increase because there would be more unevenness between expected and 
#observed 
#Extra Credit #didnt work  
function(Popsize=100, nGenerations=100, h=1, s=0, initial_p=0.5, mu = 0, twoway = TRUE, w = NULL)	{
  if (is.null(w))	{
    # make fitness vector using h & s values
    w <- c(aa = 1, ab = 1 - h * s, bb = 1 - s)
  }
  else if (!is.null(w))	{
    if (length(w) != 3)	{
      stop(cat("Not enough fitness values supplied. Set w to NULL or provide 3 fitnesses"))
    }
    if (is.null(names(w)))	{
      names(w) <- c("aa", "ab", "bb")
      cat("Setting names of w to aa, ab, and bb")
    }
    
    
  }
  function(Popsize=100, nGenerations=100, h=1, s=0, initial_p=0.5, mu = 1, twoway = TRUE, w = NULL)