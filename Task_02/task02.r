setwd("~/Desktop/Evolution/Tasks/Task_02")
Data <- read.csv('beren.csv',stringsAsFactors=F)
sunflowers <- "potatoes"
pot

# To the right of a hashtag, R ignores
# This is called a comment.
write.csv (Data, 'rawdata.csv' , quote=F)
Data
# an example of an object is Data. An object is like a noun while a function
#is like a verb (doing something). R recognizes Data as what we told it.
#putting things in "" means R reads them as string of 
#letters not function or object 
length (Data)
#12 numbers
nrow(Data)
#1363 #rows come before columns 
ncol(Data)
#12
colnames(Data)
#data consists of dates, times, names 
head(Data)
#gives you first 6 rows
Data[1,]
#shows the very first row of data 
Data[2,]
#shows 2nd row of data 
Data[1:3 ,]
#gives you rows 1-3 
Data[1:3 , 4]
#gives column 4 of rows 1-3 
Data[1:5 , 1:3]
#5 rows of columns 1-3 
Data[257,]
Feeds <- which(Data[ ,9] =='bottle')
berenMilk <- Data[Feeds ,]
head(berenMilk)
#shows 6 rows representing each time there was a feeding 
Feeds <- which(Data[, 'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
Feeds
head(berenMilk)
Data
#column 9 of the data represented by [, 9] is the events which include
#the bottle feedings. The brackets and dollar sign do the same thing. 
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-')) 
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d" , origin = "2019-04-08")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]
head(beren2)
head(beren3)
#beren 2 is just a copy of the data but beren 3 put the dates in order 
#so now instead of the dates going from april to october they stay in 
#april,, 4/18, 4/20, 4/22, etc where he's 0,2, and 4 days old 
write.csv(beren3, 'beren_new .csv' , quote=F, row.names=FALSE)
#Question 1- For hypothesis 1 we do not have the data to test this hypothesis 
#because we do not know how much Beren "eats" each day. 
#Hypothesis 2 is inappropriate because "there is a relationship" gives
#no sort of direction and "how much" needs to be specified, does this mean 
#frequency or duration?
beren3
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgMilk
beren3
#units for avgMilk is oz, value was used instead of the event 
#column bc event tells us "what" it was we were looking at,but the actual values
#of the feeding were needed in order to calculate the mean 
#the [Feeds] was used because we had previously made the Object Feeds 
#specifying we were looking at the event "bottle" and with the newest object
#avgMilk we were specifying the mean of the bottle/Feeds Values. 
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
?cor
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab = "amount of milk consumed (oz)")
?par   
#las=1 style of axis labels: always horizontal 
#marc=c margins of the plot 
#mgp= margin line for axis title, axis lables, and axis line 
#tck= length of tick marks 
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, ,type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
getwd()
source("http://jonsmitchell.com/code/plotFxn02b.R")
#Question 02: The total milk/ age in days/ time of day graph is impossible to 
#interpret because there are three axis labels but only two axis. Also there
#was not sufficient end times of that data recorded to know the time of day for the 
#feedings. The nap graph y axis"nap time" has no units.
unique(beren3$event)
beren3
Counting <- which(beren3$event == 'skill_count')
Counting
avgCounting <- mean(beren3$value[Counting])
avgCounting
cor.test(beren3$value[Counting], beren3$age[Counting])
Solids <- which(beren3$event == 'solids')
Solids
avgSolids <- mean(beren3$value[Solids])
avgSolids
cor.test(beren3$value[Solids],  beren3$age[Solids])
t.test(beren3$value[Solids],  beren3$age[Solids])
#My hypothesis: Beren's mass in kg will increase as he ages by days 
Traitmass <- which(beren3$event == 'trait_mass')
Traitmass
avgTraitmass <- mean(beren3$value[Traitmass])
avgTraitmass 
cor.test(beren3$value[Traitmass], beren3$age[Traitmass])
t.test(beren3$value[Traitmass], beren3$age[Traitmass])
#correlation and t-test both show a p-value less than 0.05 indicating 
#a significance between the data 
Naps <- which(beren3$event == 'nap')
beren4 <- beren3[Naps, ]
beren4
?tapply
Start <- beren4$start_hour + beren4$start_minute/60
End <- beren4$end_hour + beren4$end_minute/60
duration <- End-Start 




plot(beren4$age, duration)
cor.test(beren4$age, duration)
#There is a negative correlation between Beren's age and nap duration. As Beren
#ages his nap durations decrease. 






























#############
Starttime <- tapply(beren4$start_hour, beren4$start_minute)
time1 <- "2019-08-22 12:30"
time1E <- "2019-08-22 13:30"
Aug22nap <- difftime(time1E,time1, units="hours")
time2 <- "2019-08-23 8:55"
time2E <- "2019-08-23 9:36"
Aug23nap <- difftime(time2E,time2, units="hours")
time3 <- "2019-08-23 11:52"
time3E <- "2019-08-23 12:46"
Aug23nap2 <- difftime(time3E, time3, units="hours")
time4 <-"2019-08-26 8:40"
time4E <- "2019-08-26 10:10"
Aug26nap <- difftime(time4E, time4, units="hours")
time5 <- "2019-08-26 12:45"
time5E <- "2019-08-26 13:20"
Aug26nap2 <- difftime(time5E, time5, units="hours")
Aug23naptotal <- sum(Aug23nap, Aug23nap2)
Aug26naptotal <- sum(Aug26nap, Aug26nap2)
time6 <- "2019-08-27 11:10"
time6E <- "2019-08-27 12:40"
Aug27nap <- difftime(time6E, time6, units="hours")
time7 <- "2019-08-27 14:10"
time7E <- "2019-08-27 14:45"
Aug27nap2 <- difftime(time7E, time7, units="hours")
Aug27naptotal <- sum(Aug27nap, Aug27nap2)
time8 <- "2019-08-30 7:30"
time8E <- "2019-08-30 8:42"
Aug30nap <- difftime(time8E, time8, units="hours")
time9 <- "2019-08-30 10:21"
time9E <- "2019-08-30 11:34"
Aug30nap2 <- difftime(time9E, time9, units="hours")
time10 <- "2019-08-30 14:27"
time10E <- "2019-08-30 14:37"
Aug30nap3 <- difftime(time10E, time10, units="hours")
Aug30naptotal <- sum(Aug30nap, Aug30nap2, Aug30nap3)
time11 <- "2019-09-04 8:04"
time11E <- "2019-09-04 9:19"
Sept4nap <- difftime(time11E, time11, units="hours")
time12 <- "2019-09-04 11:07"
time12E <- "2019-09-04 12:56"
Sept4nap2 <- difftime(time12E, time12, units="hours")
Sept4naptotal <- sum(Sept4nap, Sept4nap2)
time13 <- "2019-09-05 8:00"
time13E <- "2019-09-05 9:19"
Sept5nap <- difftime(time13E, time13, units="hours")
time14 <- "2019-09-05 12:15"
time14E <- "2019-09-05 12:33"
Sept5nap2 <- difftime(time14E, time14, units="hours")
time15 <- "2019-09-05 12:55"
time15E <- "2019-09-05 14:17"
Sept5nap3 <- difftime(time15E, time15, units="hours")
Sept5naptotal <- sum(Sept5nap, Sept5nap2, Sept5nap3)
time16 <- "2019-09-06 8:48"
time16E <- "2019-09-06 9:32"
Sept6nap <- difftime(time16E, time16, units="hours")
time17 <- "2019-09-06 11:34"
time17E <- "2019-09-06 12:10"
Sept6nap2 <- difftime(time17E, time17, units="hours")
time18 <- "2019-09-06 14:10"
time18E <- "2019-09-06 14:40"
Sept6nap3 <- difftime(time18E, time18, units="hours")
Sept6naptotal <- sum(Sept6nap, Sept6nap2, Sept6nap3)
time19 <- "2019-09-09 8:46"
time19E <- "2019-09-09 9:43"
Sept9nap <- difftime(time19E, time19, units="hours")
time20 <- "2019-09-09 12:38"
time20E <- "2019-09-09 13:23"
Sept9nap2 <- difftime(time20E, time20, units="hours")
Sept9naptotal <- sum(Sept9nap, Sept9nap2)
time21 <- "2019-09-10 9:00"
time21E <- "2019-09-10 9:40"
Sept10nap <- difftime(time21E, time21, units="hours")
time22 <- "2019-09-10 12:00"
time22E <- "2019-09-10 12:35"
Sept10nap2 <- difftime(time22E, time22, units="hours")
Sept10naptotal <- sum(Sept10nap, Sept10nap2)
time23 <- "2019-09-11 9:49"
time23E <- "2019-09-11 10:36"
Sept11nap <- difftime(time23E, time23, units="hours")
time24 <- "2019-09-11 11:49"
time24E <- "2019-09-11 12:05"
Sept11nap2 <- difftime(time24E, time24, units="hours")
time25 <- "2019-09-11 14:16"
time25E <- "2019-09-11 14:45"
Sept11nap3 <- difftime(time25E, time25, units="hours")
Sept11naptotal <- sum(Sept11nap, Sept11nap2, Sept11nap3)
time26 <- "2019-09-12 9:08"
time26E <- "2019-09-12 9:46"
Sept12nap <- difftime(time26E, time26, units="hours")
time27 <- "2019-09-12 12:07"
time27E <- "2019-09-12 13:05"
Sept12nap2 <- difftime(time27E, time27, units="hours")
Sept12naptotal <- sum(Sept12nap, Sept12nap2)
time28 <- "2019-09-13 8:22"
time28E <- "2019-09-13 9:17"
Sept13nap <- difftime(time28E, time28, units="hours")
time29 <- "2019-09-13 11:45"
time29E <- "2019-09-13 12:15"
Sept13nap2 <- difftime(time29E, time29, units="hours")
Sept13naptotal <- sum(Sept13nap, Sept13nap2)
time30 <- "2019-09-16 12:15"
time30E <- "2019-09-16 12:53"
Sept16naptotal <- difftime(time30E, time30, units="hours")
time31 <- "2019-09-18 10:40"
time31E <- "2019-09-18 11:02"
Sept18nap <- difftime(time31E, time31, units="hours")
time32 <- "2019-09-18 14:35"
time32E <- "2019-09-18 14:45"
Sept18nap2 <- difftime(time32E, time32, units="hours")
Sept18naptotal <- sum(Sept18nap, Sept18nap2)
time33 <- "2019-09-20 9:40"
time33E <- "2019-09-20 10:23"
Sept20nap <- difftime(time33E, time33, units="hours")
time34 <- "2019-09-20 13:21"
time34E <- "2019-09-20 14:14"
Sept20nap2 <- difftime(time34E, time34, units="hours")
Sept20naptotal <- sum(Sept20nap, Sept20nap2)
time35 <- "2019-09-23 11:17"
time35E <- "2019-09-23 11:32"
Sept23naptotal <- difftime(time35E, time35, units="hours")
time36 <- "2019-09-24 10:35"
time36E <- "2019-09-24 11:20"
Sept24nap <- difftime(time36E, time36, units="hours")
time37 <- "2019-09-24 14:21"
time37E <- "2019-09-24 15:03"
Sept24nap2 <- difftime(time37E, time37, units="hours")
Sept24naptotal <- sum(Sept24nap, Sept24nap2)
time38 <- "2019-09-25 8:40"
time38E <- "2019-09-25 9:30"
Sept25naptotal <- difftime(time38E, time38, units="hours")
time41 <- "2019-09-30 9:36"
time41E <- "2019-09-30 10:10"
Sept30naptotal <- difftime(time41E, time41, units="hours")
time43 <- "2019-10-02 8:57"
time43E <- "2019-10-02 10:14"
Oct2nap <- difftime(time43E, time43, units="hours")
time44 <- "2019-10-02 13:23"
time44E <- "2019-10-02 14:00"
Oct2nap2 <- difftime(time44E, time44, units="hours")
Oct2naptotal <- sum(Oct2nap, Oct2nap2)
time45 <- "2019-10-03 9:10"
time45E <- "2019-10-03 10:25"
Oct3naptotal <- difftime(time45E, time45, units="hours")
time47 <- "2019-10-04 7:52"
time47E <- "2019-10-04 9:10"
Oct4nap <- difftime(time47E, time47, units="hours")
time48 <- "2019-10-04 14:09"
time48E <- "2019-10-04 15:20"
Oct4nap2 <- difftime(time48E, time48, units="hours")
Oct4naptotal <- sum(Oct4nap, Oct4nap2)
time49 <- "2019-10-07 9:10"
time49E <- "2019-10-07 9:43"
Oct7nap <- difftime(time49E, time49, units="hours")
time50 <- "2019-10-07 12:28"
time50E <- "2019-10-07 13:22"
Oct7nap2 <- difftime(time50E, time50, units="hours")
Oct7naptotal <- sum(Oct7nap, Oct7nap2)
time51 <- "2019-10-08 9:23"
time51E <- "2019-10-08 10:00"
Oct8nap <- difftime(time51E, time51, units="hours")
time52 <- "2019-10-08 12:15"
time52E <- "2019-10-08 12:40"
Oct8nap2 <- difftime(time52E, time52, units="hours")
time53 <- "2019-10-08 14:45"
time53E <- "2019-10-08 15:10"
Oct8nap3 <- difftime(time53E, time53, units="hours")
Oct8naptotal <- sum(Oct8nap, Oct8nap2, Oct8nap3)
time54 <- "2019-10-09 9:20"
time54E <- "2019-10-09 10:55"
Oct9naptotal <- difftime(time54E, time54, units="hours")
time56 <- "2019-10-10 9:18"
time56E <-  "2019-10-10 9:40"
Oct10naptotal <- difftime(time56E, time56, units="hours")
time58 <-  "2019-10-15 9:30"
time58E <-  "2019-10-15 10:00"
Oct15nap <- difftime(time58E, time58, units="hours")
time59 <-  "2019-10-15 11:35"
time59E <-  "2019-10-15 12:15"
Oct15nap2 <- difftime(time59E, time59, units="hours")
Oct15naptotal <- sum(Oct15nap, Oct15nap2)
time60 <-  "2019-10-16 9:05"
time60E <-  "2019-10-16 9:45"
Oct16nap <- difftime(time60E, time60, units="hours")
time61 <-  "2019-10-16 13:30"
time61E <-  "2019-10-16 14:10"
Oct16nap2 <- difftime(time61E, time61, units="hours")
Oct16naptotal <- sum(Oct16nap, Oct16nap2)
time62 <-  "2019-10-17 9:21"
time62E <-  "2019-10-17 9:52"
Oct17nap <- difftime(time62E, time62, units="hours")
time63 <-  "2019-10-17 14:12"
time63E <-  "2019-10-17 14:45"
Oct17nap2 <- difftime(time63E, time63, units="hours")
Oct17naptotal <- sum(Oct17nap, Oct17nap2)
time64 <-  "2019-10-18 9:10"
time64E <-  "2019-10-18 9:58"
Oct18nap <- difftime(time64E, time64, units="hours")
time65 <-  "2019-10-18 13:01"
time65E <-  "2019-10-18 13:30"
Oct18nap2 <- difftime(time65E, time65, units="hours")
Oct18naptotal <- sum(Oct18nap, Oct18nap2)
time66 <-  "2019-10-21 9:27"
time66E <-  "2019-10-21 10:00"
Oct21nap <- difftime(time66E, time66, units="hours")
time67 <-  "2019-10-21 13:00"
time67E <-  "2019-10-21 13:30"
Oct21nap2 <- difftime(time67E, time67, units="hours")
Oct21naptotal <- sum(Oct21nap, Oct21nap2)
time68 <-  "2019-10-22 9:17"
time68E <-  "2019-10-22 10:30"
Oct22nap <- difftime(time68E, time68, units="hours")
time69 <-  "2019-10-22 13:10"
time69E <-  "2019-10-22 13:25"
Oct22nap2 <- difftime(time69E, time69, units="hours")
Oct22naptotal <- sum(Oct22nap, Oct22nap2)
time70 <-  "2019-10-23 9:00"
time70E <-  "2019-10-23 9:45"
Oct23nap <- difftime(time70E, time70, units="hours")
time71 <-  "2019-10-23 11:32"
time71E <-  "2019-10-23 12:55"
Oct23nap2 <- difftime(time71E, time71, units="hours")
Oct23naptotal <- sum(Oct23nap, Oct23nap2)
time72 <-  "2019-10-24 8:45"
time72E <-  "2019-10-24 9:15" 
Oct24nap <- difftime(time72E, time72, units="hours")
time73 <-  "2019-10-24 11:10"
time73E <-  "2019-10-24 12:40"
Oct24nap2 <- difftime(time73E, time73, units="hours")
Oct24naptotal <- sum(Oct24nap, Oct24nap2)
time74 <-  "2019-10-25 11:40"
time74E <-  "2019-10-25 12:14"
Oct25naptotal <- difftime(time74E,time74, units="hours")
time76 <-  "2019-10-28 10:53"
time76E <-  "2019-10-28 11:43"
Oct28naptotal <- difftime(time76E, time76, units="hours")


