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