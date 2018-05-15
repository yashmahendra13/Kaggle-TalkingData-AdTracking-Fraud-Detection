#Sampling data using SMOTE

rm(list = ls())

#Loading Library for SMOTE
library(DMwR)
library(ROSE)
library(class)

#set.seed(0)

?SMOTE

df <- read.csv("E:/Semester 2/KDD/Project/Unsampled_Data.csv")
View(df)
df$X <- NULL

#data <- subset(df, select = -c(os))
data <- df
data$is_attributed<-as.factor(as.numeric(data$is_attributed))

#View(data)

table(data$is_attributed)

is.factor(data$is_attributed)

## now using SMOTE to create a more "balanced problem"

dm <- data
View(dm)

#Moving OS and is_attributed column to last 
ncols<-ncol(dm)
dm<-cbind(dm[2:ncols],dm[1])

#View(dm)
dm<-cbind(dm[2:ncols],dm[1])

#ncols<-ncol(dm)
#dm<-cbind(dm[2:ncols],dm[1])
?SMOTE
dmSmote<-SMOTE(is_attributed ~ . , dm,k=5,perc.over = 5500,perc.under=500)
dm2<-cbind(dmSmote[ncols],dmSmote[1:ncols-1])

table(dm2$is_attributed)

View(dm2)

dm3 <- subset(dm2, select = -c(os))
#View(dm3)

dm3 <- na.omit(dm3)
#View(dm3)

ncols<-ncol(dm3)
dm3<-cbind(dm3[2:ncols],dm3[1])

#Creating Smote_Sampled Excel File
write.csv(dm3,'E:/Semester 2/KDD/Smote_Sampled.csv')
