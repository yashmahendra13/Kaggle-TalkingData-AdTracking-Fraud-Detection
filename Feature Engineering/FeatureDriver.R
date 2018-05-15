## This is the driver code to extract features
## It reads the data into a dataframe and then 
## calls each feature method which returns new features for the
## subset of data (dataset) passed to them
## finally the features are accumulated across datasets
## into a final featuresDF

##/Library/Frameworks/R.framework/Versions/3.4/Resources/library/
#installed.packages()
library(heuristica)
library(caret)
library(randomForest)
library(e1071)
library(RWeka)
library(C50)
library(class)

rm(list = ls())

source("/Users/Paromita/Documents/CS-513/ProjectFiles/FinalProject/FeatureGeneration.R")

df <- read.csv("/Users/Paromita/Documents/CS-513/ProjectFiles/FinalProject/train_sample.csv")

##break the dataframe in smaller datasets -- group by app
groupedDS <- split(df, df$app)

## iterate thru each dataset and add features to that part of the dataset
## then append to a cumulative dataframe that will contain the dataframe with the added features
featuresDF = c()
for(ds in groupedDS) {
  min1 <- bucketInMins(ds$click_time, 1)
  min5 <- bucketInMins(ds$click_time, 5)
  #min60 <- bucketInMins(ds$click_time, 60)
  hr1 <- bucketInHrs(ds$click_time, 1)
  hr3 <- bucketInHrs(ds$click_time, 3)
  hr6 <- bucketInHrs(ds$click_time, 6)
  
  addedTimeFeat <- featuresByTime(ds, min1$Freq, min5$Freq, hr1$Freq, hr3$Freq, hr6$Freq)
  addedIpFeat <- featuresByIP(addedTimeFeat)
  addedDeviceFeat <- featuresByDevice(addedIpFeat)
  addedChannelFeat <- featuresByChannel(addedDeviceFeat)
  
  featuresDF <- rbind(featuresDF, addedChannelFeat)
  
}
#print(featuresDF)
#remove origianl columns and write to file
featuresDF <- featuresDF[,-1:-7]
write.csv(featuresDF,'/Users/Paromita/Documents/CS-513/ProjectFiles/FinalProject/FeatureEngineeredData.csv', row.names=FALSE) #to avoid printing row numbers

