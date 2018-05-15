library(randomForest)
library(RWeka)

rm(list = ls())

#### utilities
## factored fields prediction
prediction <- function (algo, modelDS, testDf, col) { 
  predictField <- predict(modelDS,testDf[,-col])
  predictionNonFactored(algo, predictField, testDf, col)
}

## non factored fields prediction
predictionNonFactored <- function (algo, modelDS, testDf, col) { 
  table(predicted = modelDS, actual = testDf[,col])
  accuracyRate <- sum(modelDS == testDf[,col])/nrow(testDf) * 100
  errorRate <- sum(modelDS != testDf[,col])/nrow(testDf) * 100 
  print(paste(algo, ": accuracyRate: ", accuracyRate, "%, errorRate: ", errorRate, "% ", sep=""))
} 

#process smote sampled data
featuresDF <- read.csv("/Users/Paromita/Documents/CS-513/ProjectFiles/FinalProject/Smote_Sampled.csv")
featuresDF$is_attributed <- as.factor(featuresDF$is_attributed)

#store every 5th record in “test” starting with the first, the rest in “training”
testDf <- featuresDF[c(rep(FALSE,4),TRUE), ]
trainingDf <- featuresDF[c(rep(TRUE,4),FALSE), ]

rf <- randomForest(is_attributed~.,data=trainingDf,importance=TRUE,ntree=200)
importance(rf)
varImpPlot(rf)
prediction("Random Forest", rf, testDf, 29)  #29 is col number of is_attributed

c45 <- J48(is_attributed~., data=trainingDf)
#summary(c45)
prediction("C4.5", c45, testDf, 29) #29 is col number of is_attributed

################################################################################################

##process rose sampled data
featuresDF <- read.csv("/Users/Paromita/Documents/CS-513/ProjectFiles/FinalProject/Rose_Sampled_Data.csv")
featuresDF <- featuresDF[,-1]
featuresDF$is_attributed <- as.factor(featuresDF$is_attributed)

#store every 5th record in “test” starting with the first, the rest in “training”
testDf <- featuresDF[c(rep(FALSE,4),TRUE), ]
trainingDf <- featuresDF[c(rep(TRUE,4),FALSE), ]

rf <- randomForest(is_attributed~.,data=trainingDf,importance=TRUE,ntree=200)
importance(rf)
varImpPlot(rf)
prediction("Random Forest", rf, testDf, 3) #3 is col number of is_attributed

c45 <- J48(is_attributed~., data=trainingDf)
#summary(c45)
prediction("C4.5", c45, testDf, 3)  #3 is col number of is_attributed
