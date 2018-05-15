library(heuristica)
library(caret)
library(randomForest)
library(e1071)
library(RWeka)
library(C50)
library(class)

#### utilities
## factored fields prediction
prediction <- function (algo, modelDS, testDf, col) { 
  predictField <- predict(modelDS,testDf[,-col])
  table(predicted = predictField, actual = testDf[,col])
  accuracyRate <- sum(predictField == testDf[,col])/nrow(testDf) * 100
  errorRate <- sum(predictField != testDf[,col])/nrow(testDf) * 100 
  print(paste(algo, ": accuracyRate: ", accuracyRate, "%, errorRate: ", errorRate, "% ", sep=""))
}

## non factored fields prediction
##predictionNonFactored <- function (algo, modelDS, testDf, col) { 
##  table(predicted = modelDS, actual = testDf[,col])
##  accuracyRate <- sum(modelDS == testDf[,col])/nrow(testDf) * 100
##  errorRate <- sum(modelDS != testDf[,col])/nrow(testDf) * 100 
##  print(paste(algo, ": accuracyRate: ", accuracyRate, "%, errorRate: ", errorRate, "% ", sep=""))
##}  

featuresDF <- read.csv('/Users/Paromita/Documents/CS-513/ProjectFiles/FinalProject/FeatureEngineeredData.csv')
featuresDF$is_attributed <- as.factor(featuresDF$is_attributed)

#store every 5th record in “test” starting with the first, the rest in “training”
testDf <- featuresDF[c(rep(FALSE,4),TRUE), ]
trainingDf <- featuresDF[c(rep(TRUE,4),FALSE), ]

nBayesAll <- naiveBayes(is_attributed ~., data=trainingDf)
prediction("Naive Bayes", nBayesAll, testDf, 1)  #1 is col number of is_attributed

rf <- randomForest(is_attributed~.,data=trainingDf,importance=TRUE,ntree=200)
importance(rf)
varImpPlot(rf)
prediction("Random Forest", rf, testDf, 1)   #1 is col number of is_attributed

c45 <- J48(is_attributed~., data=trainingDf)
plot(c45)
summary(c45)
prediction("C4.5", c45, testDf, 1)  #1 is col number of is_attributed

#knnpredict <- knn(trainingDf[,-1],testDf[,-1],trainingDf[,1],k=1)
#predictionNonFactored("KNN", knnpredict, testDf, 1)   #1 is col number of is_attributed

#c5 <- C5.0(is_attributed~.,data=trainingDf)
#plot(c5)
#summary(c5)
#prediction("C5.0", c5, testDf, 1)

#PCA
pc <- princomp(trainingDf[-1], cor = TRUE, scores = TRUE)
summary(pc)
plot(pc)
plot(pc, type='l')
biplot(pc)
