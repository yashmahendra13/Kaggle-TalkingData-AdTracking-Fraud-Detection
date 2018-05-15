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
featuresDF <- read.csv("/Users/yash/Documents/CS-513/ProjectFiles/FinalProject/Smote_Sampled.csv")
featuresDF$is_attributed <- as.factor(featuresDF$is_attributed)

#store every 5th record in “test” starting with the first, the rest in “training”
testDf <- featuresDF[c(rep(FALSE,4),TRUE), ]
trainingDf <- featuresDF[c(rep(TRUE,4),FALSE), ]



#Random Forest
rf <- randomForest(is_attributed~.,data=trainingDf,importance=TRUE,ntree=200)
importance(rf)
varImpPlot(rf)
prediction("Random Forest", rf, testDf, 29)  #29 is col number of is_attributed

#C4.5 Decision Tree
c45 <- J48(is_attributed~., data=trainingDf)
#summary(c45)
prediction("C4.5", c45, testDf, 29) #29 is col number of is_attributed

################################################################################################

##process rose sampled data
featuresDF <- read.csv("/Users/yash/Documents/CS-513/ProjectFiles/FinalProject/Rose_Sampled_Data.csv")
featuresDF <- featuresDF[,-1]
featuresDF$is_attributed <- as.factor(featuresDF$is_attributed)

#store every 5th record in “test” starting with the first, the rest in “training”
testDf <- featuresDF[c(rep(FALSE,4),TRUE), ]
trainingDf <- featuresDF[c(rep(TRUE,4),FALSE), ]

#Random Forest
rf <- randomForest(is_attributed~.,data=trainingDf,importance=TRUE,ntree=200)
importance(rf)
varImpPlot(rf)
prediction("Random Forest", rf, testDf, 3) #3 is col number of is_attributed

#C4.5 Decision Tree
c45 <- J48(is_attributed~., data=trainingDf)
#summary(c45)
prediction("C4.5", c45, testDf, 3)  #3 is col number of is_attributed

##Neural Network
m <- model.matrix( 
  ~is_attributed+min1_mean+min1_max+min1_var+min5_mean+min5_max+min5_var+hr1_mean+hr1_max+hr1_var+hr3_mean+hr3_max+hr3_var+maxClicks+noOfIPs+clickToIpRatio+clickVariance+entropy+maxClicksDevice+noOfDevices+clickToDeviceRatio+clickVarianceDevice+entropyDevice+maxClicksChannel+noOfChannel+clickToChannelRatio+clickVarianceChannel+entropyChannel, 
  data =trainingDf 
)
net_bc2 <-neuralnet(is_attributed1~min1_mean+min1_max+min1_var+min5_mean+min5_max+min5_var+hr1_mean+hr1_max+hr1_var+hr3_mean+hr3_max+hr3_var+maxClicks+noOfIPs+clickToIpRatio+clickVariance+entropy+maxClicksDevice+noOfDevices+clickToDeviceRatio+clickVarianceDevice+entropyDevice+maxClicksChannel+noOfChannel+clickToChannelRatio+clickVarianceChannel+entropyChannel
                      ,m, hidden=5, threshold=0.01)

#Plot the neural network
plot(net_bc2)

net_bc2_results <-compute(net_bc2, testDf[,c(cols)])
ANN=as.numeric(net_bc2_results$net.result)
ANN_round<-round(ANN)

table(Actual=test_sampled$is_attributed,ANN_round)

wrong<- (testDf$is_attributed!=ANN_round)
rate<-sum(wrong)/length(wrong)
rate_FN<-2/length(wrong)
rate_FN
