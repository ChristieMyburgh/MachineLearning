library(data.table)
library(caret)
library(corrplot)


traindata <- read.csv("N:/R_Directory/ML/Assignment/MachineLearning/pml-training.csv",
                        sep = ",", header = TRUE)

testdata <- read.csv("N:/R_Directory/ML/Assignment/MachineLearning/pml-testing.csv",
                       sep=",", header=TRUE)


trainPredictors <- traindata[,7:159]

testPredictors <- testdata[,7:159]

trainPredictors <- suppressWarnings(data.frame(sapply(trainPredictors, as.numeric)))

testPredictors <- suppressWarnings(data.frame(sapply(testPredictors, as.numeric)))

trainPredictors[is.na(trainPredictors)] <- 0  ## Change NA's to 0

testPredictors[is.na(testPredictors)] <- 0


#Removing Predictors
zeroIdx <- nearZeroVar(trainPredictors)

trainPredictors <- trainPredictors[ ,-zeroIdx]

zeroIdx <- nearZeroVar(testPredictors)

testPredictors <- testPredictors[,-zeroIdx]

#users <- data.frame(user=unique(traindata$user_name), id=1:6)

#for(i in 1:6) {
#  trainPredictors$user[traindata[,2] == users[i,1]] <- users[i,2]
#}

#for(i in 1:6) {
#  testPredictors$user[testdata[,2] == users[i,1]] <- users[i,2]
#}

set.seed(1234)

#Creating my own test and training set data
#CrossValidation
newTrainSet <- createDataPartition(y=traindata$classe, p=0.75, list=FALSE)

newTrain <- trainPredictors[newTrainSet,]

newTest <- trainPredictors[-newTrainSet,]

fact_class_train <- as.factor(traindata$classe[newTrainSet])

fact_class_test <- as.factor(traindata$classe[-newTrainSet])


#Check for corellated variables.
correlations <- cor(newTrain)

library(corrplot)

corrplot(correlations, order="hclust")

highCorr <- findCorrelation(correlations, cutoff=0.75)

newTrain_decorr <- newTrain[ , -highCorr]

newTest_decorr <- newTest[ , -highCorr]

testPredictors <- testPredictors[ ,-highCorr]

tt <- cor(newTrain_decorr)

corrplot(tt)

rf1 <- train(fact_class_train ~., data=newTrain_decorr, method="rf")

rf1 <- train(fact_class_train ~., data=newTrain_decorr, method="rf",
             trControl=trainControl(method="repeatedcv", number=10, repeats=3))

rf1_pred <- predict(rf1, newTest_decorr)

confusionMatrix(rf1_pred, fact_class_test)

test1 <- predict(rf1, testPredictors[1,])
test2 <- predict(rf1, testPredictors[2,])
test3 <- predict(rf1, testPredictors[3,])
test4 <- predict(rf1, testPredictors[4,])
test5 <- predict(rf1, testPredictors[5,])
test6 <- predict(rf1, testPredictors[6,])
test7 <- predict(rf1, testPredictors[7,])
test8 <- predict(rf1, testPredictors[8,])
test9 <- predict(rf1, testPredictors[9,])
test10 <- predict(rf1, testPredictors[10,])
test11 <- predict(rf1, testPredictors[11,])
test12 <- predict(rf1, testPredictors[12,])
test13 <- predict(rf1, testPredictors[13,])
test14 <- predict(rf1, testPredictors[14,])
test15 <- predict(rf1, testPredictors[15,])
test16 <- predict(rf1, testPredictors[16,])
test17 <- predict(rf1, testPredictors[17,])
test18 <- predict(rf1, testPredictors[18,])
test19 <- predict(rf1, testPredictors[19,])
test20 <- predict(rf1, testPredictors[20,])

predict(rf1, testPredictors[1,])
predict(rf1, testPredictors[2,])
predict(rf1, testPredictors[3,])
predict(rf1, testPredictors[4,])
predict(rf1, testPredictors[5,])
predict(rf1, testPredictors[6,])
predict(rf1, testPredictors[7,])
predict(rf1, testPredictors[8,])
predict(rf1, testPredictors[9,])
predict(rf1, testPredictors[10,])
predict(rf1, testPredictors[11,])
predict(rf1, testPredictors[12,])
predict(rf1, testPredictors[13,])
predict(rf1, testPredictors[14,])
predict(rf1, testPredictors[15,])
predict(rf1, testPredictors[16,])
predict(rf1, testPredictors[17,])
predict(rf1, testPredictors[18,])
predict(rf1, testPredictors[19,])
predict(rf1, testPredictors[20,])
