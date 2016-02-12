#install.packages("princomp")
library(princomp)
library(ROCR)
library(mice)
library(rpart)
library(rpart.plot)
library(randomForest)

#Read using the "MISS" data
setwd("c:/users/n/skydrive/analytics course/competition")
train = read.csv("train_miss2.csv", stringsAsFactors=FALSE)
train$YOB = NULL #Mas.numeric(train$YOB)
test = read.csv("test_miss.csv", stringsAsFactors=TRUE)
test$YOB = NULL #as.numeric(test$YOB)

names(train)
source("names.R")

table(complete.cases(train))
table(complete.cases(test))

best = .785976


#GLM
trainGLM = glm(Happy ~  . - UserID - votes,  data=train, family=binomial)
#summary(trainGLM)
predict_train = predict(trainGLM, type = "response")
ROCRpred = prediction(predict_train, train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values) - best

#CART
trainCART = rpart(Happy ~ . - UserID - votes, data=train, method="class")
predictCART = predict(trainCART, type = "prob")
ROCRpred = prediction(predictCART[,2], train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values) - best

#RF
trainRF = randomForest(Happy ~ . - UserID - votes, data=train, type = "class", ntree=40, nodesize=30 )
predictRF = predict(trainRF)
ROCRpred = prediction(predictRF, train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values) - best

#MAKE THE SUBMISSION
testPred = predict(trainGLM, newdata = test, type="response")
testPred
submission = data.frame(UserID = test$UserID, Probability1 = testPred)
submission
write.csv(submission, "submission1.csv", row.names=FALSE) 
#https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/submissions/attach