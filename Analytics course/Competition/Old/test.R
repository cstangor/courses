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
str(trainGLM)
predict_train = predict(trainGLM, type = "response")
ROCRpred = prediction(predict_train, train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values) - best


#MAKE THE SUBMISSION
testPred = predict(trainGLM, newdata = test, type="response")
testPred
submission1 = data.frame(UserID = test$UserID, Probability1 = testPred)
submission1
write.csv(submission1, "submission1.csv", row.names=FALSE) 
#https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/submissions/attach