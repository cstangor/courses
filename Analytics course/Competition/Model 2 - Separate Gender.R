#library(princomp)
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
source("names.R")

table(complete.cases(train))
table(complete.cases(test))

best = .785976
g1 = subset(train, Gender == "Male")
g2 = subset(train, Gender == "Female")
g3 = subset(train, Gender == "MISS")

listoffactors <- names(g1[3:109])
fmla = as.formula(paste("Happy ~",paste(listoffactors,collapse="+")))

trainGLM1 = glm(fmla,  data=g1, family=binomial)
predict_1 = predict(trainGLM1, type = "response")

trainGLM2 = glm(fmla,  data=g2, family=binomial)
predict_2 = predict(trainGLM2, type = "response")

trainGLM3 = glm(fmla,  data=g3, family=binomial)
predict_3 = predict(trainGLM3, type = "response")

predict_1
ROCRpred1 = prediction(predict_1, g1$Happy)
ROCRpred2 = prediction(predict_2, g2$Happy)
ROCRpred3 = prediction(predict_3, g3$Happy)
#ROCpred = c(ROCRpred1, ROCRpred2, ROCRpred3)
as.numeric(performance(ROCRpred1, "auc")@y.values) - best

#MAKE THE SUBMISSION
dim (test)
g1_test = subset(test, Gender == "Male")
g2_test = subset(test, Gender == "Female")
g3_test = subset(test, Gender == "MISS")

dim (g3_test)
testPred1 = predict(trainGLM1, newdata = g1_test, type="response")
testPred2 = predict(trainGLM1, newdata = g2_test, type="response")
testPred3 = predict(trainGLM1, newdata = g3_test, type="response")
testPred = c(testPred1,testPred2, testPred3)

submission2 = data.frame(UserID = test$UserID, Probability1 = testPred)
submission2a = data.frame(UserID = g1_test$UserID, Probability1 = testPred1)
submission2b = data.frame(UserID = g2_test$UserID, Probability1 = testPred2)
submission2c = data.frame(UserID = g3_test$UserID, Probability1 = testPred3)
submission2 = rbind(submission2a, submission2b, submission2c)
submission2
write.csv(submission2, "submission2.csv", row.names=FALSE) 
#https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/submissions/attach

submission = read.csv("test.csv", header = FALSE)

for(i in 3:109) 
{ 
  CrossTable(g1[[i]], g1$Gender, prop.t = FALSE, prop.r = TRUE, prop.c = FALSE, prop.chisq = FALSE, format = "SAS", chisq=TRUE,dnn = c(names(g1)[i], ""))
}
