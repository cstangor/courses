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

best = 0.7882168

bestnames = c("Hardship", "StartTask", "Accomplish", "Over_Head", "Gamble", "Liar", "CCDebt", "Breakfast", "Adventurous", "Optimist", "Live_Alone", "LifeHasPurpose", "Normal", "Grudge", "Positive_Thinking")

names(train)
#Look for 2-way interactions:
for(i in 1:15){
  for (j in 1:15)
{ 
  fmla  = concat(c("Happy ~", bestnames[i], "*", bestnames[j]))
  lreg = lm(fmla, data=train)
  cat (concat(c(bestnames[i], ",", bestnames[j], "," ,summary(lreg)$r.squared, "\n")))
  }
}

#Create the regression listing all of the variables
listoffactors <- names(train[c(1:6, 8:108)])
listoffactors
fmla = paste("Happy ~",paste(listoffactors,collapse="+"))
fmla = paste( fmla, '+ Gender*Over_Head + Gender*Optimist + Gender*Accomplish + Gender*LifeHasPurpose + Gender*Grudge + Gender* Normal + Income * Accomplish + Income * Over_Head + Income * Optimist + Income * Grudge + Income * Positive_Thinking + Income * LifeHasPurpose + Party*Over_Head + Party*Optimist + Party*Accomplish +  Party*Grudge + Party* Normal +  HouseholdStatus*Accomplish + HouseholdStatus* Over_Head + HouseholdStatus*Adventurous + HouseholdStatus* Optimist +  HouseholdStatus*Grudge + HouseholdStatus* Positive_Thinking')
#fmla
trainGLM = glm(fmla ,  data=train, family=binomial)

#summary(trainGLM)
as.numeric(performance(ROCRpred, "auc")@y.values)
as.numeric(performance(ROCRpred, "auc")@y.values)- best
predict_train = predict(trainGLM, type = "response")
ROCRpred = prediction(predict_train, train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values)
as.numeric(performance(ROCRpred, "auc")@y.values)- best


#MAKE THE SUBMISSION
testPred = predict(trainGLM, newdata = test, type="response")
submission = data.frame(UserID = test$UserID, Probability1 = testPred)
submission
write.csv(submission, "submission.csv", row.names=FALSE) 
#https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/submissions/attach