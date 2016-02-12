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


barplot(table(train$Happy),
        names.arg = c("Not Happy", "Happy"),
        main="Happy?", col="black")

table(complete.cases(train))
table(complete.cases(test))

best = 0.7882168

bestnames = c("Hardship", "StartTask", "Accomplish", "Over_Head", "Gamble", "Liar", "CCDebt", "Breakfast", "Adventurous", "Optimist", "Live_Alone", "LifeHasPurpose", "Normal", "Grudge", "Positive_Thinking")

#Look for 2-way interactions:
for(i in 1:15) 
{ 
  fmla = as.formula(paste("Happy ~ EducationLevel *",bestnames[i]))
  lreg = lm(fmla, data=train)
  cat(paste(bestnames[i],paste(paste(summary(lreg)$r.squared), "\n")))
}


#Create the regression listing all of the variables
listoffactors <- names(train[c(1:6, 8:108)])
fmla = paste("Happy ~",paste(listoffactors,collapse="+"))
fmla = paste( fmla, '+ EducationLevel * Over_Head + EducationLevel * Optimist + EducationLevel * Normal + EducationLevel * Grudge + Gender*Over_Head + Gender*Optimist + Gender*Accomplish + Gender*LifeHasPurpose + Gender*Grudge + Gender* Normal + Income * Accomplish + Income * Over_Head + Income * Optimist + Income * Grudge + Income * Positive_Thinking + Income * LifeHasPurpose + Party*Over_Head + Party*Optimist + Party*Accomplish +  Party*Grudge + Party* Normal +  HouseholdStatus*Accomplish + HouseholdStatus* Over_Head + HouseholdStatus*Adventurous + HouseholdStatus* Optimist +  HouseholdStatus*Grudge + HouseholdStatus* Positive_Thinking +Over_Head   * Optimist +Over_Head   * Normal +  Over_Head   *  Adventurous + Over_Head * Grudge')

trainGLM = glm(fmla ,  data=train, family=binomial)

#summary(trainGLM)
oldAUC = as.numeric(performance(ROCRpred, "auc")@y.values)
oldAUC
predict_train = predict(trainGLM, type = "response")
ROCRpred = prediction(predict_train, train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values)
as.numeric(performance(ROCRpred, "auc")@y.values)- oldAUC


#MAKE THE SUBMISSION
testPred = predict(trainGLM, newdata = test, type="response")
submission = data.frame(UserID = test$UserID, Probability1 = testPred)
setwd("c:/users/n/skydrive/analytics course/competition/submissions")
write.csv(submission, "submission3.csv", row.names=FALSE) 
setwd("c:/users/n/skydrive/analytics course/competition")
#https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/submissions/attach