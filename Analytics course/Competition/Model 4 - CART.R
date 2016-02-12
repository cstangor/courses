#install.packages("C50")
#install.packages("princomp")
library(princomp)
library(ROCR)
library(mice)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)
library (C50)

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

set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
fitControl = trainControl( method = "cv", number = 10 )

#train(Happy ~ ., data=train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

bestnames = c("Hardship", "StartTask", "Accomplish", "Over_Head", "Gamble", "Liar", "CCDebt", "Breakfast", "Adventurous", "Optimist", "Live_Alone", "LifeHasPurpose", "Normal", "Grudge", "Positive_Thinking")

#CART
listoffactors = bestnames
fmla = paste("Happy ~",paste(listoffactors,collapse="+"))
#Smaller cp = more accuracy but perhaps less generality?
trainCART = rpart(fmla, data=train, method="class", control=rpart.control(cp = 0.0001))
predictCART = predict(trainCART, type = "prob")
ROCRpred = prediction(predictCART[,2], train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values) 
as.numeric(performance(ROCRpred, "auc")@y.values) - best

predictCART
#MAKE THE SUBMISSION
testPred = predict(trainCART, newdata = test, type="prob")
testPred[,2]
submission = data.frame(UserID = test$UserID, Probability1 = testPred[,2])
submission
setwd("c:/users/n/skydrive/analytics course/competition/submissions")
write.csv(submission, "submission4.csv", row.names=FALSE) 
setwd("c:/users/n/skydrive/analytics course/competition")




#Boosting with C50
#But it created a binary outcome
train$Happy = as.factor(train$Happy)
#x  = C5.0(fmla, data=train, rules=TRUE)
x  = C5.0(x = train, y = train$Happy, rules=TRUE)
grid <- expand.grid(.model = "tree",
                    .trials = c(1:100),
                    .winnow = FALSE)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE)

c5Tune <- train(train, train$Happy,
                method = "C5.0",
                metric = "ROC",
                tuneGrid = grid,
                trControl = cvCtrl)

c5Pred <- predict(c5Tune, train)
c5Pred
ROCRpred = prediction(c5Pred, train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values) 
as.numeric(performance(ROCRpred, "auc")@y.values) - best
fmla

#https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/submissions/attach