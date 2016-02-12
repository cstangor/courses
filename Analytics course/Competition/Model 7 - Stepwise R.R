#Read using the "MISS" data
setwd("c:/users/n/skydrive/analytics course/competition")
train = read.csv("train_miss2.csv", stringsAsFactors=FALSE)
train$YOB = as.numeric(train$YOB)
test = read.csv("test_miss.csv", stringsAsFactors=TRUE)
test$YOB = as.numeric(test$YOB)
source("names.R")
test$YOB

# Stepwise Regression
library(MASS)
fit <- glm(Happy ~. , data = train, family = binomial )
step <- stepAIC(fit, direction="backward")
step$anova # display results

train
trainGLM = glm(Happy ~ YOB + Gender + HouseholdStatus + Party + Q122769 + Q122770 + 
    Q121700 + Q121011 + Q120194 + Q120012 + Q120014 + Q119334 + 
    Q119650 + Q118237 + Q116797 + Q116953 + Q116441 + Q116197 + 
    Q115610 + Q115611 + Q115899 + Q114961 + Q113992 + Q113583 + 
    Q113584 + Q109367 + Q108855 + Q108617 + Q108754 + Q108342 + 
    Q108343 + Q107869 + Q106388 + Q106389 + Q102906 + Q102674 + 
    Q102687 + Q102289 + Q102089 + Q101162 + Q100680 + Q100562 + 
    Q99982 + Q99716 + Q98869 + Q98197, data=train, family=binomial)

train$Happy
predict_train
predict_train = predict(trainGLM, type = "response")
ROCRpred = prediction(predict_train, train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values)
#MAKE THE SUBMISSION

testPred = predict(trainGLM, newdata = test, type="response")
submission = data.frame(UserID = test$UserID, Probability1 = testPred)
setwd("c:/users/n/skydrive/analytics course/competition/submissions")
write.csv(submission, "submission6.csv", row.names=FALSE) 
setwd("c:/users/n/skydrive/analytics course/competition")
#https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/submissions/attach