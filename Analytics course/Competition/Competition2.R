source ("c:/users/n/Skydrive/documents/research/r/txt/startup.R")
#install.packages("reshape")
library(reshape)
library(rpart)
library(rpart.plot)
library(mice)
library(ROCR)
#set.seed(144)
#vars.for.imputation = setdiff(names(train_data), "Happy")
#imputed = complete(mice(train_data[vars.for.imputation]))
#train_data[vars.for.imputation] = imputed

setwd("c:/users/n/skydrive/analytics course/competition")
train_data = read.csv("train_numeric_small.csv", stringsAsFactors=FALSE)
#test_data = read.csv("test.csv", stringsAsFactors=FALSE)
#table(train_data$Happy,useNA="always")

names(train_data)[names(train_data) == 'Q115899'] = 'Hardship'
names(train_data)[names(train_data) == 'Q120194'] = 'StartTask'
names(train_data)[names(train_data) == 'Q120014'] = 'Successful'
names(train_data)[names(train_data) == 'Q119334'] = 'Accomplish'
names(train_data)[names(train_data) == 'Q118237'] = 'Over_Head'
names(train_data)[names(train_data) == 'Q113992'] = 'Gamble'
names(train_data)[names(train_data) == 'Q106389'] = 'Liar'
names(train_data)[names(train_data) == 'Q102674'] = 'CCDebt'
names(train_data)[names(train_data) == 'Q102687'] = 'Breakfast'
names(train_data)[names(train_data) == 'Q102289'] = 'Adventurous'
names(train_data)[names(train_data) == 'Q101162'] = 'Optimist'
names(train_data)[names(train_data) == 'Q99716'] = 'Live_Alone'
names(train_data)[names(train_data) == 'Q98869'] = 'LifeHasPurpose'


names(test_data)[names(test_data) == 'Q115899'] = 'Hardship'
names(test_data)[names(test_data) == 'Q120194'] = 'StartTask'
names(test_data)[names(test_data) == 'Q120014'] = 'Successful'
names(test_data)[names(test_data) == 'Q119334'] = 'Accomplish'
names(test_data)[names(test_data) == 'Q118237'] = 'Over_Head'
names(test_data)[names(test_data) == 'Q113992'] = 'Gamble'
names(test_data)[names(test_data) == 'Q106389'] = 'Liar'
names(test_data)[names(test_data) == 'Q102674'] = 'CCDebt'
names(test_data)[names(test_data) == 'Q102687'] = 'Breakfast'
names(test_data)[names(test_data) == 'Q102289'] = 'Adventurous'
names(test_data)[names(test_data) == 'Q101162'] = 'Optimist'
names(test_data)[names(test_data) == 'Q99716'] = 'Live_Alone'
names(test_data)[names(test_data) == 'Q98869'] = 'LifeHasPurpose'

#GLM
trainGLM = glm(Happy ~ Gender+HouseholdStatus+Hardship+StartTask+Successful+Accomplish+Over_Head+Gamble
               +Liar+CCDebt+Breakfast+Adventurous+Optimist+Live_Alone+LifeHasPurpose, data=train_data, family=binomial)
predictGLM = predict(trainGLM, type = "response")
ROCRpred = prediction(predictGLM, train_data$Happy)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

predict_test_GLM = predict(trainGLM, type = "response", newdata=test_data)

names(train_data)
#CART
trainCART = rpart(Happy ~ Gender, data=train_data, method="class")
predictCART = predict(trainCART, type = "prob")
str(predictCART)
ROCRpred = prediction(predictCART[,2], train_data$Happy)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

str(predictCART[,2])
predict_test_CART = predict(predictCART[,2], newdata=test_data)


#RF

trainRF = randomForest(Happy ~ ., data=train_data, na.action = na.omit) #Train, ntree=200, nodesize=25 )
predictRF = predict(trainRF, type = "prob")

ROCRpred = prediction(predictRF[,2], train_data$Happy)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

ROCRpred = prediction(predictCART[,2], train_data$Happy)

#MAKE THE SUBMISSION
submission = data.frame(UserID = test_data$UserID, Probability1 = predict_test_GLM)
write.csv(submission, "submission.csv", row.names=FALSE)
'https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/submissions/attach'