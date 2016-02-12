install.packages("princomp")
library(princomp)
library(ROCR)
library(mice)
library(rpart)
library(rpart.plot)

set.seed(144)
vars.for.imputation = setdiff(names(train_data), "Happy")
imputed = complete(mice(train_data[vars.for.imputation]))
train_data[vars.for.imputation] = imputed


setwd("c:/users/n/skydrive/analytics course/competition")
train_data = read.csv("train.csv", stringsAsFactors=FALSE)
table(train_data$Happy,useNA="always")
names(train_data)
dim(train_data)

#GLM
train_glm = glm(Happy ~ ., data=train_data, family=binomial)
predict_train = predict(train_glm, type = "response")
summary(train_glm)

#CART
trainCART = rpart(Happy ~ ., data=train_data, method="class")
predictCART = predict(trainCART, type = "prob")
predictCART
ROCRpred = prediction(predictCART[,2], train_data$Happy)
?rfImpute
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

names(train_data)
table(train_data$Q96024,useNA="always")

#RF

trainRF = randomForest(Happy ~ ., data=train_data, na.action = na.omit) #Train, ntree=200, nodesize=25 )
predictRF = predict(trainRF, type = "prob")

ROCRpred = prediction(predictRF[,2], train_data$Happy)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc


#MAKE THE SUBMISSION
submission = data.frame(UserID = test$UserID, Probability1 = testPred)
write.csv(submission, "submission.csv", row.names=FALSE) 