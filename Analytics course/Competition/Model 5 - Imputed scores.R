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

setwd("c:/users/n/skydrive/analytics course/competition")
train = read.csv("train.csv", stringsAsFactors=FALSE, na.strings = c("", "NA"))
test = read.csv("test.csv", stringsAsFactors=FALSE, na.strings = c("", "NA"))

for (i in 9:109) #3:7) #;9:109)
  {t = table(train[i])
  for (j in 1:length(t))
    {train[[i]] = ifelse(train[[i]]==names(t)[j], t(j), train[[i]])
  }
  train[[i]] = as.numeric(train[[i]])
}


for (i in 3:109)
{t = table(test[i])
 for (j in 1:length(t))
 {test[[i]] = ifelse(test[[i]]==names(t)[j], t(j), test[[i]])
 }
 test[[i]] = as.numeric(test[[i]])
}

source("names.R")

#Impute missing values
install.packages("mice")
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(train), "Happy")
imputed = complete(mice(train[vars.for.imputation]))
train[vars.for.imputation] = imputed

set.seed(144)
vars.for.imputation = setdiff(names(test), "Happy")
imputed = complete(mice(test[vars.for.imputation]))
test[vars.for.imputation] = imputed
save.image()
table(complete.cases(train))
table(complete.cases(test))

write.csv(train, "train.imputed.csv")
write.csv(test, "test.imputed.csv")

trainGLM = glm(Happy ~. -votes ,  data=train, family=binomial)

summary(trainGLM)
oldAUC = as.numeric(performance(ROCRpred, "auc")@y.values)
predict_train = predict(trainGLM, type = "response")

ROCRpred = prediction(predict_train, train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values)
as.numeric(performance(ROCRpred, "auc")@y.values)- oldAUC

names(train)
names(test)
warnings()
?predict
#MAKE THE SUBMISSION
testPred = predict(trainGLM, newdata = test, type = "response")
testPred
submission = data.frame(UserID = test$UserID, Probability1 = testPred)
setwd("c:/users/n/skydrive/analytics course/competition/submissions")
write.csv(submission, "submission5.csv", row.names=FALSE) 
setwd("c:/users/n/skydrive/analytics course/competition")


#Create the regression listing all of the variables
listoffactors <- names(train[c(1:6, 8:108)])
fmla = paste("Happy ~",paste(listoffactors,collapse="+"))
fmla = paste( fmla, '+ EducationLevel * Over_Head + EducationLevel * Optimist + EducationLevel * Normal + EducationLevel * Grudge + Gender*Over_Head + Gender*Optimist + Gender*Accomplish + Gender*LifeHasPurpose + Gender*Grudge + Gender* Normal + Income * Accomplish + Income * Over_Head + Income * Optimist + Income * Grudge + Income * Positive_Thinking + Income * LifeHasPurpose + Party*Over_Head + Party*Optimist + Party*Accomplish +  Party*Grudge + Party* Normal +  HouseholdStatus*Accomplish + HouseholdStatus* Over_Head + HouseholdStatus*Adventurous + HouseholdStatus* Optimist +  HouseholdStatus*Grudge + HouseholdStatus* Positive_Thinking +Over_Head   * Optimist +Over_Head   * Normal +  Over_Head   *  Adventurous + Over_Head * Grudge')

trainGLM = glm(fmla ,  data=train, family=binomial)
summary(trainGLM)
predict_train = predict(trainGLM, type = "response")

ROCRpred = prediction(predict_train, train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values)
as.numeric(performance(ROCRpred, "auc")@y.values)- oldAUC

names(train)
names(test)
warnings()
?predict
#MAKE THE SUBMISSION
testPred = predict(trainGLM, newdata = test, type = "response")
testPred
submission = data.frame(UserID = test$UserID, Probability1 = testPred)
setwd("c:/users/n/skydrive/analytics course/competition/submissions")
write.csv(submission, "submission5.csv", row.names=FALSE) 
setwd("c:/users/n/skydrive/analytics course/competition")

###CART

set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
fitControl = trainControl( method = "cv", number = 10 )

#train(Happy ~ ., data=train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

bestnames = c("Hardship", "StartTask", "Accomplish", "Over_Head", "Gamble", "Liar", "CCDebt", "Breakfast", "Adventurous", "Optimist", "Live_Alone", "LifeHasPurpose", "Normal", "Grudge", "Positive_Thinking")

#CART
listoffactors = bestnames
fmla = paste("Happy ~",paste(listoffactors,collapse="+"))
#Smaller cp = more accuracy but perhaps less generality?
trainCART = rpart(Happy ~. -votes , data=train, method="class", control=rpart.control(cp = 0.0001))
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
write.csv(submission, "submission6.csv", row.names=FALSE) 
setwd("c:/users/n/skydrive/analytics course/competition")

#RF
trainRF = randomForest(Happy ~ . - UserID - votes, data=train, type = "class", ntree=40, nodesize=10 )
predictRF = predict(trainRF, type = "prob")
ROCRpred = prediction(predictRF[,2], train$Happy)
as.numeric(performance(ROCRpred, "auc")@y.values) 

