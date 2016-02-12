setwd("c:/users/n/skydrive/analytics course/Week 3 - Logistic Regression")
loans = read.csv("loans.csv")

split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loanstrain = subset(loans, split == TRUE)
loanstest = subset(loans, split == FALSE)

loanspred = glm(not.fully.paid ~ ., data=loanstrain, family=binomial)
predicted.risk= predict(loanspred, type="response", newdata=loanstest)
ROCRpred = prediction(predicted.risk, loanstest$not.fully.paid)
str(predicted.risk)
dim(loanstrain)


setwd("c:/users/n/skydrive/analytics course/competition")
train_data = read.csv("train.csv", stringsAsFactors=FALSE)
test_data = read.csv("test.csv", stringsAsFactors=FALSE)
names(test_data)
colnames(test_data) = make.names(colnames(test_data))
colnames(train_data) = make.names(colnames(train_data))
str(loanstrain)
str(train_data)

names = c(train_data$YOB, train_data$Gender)
name1 = "Gender"
trainGLM = glm(Happy ~ Gender , data=train_data, family=binomial)

predictGLM = predict(trainGLM, type = "response", newdata=test_data)
predictGLM
str(predictGLM)
dim(train_data)
ROCRpred = prediction(predictGLM, test_data$Happy)
test_data$Happy
names(train_data)
