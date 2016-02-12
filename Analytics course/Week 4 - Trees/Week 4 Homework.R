setwd("c:/users/n/downloads")
census = utils::read.csv("census.csv")

#Split the data 60/40
set.seed(2000)
library(caTools)
split = sample.split(census$over50k, SplitRatio = 0.6)
censustrain = subset(census, split == TRUE)
censustest = subset(census, split == FALSE)

#Run the intial binomial model
censusLog = glm(over50k ~ ., data=censustrain, family=binomial)
summary(censusLog)

#Create the y-hat variable:
predTest = predict(censusLog, newdata=censustest)

#Get the accuracy of the test t1 = table(censustest$over50k, predTest > .5)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Sensitivity
Specificity
Accuracy

#Baseline accuracy is 1- mean of the binary DV
summary(as.numeric(censustest$over50k)

#Use the ROC package to look at the "Area under the Curve"
library(ROCR)
ROCRpred = prediction(predTest, censustest$over50k)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#Make a Treemodel
CART = rpart(over50k ~ ., data=censustrain, method="class")
summary(CART)

rpart.plot::prp(CART)
table(censustrain$education)

#Get the prediction of the binomial model
predTest = predict(CART, newdata=censustest, type = "class")

#Get the accuracy of the test 
t1 = table(censustest$over50k, predTest > .5)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Sensitivity
Specificity
Accuracy

#Get the prediction of the binomial model
CART = rpart(over50k ~ ., data=censustrain, method="class")
predTest
#Use prob method for the accuracy
predTest = predict(CART, newdata=censustest, type = "prob")
ROCRpred = prediction(predTest[,2], censustest$over50k)

auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#GEt a small sample for random forest training 
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]


# Build random forest model
library(randomForest)
censusForest = randomForest(over50k ~ . - nativecountry, data=censustrain) #Train, ntree=200, nodesize=25 )

# Make predictions
set.seed(1000)
PredictForest = predict(censusForest, newdata = censustest)

#Get the accuracy of the test t1 = table(censustest$over50k, predTest > .5)
t1 = table(censustest$over50k, PredictForest)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Accuracy

#Impurity
varImpPlot(censusForest)

set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

train(over50k ~ ., data=censustrain, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

# Train a new CART model
CensusTree = rpart(over50k ~ ., data=censustrain, method="class", control=rpart.control(cp = 0.002))

#Get the prediction of the binomial model
predTest = predict(CensusTree, newdata=censustest, type = "class")

#Get the accuracy of the test 
t1 = table(censustest$over50k, predTest)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Accuracy

rpart.plot::prp(CensusTree)

##################################
data(state)
statedata = data.frame(state.x77)

stateglm = lm(Life.Exp ~ ., data = statedata )
summary(stateglm)

#the y-hat variable:
predTest = predict(stateglm)
TestSetStats(statedata, statedata$Life.Exp, predTest)

stateglm = lm(Life.Exp ~  Population+ Murder+ Frost + HS.Grad, data = statedata )
summary(stateglm)

#the y-hat variable:
predTest = predict(stateglm)
TestSetStats(statedata, statedata$Life.Exp, predTest)

TestSetStats = function  (testset, testsetDV, predTest)
{
  SSE = sum((testsetDV - predTest)^2)
  SST = sum((testsetDV - mean(predTest))^2)
  RMSE = sqrt(SSE/nrow(testset))
  print (paste("SSE ", SSE))
  print (paste("SST ", SST))
  print (paste("RMSE ", RMSE))
  print (paste("R-squared ", 1 - SSE/SST))
}


CART = rpart(Life.Exp ~ ., data = statedata )
summary(CART)

rpart.plot::prp(CART)

predTest = predict(CART)
TestSetStats(statedata, statedata$Life.Exp, predTest)

CART2 = rpart(Life.Exp ~ ., data = statedata, control=rpart.control(minbucket=5) )
rpart.plot::prp(CART2)
predTest = predict(CART2)
TestSetStats(statedata, statedata$Life.Exp, predTest)

CART3 = rpart(Life.Exp ~ Area, data = statedata, control=rpart.control(minbucket=1) )
rpart.plot::prp(CART3)
predTest = predict(CART3)
TestSetStats(statedata, statedata$Life.Exp, predTest)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
set.seed(111)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01) 

# Perform the cross validation
train(Life.Exp ~ ., data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

# Create a new CART model
CART4 = rpart(Life.Exp ~ ., data = statedata, control=rpart.control(cp = 0.12))
rpart.plot::prp(CART4, left=FALSE)
predTest = predict(CART4)
TestSetStats(statedata, statedata$Life.Exp, predTest)

#Back to the model with Area only
set.seed(111)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01) 
train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )
CART3 = rpart(Life.Exp ~ Area, data = statedata, control=rpart.control(cp = 0.02))
prp(CART3)

predTest = predict(CART3)
TestSetStats(statedata, statedata$Life.Exp, predTest)


#####################
setwd("c:/users/n/downloads")
letters = utils::read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
library(caTools)

split = sample.split(letters$isB, SplitRatio = 0.5)
letterstrain = subset(letters, split == TRUE)
letterstest = subset(letters, split == FALSE)
summary(letterstest$isB)
1175/(1175+383)

CARTb = rpart(isB ~ . - letter, data=letterstrain, method="class")
summary(CARTb)

# Make binomial predictions on the data
CARTbPredict = predict(CARTb,  newdata=letterstest, type="class")
CARTbPredict

t1 = table(letterstest$isB, CARTbPredict )
t1
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Sensitivity
Specificity
Accuracy

library(randomForest)

# Build random forest model
lettersForest = randomForest(isB ~ . - letter, data=letterstrain) #Train, ntree=200, nodesize=25 )

# Make predictions
set.seed(1000)
PredictForest = predict(lettersForest, newdata = letterstest)
t1 = table(letterstest$isB, PredictForest)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Accuracy

#Now do it again predicting letter rather than isB
setwd("c:/users/n/downloads")
letters = utils::read.csv("letters_ABPR.csv")
letters$letter = as.factor( letters$letter )

set.seed(2000)
library(caTools)

split = sample.split(letters$letter, SplitRatio = 0.5)
letterstrain = subset(letters, split == TRUE)
letterstest = subset(letters, split == FALSE)
table(letterstest$letter)

401/(395+383+401+379)

CARTb = rpart(letter ~ . - letter, data=letterstrain, method="class")
summary(CARTb)

# Make binomial predictions on the data
CARTbPredict = predict(CARTb,  newdata=letterstest, type="class")
CARTbPredict

t1 = table(letterstest$letter, CARTbPredict )
t1
Accuracy = (t1[1,1] + t1[2,2] + t1[3,3] + t1[4,4]) /nrow(letterstest)
Accuracy

library(randomForest)

# Build random forest model
lettersForest = randomForest(letter ~ . , data=letterstrain) #Train, ntree=200, nodesize=25 )

# Make predictions
set.seed(2000)
PredictForest = predict(lettersForest, newdata = letterstest)
t1 = table(letterstest$letter, PredictForest)
Accuracy = (t1[1,1] + t1[2,2] + t1[3,3] + t1[4,4]) /nrow(letterstest)
Accuracy


#########################
setwd("c:/users/n/downloads")
gerber = utils::read.csv("gerber.csv")
str(gerber)
summary (gerber)
names(gerber)
t1 = table(gerber$voting, gerber$hawthorne)
t1[2,2]/(t1[1,2] + t1[2,2])

t1 = table(gerber$voting, gerber$civicduty)
t1[2,2]/(t1[1,2] + t1[2,2])

t1 = table(gerber$voting, gerber$neighbors)
t1[2,2]/(t1[1,2] + t1[2,2])

t1 = table(gerber$voting, gerber$self)
t1[2,2]/(t1[1,2] + t1[2,2])

table(gerber$voting)
str(t1)
t1[2]/(t1[1]+t1[2])
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]

#Calculate the binomial regression
GerberLog = glm(voting ~ civicduty +hawthorne +  self + neighbors, data=gerber, family=binomial)
summary(GerberLog)

# Make binomial predictions on the data
GerberPredict = predict(GerberLog, type="response")

#compare the actual and predicted
t1 = table(gerber$voting, GerberPredict > .5)
t1
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Sensitivity
Specificity
Accuracy

#Use the ROC package to look at the "Area under the Curve"
library(ROCR)
ROCRpred = prediction(GerberPredict, gerber$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model

GerberTree = rpart::rpart(voting ~ civicduty +hawthorne +  self + neighbors, data=gerber, method="class",
    control=rpart.control(minbucket=0.0))
rpart.plot::prp(GerberTree2)

GerberTree2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
#draw the plot:
rpart.plot::prp(GerberTree2)

GerberTree3 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
#draw the plot:
rpart.plot::prp(GerberTree3)

ControlTree = rpart(voting ~ control, data=gerber, cp=0.0)
rpart.plot::prp(ControlTree, digits=6)
.296638-.34

SexControlTree= rpart(voting ~ sex + control, data=gerber, cp=0.0)
rpart.plot::prp(SexControlTree, digits=6)

# Make predictions using the class model 
PredictCART = stats::predict(StevensTree, newdata = Test, type = "class")
base::table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)


#Calculate the binomial regression
LogModel1 = glm(voting ~ sex + control, data=gerber, family=binomial)
LogModel1
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModel1, newdata=Possibilities, type="response")
.290456- 0.2904558
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family=binomial)
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")

# Split the data
library(caTools)
set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, split==TRUE)
Test = subset(stevens, split==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart::rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, method="class", data = Train, control=rpart.control(minbucket=100))
#draw the plot:
rpart.plot::prp(StevensTree)

# Make predictions using the class model 
PredictCART = stats::predict(StevensTree, newdata = Test, type = "class")
base::table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)

# ROC curve
library(ROCR)

#same as above but without the class parameter?
PredictROC = stats::predict(StevensTree, newdata = Test)
PredictROC

pred = ROCR::prediction(PredictROC[,2], Test$Reverse)
perf = ROCR::performance(pred, "tpr", "fpr")
plot(perf)

auc = as.numeric(performance(pred, "auc")@y.values)
auc


# VIDEO 5 - Random Forests

# Install randomForest package
install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )

# Make predictions
set.seed(200)
PredictForest = predict(StevensForest, newdata = Test)
t1 = table(Test$Reverse, PredictForest)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Accuracy


# VIDEO 6

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, method="class", data = Train, control=rpart.control(cp = 0.18))

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)

rpart.plot::prp(StevensTreeCV)
prp
