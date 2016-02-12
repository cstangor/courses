# Week 4 - "Judge, Jury, and Classifier" Lecture


# VIDEO 4

# Read in the data
setwd("c:/users/n/downloads")
stevens = utils::read.csv("stevens.csv")
str(stevens)

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

#same as above but without the class parameter
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
