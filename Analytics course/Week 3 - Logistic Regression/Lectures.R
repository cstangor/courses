setwd("c:/users/n/downloads")
quality = read.csv("quality.csv")
install.packages("caTools")
library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)


#QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)

# Make predictions on training set
predictTrain = predict(QualityLog, type="response")

# Analyze predictions
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

#Look at the out of sample data
predictTest = predict(QualityLog, type="response", newdata=qualityTest)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

#Compute some sensitivities and specificities:
t1 = table(qualityTrain$PoorCare, predictTrain > .2)

TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
TP = 20 ; TN = 15; FP = 10; FN = 5
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Sensitivity
Specificity

install.packages("ROCR")
library(ROCR)

# Prediction function
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
