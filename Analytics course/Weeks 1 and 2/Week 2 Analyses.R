
#*********************
setwd("c:/users/n/downloads")
FluTrain= read.csv("FluTrain.csv")
FluTest= read.csv("FluTest.csv")

x = tapply(FluTrain$Queries, FluTrain$Week, mean)
x = tapply(FluTest$ILI, FluTest$Week, mean)

FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
FluTest$logILI = log(FluTest$ILI)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
TestSetStats(FluTest, FluTest$ILI, PredTest1)

finstall.packages("zoo")

library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
FluTrain$logILI = log(FluTrain$ILI)
plot(FluTrain$logILI, FluTrain$ILILag2)

FluTrend2 = lm(logILI ~  log(ILILag2)+Queries, data=FluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
FluTest$logILI = log(FluTest$ILI)
summary(FluTest$ILILag2)

FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] = FluTrain$ILI[nrow(FluTrain)]

FluTrend2 = lm(log(ILI) ~ Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
TestSetStats(FluTest, FluTest$ILI, PredTest2)



#*********************

setwd("c:/users/n/downloads")
pisaTest= read.csv("pisa2009test.csv")
pisaTrain= read.csv("pisa2009train.csv")
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

#the y-hat variable:
predTest = predict(lmScore, newdata=pisaTest)

predTest = predTest

TestSetStats(pisaTest, pisaTest$readingScore, predTest)

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

#Baseline model predicted test score:
mean(predTest)

#
637.6914-353.2231
sort(predTest)
#Reading scores
lmScore = lm(readingScore ~ ., data=pisaTrain)
predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)

# Compute R-squared on the test set
SSE = sum((pisaTest$readingScore - predTest)^2)
SST = sum((pisaTest$readingScore - mean(pisaTest$readingScore))^2)
1 - SSE/SST


