setwd("c:/users/n/downloads")
loans = read.csv("loans.csv")
str(loans)
table(loans$not.fully.paid)
summary(loans)
loans1 = subset(loans, is.na(pub.rec) )
summary(loans$not.fully.paid)                
summary(loans1$not.fully.paid)                

#Impute missing values
install.packages("mice")
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loanstrain = subset(loans, split == TRUE)
loanstest = subset(loans, split == FALSE)

loanspred = glm(not.fully.paid ~ ., data=loanstrain, family=binomial)
summary(loanspred)

predicted.risk= predict(loanspred, type="response", newdata=loanstest)
predicted.risk

t1 = table(loanstest$not.fully.paid, predicted.risk > .5)
t1

TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Sensitivity
Specificity
Accuracy

#Baseline model:
table(loanstest$not.fully.paid)
460/(460+2413)
1-.1601114

#Log odds change: coefficent * amount of change
#Odds change: exp(coefficent * amount of change)
exp(-9.288e-03*(700-710))

#Didn't use this, but: exp(A + B + C) = exp(A)*exp(B)*exp(C).

#Area above curve:
#Use prediction from test set:
library(ROCR)
ROCRpred = prediction(predicted.risk, loanstest$not.fully.paid)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#A bivariate model:
loanspred.biv = glm(not.fully.paid~ int.rate, data=loanstrain, family=binomial)
summary(loanspred.biv)

predicted.risk.biv = predict(loanspred.biv, type="response", newdata=loanstest)
sort(predicted.risk.biv)

t1 = table(loanstest$not.fully.paid, predicted.risk.biv > .5)
t1

ROCRpred = prediction(predicted.risk.biv, loanstest$not.fully.paid)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

dim(loanstest)

loanstest$profit = exp(loanstest$int.rate*3) - 1
loanstest$profit[loanstest$not.fully.paid == 1] = -1
sort(loanstest$profit)

highInterest= subset(loanstest, int.rate > .15)
dim(highInterest)
summary(highInterest$profit)
table(highInterest$profit >-1)

predicted.risk.biv = predict(loanspred.biv, type="response", newdata=loanstest)
sort(predicted.risk.biv)

names(highInterest)
highInterest$predicted.risk
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

cutoff
110/(110+327)

#****************

parole = read.csv("parole.csv")
str(parole)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
table(parole$violator)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
paroletrain = subset(parole, split == TRUE)
paroletest = subset(parole, split == FALSE)

testtest = paroletest[1,]
testtest = edit (testtest)
testtest

parolepred = glm(violator ~ ., data=paroletrain, family=binomial)
summary(parolepred)
ParoleTestTestPred= predict(parolepred, type="response", newdata=testtest)
ParoleTestTestPred
exp(.154383)
1/(1+exp(-1.166938))
lstr(ParoleTestTestPred)

ParoleTestPred= predict(parolepred, type="response", newdata=paroletest)

#Accuracy of the baseline model:
table(paroletest$violator)
1-.1139
t1 = table(paroletest$violator, ParoleTestPred > .2)
t1
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Sensitivity
Specificity
Accuracy

#Area above curve:
#Use prediction from test set:
ROCRpred = prediction(ParoleTestPred, paroletest$violator)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc


#****************
PlayoffTable=table(baseball$Year)
str(PlayoffTable)
names(PlayoffTable)
baseball=subset(baseball, Playoffs==1)
dim(baseball)
summary(parole$state)
summary(parole$race)
PlayoffTable[c("1990", "2001")]
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
table(baseball$WorldSeries)
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)

baseballpred = glm(WorldSeries ~ Year, data=baseball, family=binomial)
summary(baseballpred)

baseballpred = glm(WorldSeries ~ RA, data=baseball, family=binomial)
summary(baseballpred)
baseballpred = glm(WorldSeries ~ RankSeason , data=baseball, family=binomial)
summary(baseballpred)
baseballpred = glm(WorldSeries ~ NumCompetitors, data=baseball, family=binomial)
summary(baseballpred)
baseballpred = glm(WorldSeries ~ Year+ RA, data=baseball, family=binomial)
summary(baseballpred)
baseballpred = glm(WorldSeries ~ Year+ RankSeason, data=baseball, family=binomial)
summary(baseballpred)
baseballpred = glm(WorldSeries ~ Year+  NumCompetitors, data=baseball, family=binomial)
summary(baseballpred)
baseballpred = glm(WorldSeries ~ RA +RankSeason , data=baseball, family=binomial)
summary(baseballpred)
baseballpred = glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family=binomial)
summary(baseballpred)
baseballpred = glm(WorldSeries ~ RankSeason + NumCompetitors, data=baseball, family=binomial)
summary(baseballpred)

cor(baseball$Year,baseball$RA )
cor(baseball$Year,baseball$RankSeason )
cor(baseball$Year,baseball$NumCompetitors) 
cor(baseball$RA,baseball$RankSeason )
cor(baseball$RA,baseball$NumCompetitors )
cor(baseball$RankSeason,baseball$NumCompetitors)


names(songs)
table(songs$timesignature)
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)

SongsTrain = songs[songs$year<=2009,]
SongsTest = songs[songs$year ==2010,]

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

summary(SongsLog3)

# Make predictions on training set
predictSongTrain = predict(SongsLog3, type="response")

# Analyze predictions
summary(predictSongTrain)
tapply(predictSongTrain, SongsTrain$Top10, mean)

#Look at the out of sample data
predictSongTest = predict(SongsLog3, type="response", newdata=SongsTest)

t1 = table(SongsTest$Top10, predictSongTest > .45)
t1
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Sensitivity
Specificity
table(SongsTest$Top10)

library(ROCR)
ROCRpredSongTest = prediction(predictSongTest, SongsTest$Top10)
ROCRpredSongTest
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
cor(songs$loudness, songs$energy)
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
