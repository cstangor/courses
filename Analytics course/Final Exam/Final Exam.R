setwd("c:/users/n/skydrive/analytics course/Final Exam")
nytimes = read.csv("nytimes.csv", stringsAsFactors = FALSE)
#table(nytimes$popular)
#105/(105+868)

#cor(nytimes$popular, nchar(nytimes$headline))

nytimes$popular = as.factor(nytimes$popular)
nytimes$type = as.factor(nytimes$type)

nytimesGLM = glm(popular ~ print+type+word.count, data=nytimes, family=binomial)
summary(nytimesGLM)

library(rpart)
library(randomForest)
library(ROCR)
CART = rpart(popular ~ print+type+word.count, data=nytimes, method="class")
summary(CART)

# Build random forest model
library(randomForest)
nytimesForest = randomForest(popular ~ . , data=nytimes) #Train, ntree=200, nodesize=25 )

#Split the data 60/40
set.seed(2000)
library(caTools)
split = sample.split(census$over50k, SplitRatio = 0.6)
censustrain = subset(census, split == TRUE)
censustest = subset(census, split == FALSE)
library(ROCR)
CART = rpart(over50k ~ ., data=censustrain, method="class")
summary(CART)

??CART
######################
setwd("c:/users/n/skydrive/analytics course/Final Exam")

elantra = read.csv("elantra.csv")
dim(elantra)
names(elantra)
table(elantra$Year)
train = subset(elantra, Year <= 2012)
test = subset(elantra, Year > 2012)
dim (train)
dim (test)
m1 = lm(data=train, ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries)
summary(m1)

m1 = lm(data=train, ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries)
summary(m1)
2*110.69
4*110.69

train$MonthFac = as.factor(train$Month)
test$MonthFac = as.factor(test$Month)
m1 = lm(data=train, ElantraSales ~ MonthFac + Unemployment + CPI_all + CPI_energy + Queries)
summary(m1)

myvars <- train[,c(setdiff(names(train), "MonthFac"))]
cor(myvars, use="complete.obs", method = "pearson")

m1 = lm(data=train, ElantraSales ~ MonthFac + Unemployment + CPI_all + CPI_energy + Queries)
summary(m1)

m1 = lm(data=train, ElantraSales ~ MonthFac + Unemployment + CPI_all + CPI_energy )
summary(m1)

predictTest = predict(m1, newdata=test)
predictTest

# Compute R-squared
SSE = sum((test$ElantraSales - predictTest)^2)
SST = sum((test$ElantraSales - mean(test$ElantraSales))^2)
1 - SSE/SST

sort(abs(test$ElantraSales - mean(test$ElantraSales)))

mean(train$ElantraSales)
