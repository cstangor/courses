a = c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
b = c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)
sum((a-b)^2)
2^.5
sum(a-b)
sum(a)
# Read in the data
setwd("c:/users/n/downloads")
#memory.limit()
#round(memory.limit(),2) 

#save(ls, file = "test")
#load("test")
#ls()

emails = read.csv("emails.csv", stringsAsFactors=FALSE)
dim(emails)
table(emails$spam)
names(emails)

emails$nchar = nchar(emails$text)
emails$row = rownames(emails)
myvars = emails[c("nchar", "row")]
myvars = myvars[with(myvars, order(nchar)), ]
#edit(myvars)


library(tm)
#install.packages("SnowballC")
library(SnowballC)

#Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpus = Corpus(VectorSource(emails$text))

#Convert corpusTitle and corpus to lowercase.
corpus = tm_map(corpus, tolower)

#Remove the punctuation in corpusTitle and corpus.
corpus = tm_map(corpus, removePunctuation)
stopwords("english")[1:10]

#Remove stop words 
corpus = tm_map(corpus, removeWords, c( stopwords("english")))

#Stem the words in corpusTitle and corpus (each stemming might take a few minutes).
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)

spdtm = removeSparseTerms(dtm, .95) # = .03 percent
dim (spdtm)

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

emailsSparse$spam = emails$spam
emailsHam = subset(emailsSparse, spam ==0)
emailsSpam = subset(emailsSparse, spam ==1)
dim (emailsHam)
dim (emailsSpam)

f = colSums(emailsSpam)
f2 = f[f>=1000]
f2
length(f2)


emailsSparse$spam = as.factor(emailsSparse$spam)

# Split the data
library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split==TRUE)
test = subset(emailsSparse, split==FALSE)

spamLog = glm(spam ~ ., data=train, family=binomial)
predictLog = predict(spamLog)
summary(spamLog)

library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~ ., data=train, method="class")

# Build random forest model
library(randomForest)
spamRF = randomForest(spam ~ ., data=train) #Train, ntree=200, nodesize=25 )

predictLog = predict(spamLog, type="response")
summary(spamLog)
table(predictLog < .00001)
table(predictLog > .99999)
table(predictLog >= .00001 & predictLog <= .99999)

predictCART = predict(spamCART)
predictRF = predict(spamRF, type = "prob")

#NB: You can obtain probabilities for CART models by not passing any type parameter to the predict() function, and you can obtain probabilities from a random forest by adding the argument type="prob". For CART and random forest, you need to select the second column of the output of the predict() function, corresponding to the probability of a message being spam.

#(Remember that if you used the type="class" argument when making predictions, you automatically used a threshold of 0.5. If you did not add in the type argument to the predict function, the probabilities are in the second column of the predict output.)

library(rpart)
library(rpart.plot)

prp(spamCART)

#Get the accuracy of the GLM test 
t1 = table(train$spam, predictLog > .5)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Sensitivity
Specificity
Accuracy

#Use the ROC package to look at the "Area under the Curve"
library(ROCR)
ROCRpred = prediction(predictLog, train$spam)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#Get the accuracy of the CART test
predictCART = predict(spamCART)
predictCART
sort(predictCART[,2])

t1 = table(train$spam, predictRF[,2] > .5)
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
ROCRpred = prediction(predictCART[,2], train$spam)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#Use the ROC package to look at the "Area under the Curve"
library(ROCR)
ROCRpred = prediction(predictRF[,2], train$spam)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

predictLog = predict(spamLog, type="response", newdata=test)
predictCART = predict(spamCART, newdata=test)
predictRF = predict(spamRF, type = "prob", newdata=test)

#Get the accuracy of the GLM test 
t1 = table(test$spam, predictRF[,2] > .5)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Accuracy

ROCRpred = prediction(predictRF[,2], test$spam)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc


##############################
# Read in the data
setwd("c:/users/n/downloads")
trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
names(trials)
table(nchar(trials$title))
trials$title
trials$c = nchar(trials$title)
myvars = trials[c("title", "c")]
edit(myvars)
names(trials)              

library(tm)
#install.packages("SnowballC")
library(SnowballC)


#Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

#Convert corpusTitle and corpusAbstract to lowercase.
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

#Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
stopwords("english")[1:10]

#Remove stop words 
corpusTitle = tm_map(corpusTitle, removeWords, c( stopwords("english")))
corpusAbstract = tm_map(corpusAbstract, removeWords, c( stopwords("english")))

#Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

#Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

#Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
dtmTitle = removeSparseTerms(dtmTitle, .95) # = .03 percent
dtmAbstract = removeSparseTerms(dtmAbstract, .95) # = .03 percent

#Convert dtmTitle and dtmAbstract to data frames.
dfTitle = as.data.frame(as.matrix(dtmTitle))
dfAbstract = as.data.frame(as.matrix(dtmAbstract))

dim (dfTitle)

cs = colSums(dfAbstract)
sort(cs)
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
colnames(dtmAbstract)

dtm = cbind(dfTitle, dfAbstract)
dtm$trials = trials$trial
dim(dtm)

# Split the data
library(caTools)
set.seed(144)
split = sample.split(dtm$trials, SplitRatio = 0.7)
traindtm = subset(dtm, split==TRUE)
testdtm = subset(dtm, split==FALSE)

#Basline accurac is based on predicting the most frequent outcome
table(traindtm$trials)
730/(572+730)

library(rpart)
library(rpart.plot)

dtmCART = rpart(trials ~ ., data=traindtm, method="class")
prp(dtmCART)

#Here we predict the continuous probability (Don't use type = class)
#Result has two columns which sum to 1.00.  Use only the 2nd column
predictCART = predict(dtmCART)
predictCART
sort(predictCART[,2])

predictCART2 = predict(dtmCART, type="class")
t1 = table(traindtm$trials, predictCART2)
t1

predictCART2 = predict(dtmCART, type="class", newdata=testdtm)
t1 = table(testdtm$trials, predictCART2)
t1

#Get the accuracy of the test t1 = table(censustest$over50k, predTest > .5)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Sensitivity = TP /(TP + FN)
Specificity = TN /(TN + FP)
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Sensitivity
Specificity
Accuracy


########################################
wiki = read.csv("wiki.csv", stringsAsFacors=FALSE)
names(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
summary(wiki$Vandal)

# Install new packages
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)

# Create corpus
corpusAdded = Corpus(VectorSource(wiki$Added))
str(corpusAdded)
corpusAdded[[1]]

#REMOVE STOP WORDS
# Look at stop words 
stopwords("english")[1:10]

# 3Remove stop words 
corpusAdded = tm_map(corpusAdded, removeWords, c( stopwords("english")))
corpusAdded[[1]]

#STEM DOCUMENT
corpusAdded = tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

# Check for sparsity
tm::findFreqTerms(dtmAdded, lowfreq=20)

# Remove sparse terms
sparseAdded = removeSparseTerms(dtmAdded, .997) # = .03 percent
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = make.names(colnames(wordsAdded))
colnames(wordsAdded)

colnames(wordsAdded) = paste("A", colnames(wordsAdded))

source("words_removed.R")

wikiWords = cbind(wordsAdded,wordsRemoved)

# Add dependent variable
wikiWords$Vandal = wiki$Vandal

# Split the data
library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
trainWiki = subset(wikiWords, split==TRUE)
testWiki = subset(wikiWords, split==FALSE)

summary(testWiki$Vandal)
618/(618+545)

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data=trainWiki, method="class")

prp(wikiCART)

# Evaluate the performance of the model
# Using type = class sets the threshold at .5
predictCART = predict(wikiCART, newdata=testWiki, type="class")

t1 = table(testWiki$Vandal, predictCART)
#Get the accuracy of the test t1 = table(censustest$over50k, predTest > .5)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Accuracy

wikiWords2 = wikiWords
#Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")

# Evaluate the performance of the model
# Using type = class sets the threshold at .5
predictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
t1 = table(wikiTest2$Vandal, predictCART2)
t1

#Get the accuracy of the test 
t1 = table(censustest$over50k, predTest > .5)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Accuracy

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)

#Rerun again with the numwords variables added
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
summary(wikiCART2)

# Evaluate the performance of the model
# Using type = class sets the threshold at .5
predictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
t1 = table(wikiTest2$Vandal, predictCART2)
t1
#Get the accuracy of the test t1 = table(censustest$over50k, predTest > .5)
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Accuracy

wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

#Rerun again with the new variables added
wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")

predictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
t1 = table(wikiTest3$Vandal, predictCART3)
t1
TP = t1[2,2]; TN = t1[1,1]; FP = t1[1,2]; FN = t1[2,1]
Accuracy = (TP + TN)/(TP + TN + FP +FN)
Accuracy

prp(wikiCART3)

#################################
# Create corpus
tweets$Tweet
corpus = Corpus(VectorSource(tweets$Tweet))
str(corpus)
corpus[[1]]


#CLEAN UP IRREGULARITIES:
# 1Convert to lower-case
corpus = tm_map(corpus, tolower)
corpus[[1]]

# 2Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

#REMOVE STOP WORDS
# Look at stop words 
stopwords("english")[1:10]
# Remove stopwords and apple

# 3Remove stop words 
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

#STEM DOCUMENT
corpus = tm_map(corpus, stemDocument)
corpus[[1]]



# Video 6

# Create matrix
#DocumentTermMatrix  generates a matrix where
#the rows correspond to documents, in our case tweets,
#and the columns correspond to words in those tweets.

frequencies =tm::DocumentTermMatrix(corpus)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515])

# Check for sparsity

tm::findFreqTerms(frequencies, lowfreq=20)
tm::findFreqTerms(frequencies, lowfreq=100)

# Remove sparse terms

#Sparsity: If we say 0.98, this means to only keep terms that appear in 2% or more of the tweets.
sparse = removeSparseTerms(frequencies, 0.995)
sparse
matrix sparseAdded
# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))
names(tweetsSparse)
# Make all variable names R-friendly

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
colnames(tweetsSparse)

# Add dependent variable
tweetsSparse$Negative = tweets$Negative

# Split the data
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")

table(testSparse$Negative, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18)

# Baseline accuracy 

table(testSparse$Negative)

300/(300+55)


# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF)

# Accuracy:
(293+21)/(293+7+34+21)

