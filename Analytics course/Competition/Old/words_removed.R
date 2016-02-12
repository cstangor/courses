# Read in the data
setwd("c:/users/n/downloads")
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
names(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
summary(wiki$Vandal)

# Install new packages
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)

# Create corpus
corpusRemoved = Corpus(VectorSource(wiki$Removed))
str(corpusRemoved)
corpusRemoved[[1]]

#REMOVE STOP WORDS
# Look at stop words 
stopwords("english")[1:10]

# 3Remove stop words 
corpusRemoved = tm_map(corpusRemoved, removeWords, c( stopwords("english")))
corpusRemoved[[1]]

#STEM DOCUMENT
corpusRemoved = tm_map(corpusRemoved, stemDocument)
corpusRemoved[[1]]

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

# Check for sparsity
tm::findFreqTerms(dtmRemoved, lowfreq=20)

# Remove sparse terms
sparseRemoved = removeSparseTerms(dtmRemoved, .997) # = .03 percent
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = make.names(colnames(wordsRemoved))
colnames(wordsRemoved)

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(colnames(wordsRemoved))

wordsRemoved
