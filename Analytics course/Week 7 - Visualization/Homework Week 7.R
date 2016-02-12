install.packages("wordcloud")
library(wordcloud)

setwd("c:/users/n/skydrive/analytics course/week 7 - visualization")
tweets = read.csv("tweets.csv")

library(tm)
library(SnowballC)
names(tweets)

tweets = subset(tweets, Avg < -1)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english"), "apple"))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

names(allTweets)
sub1 = subset(allTweets, avg < -1)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))
?RColorBrewer
#######################

(150*617)+(16*238)
setwd("c:/users/n/skydrive/analytics course/week 7 - visualization")
edges = read.csv("edges.csv")
users = read.csv("users.csv")
dim(edges)
names(users)
table(users$school, users$gender)
fnames(edges)
146/(59/2)

?graph.data.frame
install.packages("igraph")
library(igraph)
g=graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
sort(degree(g))
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
sort(V(g)$size)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

?###############################

setwd("c:/users/n/skydrive/analytics course/week 3 - Logistic Regression")
parole = read.csv("parole.csv")
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
table(parole$male)

#ggplot Histograms:
#Regular 
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth=5, color="blue")

#Two histograms, split by variable male:  use male~. to stack
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)

#Two histograms stacked and  colored
ggplot(data = parole, aes(x = age, fill=male)) + geom_histogram(binwidth = 5) 

#Two histograms on top of each other, not stacked
ggplot(data = parole, aes(x = age, fill=male)) + geom_histogram(binwidth = 5, position = "identity", alpha = .5) 

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth=.1, color="blue")

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth=1, color="blue") + facet_grid(.~crime)

ggplot(data = parole, aes(x = age, fill=crime)) + geom_histogram(binwidth = 5, position = "identity", alpha = .5) 
#################################

statesMap = map_data("state")
table(statesMap$group)
gp = ggplot(statesMap)
gp = gp + aes(x = long, y = lat, group = group)
gp = gp + geom_polygon(fill = "white", color = "black")
gp = gp + coord_map("mercator")
gp


setwd("c:/users/n/skydrive/analytics course/week 7 - visualization")
polling = read.csv("PollingImputed.csv")
names(polling)

Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
dim (polling)
dim (Train)
dim (Test)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

names(predictionDataFrame)
describe(predictionDataFrame$TestPredictionBinary)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)

predictionMap = merge(statesMap, predictionDataFrame, by = "region")

predictionMap = predictionMap[order(predictionMap$order),]
dim(predictionMap)
dim(statesMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(alpha = .3, color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend",   name = "Prediction 2012")

?geom_polygon
