setwd("c:/users/n/downloads")
claims = read.csv("reimbursement.csv", stringsAsFactors=FALSE)
names(claims)
dim (claims)
rs = claims[, 2:12]
dim (rs)
rss = rowSums(rs)
table(rss)
names (rs)
(458005-177578)/458005
names(rss)

rc = cor(rs, use="complete.obs", method = "pearson")
rc = ifelse(rc == 1, 0, rc)
max(abs(rc))

hist(claims$reimbursement2009)
claims$reimbursement2008 = log(claims$reimbursement2008+1)
claims$reimbursement2009 = log(claims$reimbursement2009+1)    

table(claims$reimbursement2009==0)
90498/(367507+90498)

set.seed(144)
spl = sample(1:nrow(claims), size=0.7*nrow(claims))
train = claims[spl,]
test = claims[-spl,]

names(claims)
lm.train = lm(reimbursement2009 ~ ., data=train)
summary(lm.train)

predTest2 = mean(test$reimbursement2009)
TestSetStats(test, test$reimbursement2009, predTest2)

predTest = predict(lm.train3, newdata=test)
predTest3 = test$reimbursement2008
TestSetStats(test, test$reimbursement2009, predTest3)

predTest2 = predict(mean(train$reimbursement2009), newdata=test)

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

train.limited = train
train.limited$reimbursement2009 = NULL
test.limited = test
test.limited$reimbursement2009 = NULL

library(caret)
preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
test.norm = predict(preproc, test.limited)

names(train.norm)
mean(test.norm$arthritis)

set.seed(144)
km = kmeans(train.norm, centers = 3)
cluster1 = subset(km, km$cluster==1)
cluster2 = subset(km, km$cluster==2)
cluster3 = subset(km, km$cluster==3)
#train.norm$cluster  = km$cluster

names(train.norm)[2:12]
names(train.norm)

#CREATE FORMULA:
listoffactors <- names(train.norm)[2:12]
dvlist = paste(listoffactors,collapse = "+")
ivlist = "cluster"
fmla = paste(dvlist, "~")
fmla = paste(fmla, ivlist)
fmla = as.formula(fmla)
fmla

aggregate(.  ~cluster, data = train.norm, FUN = "mean")

#install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(km, train.norm)
cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata=test.norm)
table (cluster.test)

cluster.train$cluster
cluster1 = subset(train, cluster.train==1)
cluster2 = subset(train, cluster.train==2)
cluster3 = subset(train, cluster.train==3)

test1 = subset(test, cluster.test==1)
test2 = subset(test, cluster.test==2)
test3 = subset(test, cluster.test==3)

mean(cluster1$reimbursement2009)
mean(cluster2$reimbursement2009)
mean(cluster3$reimbursement2009)

lm1 = lm(reimbursement2009 ~., data = cluster1)
summary (lm1)

lm2 = lm(reimbursement2009 ~., data = cluster2)
summary (lm1)

lm3 = lm(reimbursement2009 ~., data = cluster3)
summary (lm1)


pred.test1 = predict(lm1, newdata=test1)
pred.test2 = predict(lm2, newdata=test2)
pred.test3 = predict(lm3, newdata=test3)

mean(pred.test3)

TestSetStats(test3, test3$reimbursement2009, pred.test3)

all.predictions = c(pred.test1, pred.test2, pred.test3)
all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)
all.outcomes

TestSetStats(all.outcomes, all.outcomes, all.predictions)

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

################
setwd("c:/users/n/downloads")
airlines  = read.csv("AirlinesCluster.csv", stringsAsFactors=FALSE)
summary(airlines)

#Normalize
install.packages("caret")
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
dim (airlinesNorm)

# Compute distances
distances = dist(airlinesNorm[2:7], method = "euclidean")

# Hierarchical clustering
clusterAirlines = hclust(distances, method = "ward") 

# Plot the dendrogram
plot(clusterAirlines)

clusterGroups = cutree(clusterAirlines, k = 5)

cluster1 = subset(airlines, clusterGroups==1)
cluster2 = subset(airlinesNorm, clusterGroups==2)
cluster3 = subset(airlinesNorm, clusterGroups==3)
cluster4 = subset(airlinesNorm, clusterGroups==4)
cluster5 = subset(airlinesNorm, clusterGroups==5)
dim(cluster1)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
names(airlines)
aggregate(DaysSinceEnroll~clusterGroups, data = airlines, FUN = "mean")

############################

setwd("c:/users/n/downloads")
kos = read.csv("dailykos.csv", stringsAsFactors=FALSE)

names(kos)
dim(kos)

# Compute distances
distances = dist(kos[2:1546], method = "euclidean")

# Hierarchical clustering
clusterKos = hclust(distances, method = "ward") 

# Plot the dendrogram
plot(clusterKos)

# Assign points to clusters
clusterGroups = cutree(clusterKos, k = 7)
names(clusterGroups)

cluster1 = subset(kos, clusterGroups==1)
cluster2 = subset(kos, clusterGroups==2)
cluster3 = subset(kos, clusterGroups==3)
cluster4 = subset(kos, clusterGroups==4)
cluster5 = subset(kos, clusterGroups==5)
cluster6 = subset(kos, clusterGroups==6)
cluster7 = subset(kos, clusterGroups==7)
dim (cluster7)

#Find the 6 most frequent words in each cluster
tail(sort(colMeans(cluster6[-1])))

# Run k-means
set.seed(1000)
KMC = kmeans(kos[2:1546], centers = 7)
str(KMC)
cluster1 = subset(kos, KMC$cluster==1)
cluster2 = subset(kos, KMC$cluster==2)
cluster3 = subset(kos, KMC$cluster==3)
cluster4 = subset(kos, KMC$cluster==4)
cluster5 = subset(kos, KMC$cluster==5)
cluster6 = subset(kos, KMC$cluster==6)
cluster7 = subset(kos, KMC$cluster==7)

table(clusterGroups, KMC$cluster)
dim(cluster7)

#Find the 6 most frequent words in each cluster
tail(sort(colMeans(cluster1[-1])))
