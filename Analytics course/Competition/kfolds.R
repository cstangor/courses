install.packages("ROSE")
library(ROSE)
library(plyr)
library(randomForest)
dim(testset)
names(testset[-1])
train$UserID
train$Happy = as.factor(train$Happy)
k=5 #Folds
id <- sample(1:k,nrow(train),replace=TRUE)
list <- 1:k
prediction <- data.frame()
testsetCopy <- data.frame()
#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)
for (i in 1:k){
  trainingset <- subset(train, UserID %in% list[-i])
  # Performing upsampling of minorities using ROSE package
  trainingset <- ROSE(Happy~. , data=train, seed=3, p=0.15, N=length(train$Happy))$data
  testset <- subset(train, UserID %in% c(i))
  mymodel <- randomForest(train$Happy~. - votes, data=trainingset, ntree=100)
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  prediction <- rbind(prediction, temp)
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  progress.bar$step()
}
result <- cbind(adapred, testsetCopy)
names(result) <- c("Predicted", "Actual")
table(result)