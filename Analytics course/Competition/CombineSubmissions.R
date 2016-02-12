setwd("c:/users/n/skydrive/analytics course/competition/submissions")
sub1 = read.csv("submission1.csv", stringsAsFactors=FALSE)
names(sub1)[names(sub1) == 'Probability1'] <- 'sub1'
subdf =as.data.frame(sub1)

sub2 = read.csv("submission2.csv", stringsAsFactors=FALSE)
names(sub2)[names(sub2) == 'Probability1'] <- 'sub2'
subdf = cbind(sub1, as.data.frame(sub2))

sub3 = read.csv("submission3.csv", stringsAsFactors=FALSE)
names(sub3)[names(sub3) == 'Probability1'] <- 'sub3'
subdf = cbind(subdf, as.data.frame(sub3))
names(subdf)

sub4 = read.csv("submission4.csv", stringsAsFactors=FALSE)
names(sub4)[names(sub4) == 'Probability1'] <- 'sub4'
subdf = cbind(subdf, as.data.frame(sub4))
names(subdf)
Hmisc::rcorr(subdf$sub2, subdf$sub4)

sub5 = read.csv("submission4.csv", stringsAsFactors=FALSE)
names(sub5)[names(sub5) == 'Probability1'] <- 'sub5'
subdf = cbind(subdf, as.data.frame(sub5))
names(subdf)
Hmisc::rcorr(subdf$sub1, subdf$sub5)

edit(subdf)
cor(subdf$sub1, subdf$sub2, subdf$sub3)
str(subdf)

subdf$Probability1 = rowMeans(subdf[, c("sub1","sub2", "sub3", "sub4")], na.rm = TRUE) 
subdf$Probability1
submission = subdf[c("UserID", "Probability1")]
submission
setwd("c:/users/n/skydrive/analytics course/competition/submissions")
write.csv(submission, "submission5.csv", row.names=FALSE) 
setwd("c:/users/n/skydrive/analytics course/competition")
