par(mar=c(4,4,1,1))
par(mfrow=c(2,2))
for(a in 2:length(colnames(train_data))) {
  category=train_data[,a]
  barplot(table(train_data$Happy, category), main=colnames(train_data[a]),
          col=c("red","gray"))
}

for(a in 2:length(colnames(train_data))) {
  category=train_data[,a]
  train_glm = lm(Happy ~ category, data = train_data)
  #print(summary(train_glm))
  p = summary(train_glm)$r.squared
  print(paste(colnames(train_data[a]), p, sep = " "))

}

category
