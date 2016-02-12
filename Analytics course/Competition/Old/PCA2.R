setwd("c:/users/n/skydrive/analytics course/competition")
train_numeric_small = read.csv("train_numeric_small.csv", stringsAsFactors=FALSE)

train_numeric_small = load("t_n_s")
names(train_numeric_small)[names(train_numeric_small) == 'Q115899'] = 'Hardship'
names(train_numeric_small)[names(train_numeric_small) == 'Q120194'] = 'StartTask'
names(train_numeric_small)[names(train_numeric_small) == 'Q120014'] = 'Successful'
names(train_numeric_small)[names(train_numeric_small) == 'Q119334'] = 'Accomplish'
names(train_numeric_small)[names(train_numeric_small) == 'Q118237'] = 'Over_Head'
names(train_numeric_small)[names(train_numeric_small) == 'Q113992'] = 'Gamble'
names(train_numeric_small)[names(train_numeric_small) == 'Q106389'] = 'Liar'
names(train_numeric_small)[names(train_numeric_small) == 'Q102674'] = 'CCDebt'
names(train_numeric_small)[names(train_numeric_small) == 'Q102687'] = 'Breakfast'
names(train_numeric_small)[names(train_numeric_small) == 'Q102289'] = 'Adventurous'
names(train_numeric_small)[names(train_numeric_small) == 'Q101162'] = 'Optimist'
names(train_numeric_small)[names(train_numeric_small) == 'Q99716'] = 'Live_Alone'
names(train_numeric_small)[names(train_numeric_small) == 'Q98869'] = 'LifeHasPurpose'
names(train_numeric_small)[names(train_numeric_small) == 'Q107869'] = 'Normal'
names(train_numeric_small)[names(train_numeric_small) == 'Q102906'] = 'Grudge'
#names(train_numeric_small)[names(train_numeric_small) == 'Q106997'] = 'People'
#names(train_numeric_small)[names(train_numeric_small) == 'Q108855'] = 'Ext_Family'
names(train_numeric_small)[names(train_numeric_small) == 'Q115610'] = 'Positive_Thinking'

#train_data_complete = complete.cases(train_numeric_small)
#Dim (train_data_complete)
names(train_numeric_small)
train_numeric_small$Q106997
myvars = train_numeric_small[c("Live_Alone", "Hardship","Accomplish", "Over_Head", "Gamble", "Liar", "CCDebt", "Breakfast", "Adventurous", "Optimist", "LifeHasPurpose", "Normal","Grudge", "Positive_Thinking")]
is.finite(myvars)
pc = princomp(myvars, cor = TRUE,na.action = na.exclude)
str(pc)
dim (pc$scores)
dim (train_numeric_small_with_Happy)
names(train_numeric_small_with_Happy)[names(train_numeric_small_with_Happy) == 'X..'] = 'Happy'

train_numeric_small_with_Happy = cbind(train_numeric_small, pc$scores)
names(train_numeric_small_with_Happy)

train_glm = glm(Happy ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.8+Comp.9+Comp.10+Comp.11, data=train_numeric_small_with_Happy, family=binomial)
predict_train = predict(train_glm, type = "response")
summary(train_glm)

ROCRpred = prediction(predict_train, train_data$Happy)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

