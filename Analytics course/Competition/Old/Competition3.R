install.packages("princomp")
princomp(myvars, cor = TRUE,na.action = na.exclude)

train_data_complete = complete.cases(train_data)
dim (train_data_complete)
train_data$Live_Alone = as.numeric(train_data$Live_Alone)
train_data$Hardship = as.numeric(train_data$Hardship)

myvars = train_data[c("Live_Alone", "Hardship","Accomplish", "Over_Head", "Gamble", "Liar", "CCDebt", "Breakfast", "Adventurous", "Optimist", "LifeHasPurpose" )]

cor(myvars, use="complete.obs", method = "pearson")
str(train_data$Hardship)
table(train_data$Live_Alone, train_data$Hardship)
train_data$Hardship
fis.finite(train_data$Live_Alone)
is.finite.data.frame(train_data)
is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}

myvars = train_data[c("YOB")]
edit (myvars)
table(train_data$Live_Alone, useNA = "always")
names(train_data)
train_data$Live_Alone = as.factor(train_data$Live_Alone)
train_data[train_data$Live_Alone == "",] = train_data[train_data$Live_Alone == "",]== NA 
