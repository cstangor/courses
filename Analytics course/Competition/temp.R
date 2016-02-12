require(Hmisc)
bystats(df.train$Age, df.train$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))

barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")

readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types )
}
?read.csv

