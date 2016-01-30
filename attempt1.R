library(caret)
library(ggplot2)
library(RCurl)

setwd("C:/USERS/DON/PML/")
getwd()

setInternet2(TRUE)
inTraindata <- read.csv("pml-training.csv", header=T,na.strings=c("NA", "#DIV/01",""))
inTestdata <- read.csv("pml-testing.csv", header=T,na.strings=c("NA","#DIV/0!",""))
RT<- as.factor(inTraindata$classe)

RT

inTrain = createDataPartition(inTraindata$classe, p = .60, list=FALSE)
training = adData[ inTrain,]
testing = adData[-inTrain,]

myDataNZV <- nearZeroVar(training, saveMetrics=TRUE)

dim(training)
dim(inTraindata)
newtrain <- data.matrix(training, rownames.force = NA)

featurePlot(x=newtrain[,c("age","male", "diagnosis")], y=RT, "pairs")
