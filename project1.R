library(caret)
library(ggplot2)
library(rpart.plot)
library(randomForest)
library(rattle)


setwd("C:/USERS/DON/PML/")
getwd()
list.files(getwd())

inTraindata <- read.csv("pml-training.csv", header=T,na.strings=c("NA", "#DIV/01",""))
inTestdata <- read.csv("pml-testing.csv", header=T,na.strings=c("NA","#DIV/0!",""))

na2zr <- inTraindata[, colSums(is.na(inTraindata)) == 0]
nadata<- nearZeroVar(inTraindata,saveMetrics=TRUE)

na2zr <- nadata

set.seed(3333)
inTrain <- createDataPartition(y=inTraindata$classe, p=0.60, list=F)
training <- na2zr[inTrain,]
testing <-  na2zr[-inTrain,]

traincontrol <- trainControl(method ="cv", number = 10)

rtfit <- train(classe ~ ., data = training, method = "rpart",trControl = traincontrol)


print(rtfit)
fancyRpartPlot(rtfit$finalModel)

names(fitted)

predict_rpart <- predict(rtfit, testing)
# Show prediction result
(conf_rpart <- confusionMatrix(testing$classe, predict_rpart))
predict_rpart <- predict(rtfit, testing)
(conf_rpart <- confusionMatrix(testing$classe, predict_rpart))


rffit <- train(classe ~ ., data = training, method = "rf",trControl = traincontrol)
print(rffit)
predict_rf <- predict(rffit, testing)
(conf_rpart <- confusionMatrix(testing$classe, predict_rf))


predict(rffit, inTestdata)
 