---
title: "Practical Machine Learning"
author: "dnrl"
date: " January 2016"
output: html_document
---
Latest trend in exercising is measuring the amount or quanitity of exercise we do.  Small devices that can be carried in a pocket, on a band, in a watch or shoe, can measure all of the movement of that user.  
Unfortunately, most of them do not measure the quality of the movement.  This project will take data worn by individuals and determine how well the exercises were performed.

## Set up libraries and read data into files
```{r}
library(caret)
library(ggplot2)
library(rpart.plot)
library(randomForest)
library(rattle)
setwd("C:/USERS/DON/PML/")
set.seed(2048)

inTraindata <- read.csv("pml-training.csv", header=T,na.strings=c("NA", "#DIV/0!",""))
Classdata <- read.csv("pml-testing.csv", header=T,na.strings=c("NA","#DIV/0!",""))
```
###  Source of the data
###Training data set "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
### Class data set "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

Set up for the processing, clearing out NA and data that causes problem with the modeling
from completing and compiling cleanly.
Splitting the data file into 60% training and 40% testing.

```{r}
for(i in c(8:ncol(inTraindata)-1)) {inTraindata[,i] = as.numeric(as.character(inTraindata[,i]))}

for(i in c(8:ncol(Classdata)-1)) {Classdata[,i] = as.numeric(as.character(Classdata[,i]))}

Dataset <- colnames(inTraindata[colSums(is.na(inTraindata)) == 0])[-(1:7)]  
model_set <- inTraindata[Dataset]    
inTrain <- createDataPartition(y=model_set$classe, p=0.60, list=F) 
training <- model_set[inTrain,] 
testing <-  model_set[-inTrain,] 
```
#Modeling the data.
Fit the data using rpart: Recursive Partitioning and Regression Trees

First to split it into 5 folds.
fit it into the model with the classe data.
predict for 60% of data and then against the othe 40%.

```{r}
traincontrol <- trainControl(method ="cv", number = 5) 

rtfit <- train(classe ~ ., data = training, method = "rpart",trControl = traincontrol)
print(rtfit) #
fancyRpartPlot(rtfit$finalModel) 

predict_rpart <- predict(rtfit, training) 
(conf_rpart <- confusionMatrix(training$classe, predict_rpart))

predict_rpart <- predict(rtfit, testing) 
(conf_rpart <- confusionMatrix(testing$classe, predict_rpart)) 

```

# Using Random Forest to model the same data to compare accuracy.

```{r}
rffit <- train(classe ~ ., data = training, method = "rf",trControl = traincontrol)  
print(rffit) 

predict_rf <- predict(rffit, training)  
(conf_rf <- confusionMatrix(training$classe, predict_rf)) 


predict_class <- predict(rffit, Classdata)

predict_class
```


