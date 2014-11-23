---
title: "Practcal Machine Learning assignment"
author: "Tamás Nagy"
output: html_document
---

```{r, echo=FALSE}

setwd("c:/Users/Tamas/Documents/coursera/data science specialization/8. Practical machine learning")
```

# Loading the data

Keeping only the variables that contain momentary data (ie. not the statistics), because we want to be able to give prediction to any given moment.
I've furthermore omitted the first seven variables to exclude "metadata" and data that might confound the model (e.g. the row number, the name of the participant, etc.)

```{r, echo = T}
# Speeding up analysis by using multiple cores
library(doSNOW)
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
# stopCluster(cl)
library(caret)

# Reading data
data <- read.csv("pml-training.csv", na.strings = c("#DIV/0!","NA"))
data <- data[,sapply(data, function(x) sum(is.na(x))/length(x) < .95)]
data <- data[,-c(1:7)]

# Data partitioning
set.seed(123)
inTrain <- createDataPartition(y = data$classe, p = .6, list = F)
training <- data[inTrain,]
testing <- data[-inTrain,]

```

# Training

I used two methods (random forest and boosting with trees) to build models, as these methods are among be the bests for classification problems. 
Simple standardization was used as pre-processing. 

- Model 1 (random forests) yielded accuracy of 98.7%, with an OOB error rate of 0.84%. Moreover the importance of the variables are presented in the importance plot below.
- Model 2 (boosting) yielded accuracy of 96.3%, with an OOB error rate of 3.07%.

Model 1 seems to be more accurate.

```{r, echo=TRUE}
# Random forest model
model1 <- train(classe ~ .,data = training, 
               preProcess = c("scale","center"), 
               method = "rf")
model1
model1$finalModel
varImpPlot(model1$finalModel)

# Tree based bossting model
model2 <- train(classe ~ .,data = training, 
               preProcess = c("scale","center"), 
               method = "gbm", verbose = F)

model2
model2$finalModel


```

Testing model1 on the test dataset yielded excellent accuracy (99.1%), while model2 yielded 96.3% accuracy. I regard model1 as the final model.


```{r, echo=TRUE}

# Prediction using the random forest model
pred <- predict(model1,newdata = testing)
confusionMatrix(pred, testing$classe)

# Prediction using the tree based boosting
pred <- predict(model2,testing)
confusionMatrix(pred,testing$classe)

```

```{r, echo=FALSE}
examples <- read.csv("pml-testing.csv")
answers <- predict(model1,examples)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
```


