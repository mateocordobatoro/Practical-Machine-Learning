rm(list=ls())

library(dplyr)
library(caret)

training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
test <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")


## Eliminar varibales que aporten poca varianza

training$classe <- as.factor(training$classe)
test$classe <-as.factor(test$classe)

na.percent <- function(x){
  sum(is.na(x))/length(x)
}


x <- sapply(training, na.percent)
x <- names(x[x>0])

training <- training[,!(names(training)%in%x)]



vars <- names(training)[nearZeroVar(training)]

training <- training[,!(names(training) %in% vars)]


index <- createDataPartition(training$classe, p = 0.6, list = FALSE)
sub.train <- training[index,]
sub.test  <- training[-index,]


model1 <- rpart::rpart(classe~., data = sub.train, method = "class")
pred1  <- predict(model1, sub.test, type = "class")
table(pred1,sub.test$classe)


# Accurancy
sum(diag(table(pred1,sub.test$classe)))/length(sub.test$classe)


controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
model2 <- train(classe ~ ., data=sub.train, method="rf")
model2$finalmodel






# model desicion tree

 model1 <- rpart::rpart(classe~., data= pred.miss.model, method = "class")
 pred1 <- predict(model1,)
