library(ggplot2)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(corrplot) 
data_train<-read.csv("/home/chakri/Desktop/coursera/Machine Learning/project/pml-training.csv")
data_test<-read.csv("/home/chakri/Desktop/coursera/Machine Learning/project/pml-testing.csv")
sum(complete.cases(data_train))
data_train<-data_train[,colSums(is.na(data_train))==0]
data_test<-data_test[,colSums(is.na(data_test))==0]
classe <- data_train$classe
trainRemove <- grepl("^X|timestamp|window", names(data_train))
data_train <- data_train[, !trainRemove]
train_cleaned <- data_train[, sapply(data_train, is.numeric)]
train_cleaned$classe <- classe
testRemove <- grepl("^X|timestamp|window", names(data_test))
data_test <- data_test[, !testRemove]
testCleaned <- data_test[, sapply(data_test, is.numeric)]
set.seed(22519) # For reproducibile purpose
data_partition <- createDataPartition(train_cleaned$classe, p=0.70, list=F)
model_train <- train_cleaned[data_partition, ]
testData <- train_cleaned[-data_partition, ]
controlRf <- trainControl(method="cv", 5)
gen_model <- train(classe ~ ., data=model_train, method="rf", trControl=controlRf, ntree=10)
gen_model
gen_prediction <- predict(gen_model, testData)
confusionMatrix(testData$classe, gen_prediction)
result<-predict(gen_model,testCleaned[,-length(names(testCleaned))])
result
classPlot <- cor(model_train[, -length(names(model_train))])
corrplot(classPlot)
"
decision_gen_Tree <- rpart(classe ~ ., data=model_train, method=\"class\")
prp(decision_gen_Tree)