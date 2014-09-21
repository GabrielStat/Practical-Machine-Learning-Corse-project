## R markdown for Practical Machine Learning Corse project

##Loading original training data
data_train<-read.csv("pml-training.csv")

##Loading original testing data
data_test<-read.csv("pml-testing.csv")

## deleting unnecessary columns manually
dcol<-c(1, 3,5,12:36,50:59,69:83,87:101,103:112,125:139,141:150)

training<-data_train[,-dcol]
testing<-data_test[,-dcol]


## testing by plotting
plot(training$roll_belt,training$pitch_forearm,col=training$classe)
plot(training$roll_forearm,training$pitch_forearm,col=training$classe)

##  Out of sample error estimate.  
datSet<-training

## Trial RF training
library(caret)

## Breaking the original test file into training (train0) and testing (tes0) parts
inTrain0 <- createDataPartition(y=datSet$classe,p=0.6, list=FALSE)
train0 <- datSet[inTrain0,]
test0 <- datSet[-inTrain0,]

## determining number of cross validation option     
trControl <- trainControl(method = "cv", number = 4) ## number of cross validations

## Training the data on the subset 
modFit0 <- train(train0$classe~ .,data=train0, method="rf", trControl = trControl)

## Displaying results of training 
modFit0
summary(modFit0) 
modFit0$finalModel

## Random forest variable importance
varImp(modFit0)


##  Checking predictions on test0 for out of sample error
pred0<-predict(modFit0,test0); 
summary(pred0) 
table(pred0,test0$classe)

## Out of sample error estimate  
out_error <- 1 -  sum(pred0 == test0$classe)/length(pred0)
out_error*100 ##in %

 

## Final random forest on full training data set
modFit<- train(training $classe~ .,data=training, method="rf", trControl = trControl)
modFit
pred <- predict(modFit, testing)
pred
