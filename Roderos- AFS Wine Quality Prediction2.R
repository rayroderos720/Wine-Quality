#Wine Quality Prediction - For AFS Technical Interview
#
#
#
#
#Start Computation Time
ptm <- proc.time()

#Set Seed for Reproduction
set.seed(3)

#Required Libraries
require(plsRglm)
require(caret)
require(plyr)

getwd()
setwd("~/R")

#Load CSV Files 
white.full <- read.csv("winequality-white.csv", header = TRUE, sep = ";")
red.full <- read.csv ("winequality-red.csv",header = TRUE, sep =";")

##-----------------------------------------------------------------------------##
#Feature Engineering Part 1

#Add type of wine as category "color"
white.full$color <- 1
red.full$color <- 0

#Rescaling values of quality from 3-8 to 1-6 for better a algorithm
white.full$quality <- mapvalues(white.full$quality, c(3,4,5,6,7,8),c(1,2,3,4,5,6))
red.full$quality <- mapvalues(red.full$quality, c(3,4,5,6,7,8),c(1,2,3,4,5,6))

##-----------------------------------------------------------------------------##
#Feature Engineering Part 2
#Creates the following features:
#1. To take into account interaction effects of the following features:
#    of fixed acidity, volatile acidity and citric acid
#    of free sulfur dioxed and total sulfure dioxide
#    of sugar with chlorides, dioxides and and density
#    of density and aclohol
#2. To take into account combined effects of the 3 features most correlated with wine quality
#     density, volatile acidity, alcohol with thre rest of the features
#3. Log transformations of the original features to take into account relative changes in value 


white.full$acid1 <- white.full$fixed.acidity*white.full$volatile.acidity
white.full$acid2 <- white.full$fixed.acidity*white.full$citric.acid
white.full$acid3 <- white.full$volatile.acidity*white.full$citric.acid
white.full$dioxide <- white.full$free.sulfur.dioxide*white.full$total.sulfur.dioxide
white.full$acid.chloride <- white.full$fixed.acidity*white.full$chlorides
white.full$sugar.dioxide <- white.full$residual.sugar*white.full$total.sulfur.dioxide
white.full$sugar.density <- white.full$residual.sugar*white.full$density
white.full$density.alcohol <- white.full$density*white.full$alcohol

white.full$alcohol.acidity1 <- white.full$density+white.full$fixed.acidity
white.full$alcohol.acidity2 <-white.full$density+white.full$volatile.acidity
white.full$alcohol.acidity3 <- white.full$density+white.full$citric.acid
white.full$alcohol.chlorides <- white.full$density+white.full$chlorides
white.full$alcohol.dioxide <- white.full$density+white.full$free.sulfur.dioxide
white.full$alcohol.dioxide2 <- white.full$density+white.full$total.sulfur.dioxide
white.full$alcohol.pH <- white.full$density+white.full$pH
white.full$alcohol.sulphates <- white.full$density+white.full$sulphates

white.full$density.acidity1 <- white.full$density+white.full$fixed.acidity
white.full$density.acidity2 <-white.full$density+white.full$volatile.acidity
white.full$density.acidity3 <- white.full$density+white.full$citric.acid
white.full$density.chlorides <- white.full$density+white.full$chlorides
white.full$density.dioxide <- white.full$density+white.full$free.sulfur.dioxide
white.full$density.dioxide2 <- white.full$density+white.full$total.sulfur.dioxide
white.full$density.pH <- white.full$density+white.full$pH
white.full$density.sulphates <- white.full$density+white.full$sulphates

white.full$volatile.acidity.acidity1 <- white.full$volatile.acidity+white.full$fixed.acidity
white.full$volatile.acidity.acidity2 <-white.full$volatile.acidity+white.full$volatile.acidity
white.full$volatile.acidity.acidity3 <- white.full$volatile.acidity+white.full$citric.acid
white.full$volatile.acidity.chlorides <- white.full$volatile.acidity+white.full$chlorides
white.full$volatile.acidity.dioxide <- white.full$volatile.acidity+white.full$free.sulfur.dioxide
white.full$volatile.acidity.dioxide2 <- white.full$volatile.acidity+white.full$total.sulfur.dioxide
white.full$volatile.acidity.pH <- white.full$volatile.acidity+white.full$pH
white.full$volatile.acidity.sulphates <- white.full$volatile.acidity+white.full$sulphates

red.full$acid1 <- red.full$fixed.acidity*red.full$volatile.acidity
red.full$acid2 <- red.full$fixed.acidity*red.full$citric.acid
red.full$acid3 <- red.full$volatile.acidity*red.full$citric.acid
red.full$dioxide <- red.full$free.sulfur.dioxide*red.full$total.sulfur.dioxide
red.full$acid.chloride <- red.full$fixed.acidity*red.full$chlorides
red.full$sugar.dioxide <- red.full$residual.sugar*red.full$total.sulfur.dioxide
red.full$sugar.density <- red.full$residual.sugar*red.full$density
red.full$density.alcohol <- red.full$density*red.full$alcohol

red.full$alcohol.acidity1 <- red.full$density+red.full$fixed.acidity
red.full$alcohol.acidity2 <-red.full$density+red.full$volatile.acidity
red.full$alcohol.acidity3 <- red.full$density+red.full$citric.acid
red.full$alcohol.chlorides <- red.full$density+red.full$chlorides
red.full$alcohol.dioxide <- red.full$density+red.full$free.sulfur.dioxide
red.full$alcohol.dioxide2 <- red.full$density+red.full$total.sulfur.dioxide
red.full$alcohol.pH <- red.full$density+red.full$pH
red.full$alcohol.sulphates <- red.full$density+red.full$sulphates

red.full$density.acidity1 <- red.full$density+red.full$fixed.acidity
red.full$density.acidity2 <-red.full$density+red.full$volatile.acidity
red.full$density.acidity3 <- red.full$density+red.full$citric.acid
red.full$density.chlorides <- red.full$density+red.full$chlorides
red.full$density.dioxide <- red.full$density+red.full$free.sulfur.dioxide
red.full$density.dioxide2 <- red.full$density+red.full$total.sulfur.dioxide
red.full$density.pH <- red.full$density+red.full$pH
red.full$density.sulphates <- red.full$density+red.full$sulphates

red.full$volatile.acidity.acidity1 <- red.full$volatile.acidity+red.full$fixed.acidity
red.full$volatile.acidity.acidity2 <-red.full$volatile.acidity+red.full$volatile.acidity
red.full$volatile.acidity.acidity3 <- red.full$volatile.acidity+red.full$citric.acid
red.full$volatile.acidity.chlorides <- red.full$volatile.acidity+red.full$chlorides
red.full$volatile.acidity.dioxide <- red.full$volatile.acidity+red.full$free.sulfur.dioxide
red.full$volatile.acidity.dioxide2 <- red.full$volatile.acidity+red.full$total.sulfur.dioxide
red.full$volatile.acidity.pH <- red.full$volatile.acidity+red.full$pH
red.full$volatile.acidity.sulphates <- red.full$volatile.acidity+red.full$sulphates


#Creates log transformations of the original features to take into account relative changes in values
red.full <- cbind(red.full,log = log10(red.full[,c(1,2,4:11)]))
white.full <- cbind(white.full,log = log10(white.full[,c(1,2,4:11)]))



##-----------------------------------------------------------------------------##
#Dividing the dataset into training and test set (80-20) for white and red separately then combining them
# to have proportional amounts from the red and white for trainign and test

index.white <- createDataPartition(white.full$quality, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train.white <- white.full[index.white,]
test.white  <- white.full[-index.white,]

index.red <- createDataPartition(red.full$quality, p = .8, 
                                   list = FALSE, 
                                   times = 1)

train.red <- red.full[index.red,]
test.red  <- red.full[-index.red,]

train <- rbind(train.white,train.red)
test <- rbind(test.white,test.red)


train.y <- train[,12]
train.x <- train[,-12]
test.y <- test[,12]
test.x <- test[,-12]

##-----------------------------------------------------------------------------##
#Training and Prediction

#Training the model
model <- plsRglm(train.y,train.x,14, modele="pls-glm-gaussian")


#Making the preidction
pred_y <-predict(model,test.x)

##-----------------------------------------------------------------------------##
#Checking the Metrics

#Creat functions to compute RMSE, MAE and Accuracy
error <- (pred_y-test.y)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

mae <- function(error)
{
  mean(abs(error))
}

accuracy <- function(error){
  1-mean(abs(error))
}

#Compute MAE, RMSE and Accuracy of Prediction
mae(error)
rmse(error)
accuracy(error)

#Computes for accuracy with rounded values made by prediction
acc <- round(pred_y) == test.y
length(acc[acc==TRUE])/length(acc)


##-----------------------------------------------------------------------------##
#Check the total computation time
proc.time() - ptm 
