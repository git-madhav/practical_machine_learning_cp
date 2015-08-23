# Project

# include required packages.
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)


# set current working directory

# change this appropriately to the location test wants. 
#setwd('D:\\Personal\\Madhav\\Learning\\DataScience\\08-PracticalMachineLearning\\project')

# download training data set
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainFile <- "pml-training.csv"
download.file(trainURL, trainFile)

# download test data set
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testFile <- 'pml-testing.csv'
download.file(testURL, testFile)

# read data sets into memory, set NA for missing and NAN values. 
training.orig <- read.csv(trainFile, na.strings=c("NA","#DIV/0!",""))

testing.orig <- read.csv(testFile, na.strings=c("NA","#DIV/0!",""))

# set the seed to reproduce the results. 
set.seed(45678)

# cleaning.
## assign it to short variable names for convenience.
t <- training.orig
test <- testing.orig

# Remove all columns that have any NA values.
t.1 <- t[, colSums(is.na(t)) == 0]

# Remove meta data columns, with following names. 
m_names <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
# corresponding column numbers
m_num <- c(1, 2, 3, 4, 5, 6, 7)
# remove these columns from t.1
t.2 <- t.1[,-m_num]

# check if there are any near zero variance columns or close to constant values.
nzvData <- nearZeroVar(t.2)
nzvData
# we had zero columns with zero variance.

# Now makes sure test data set that is used for submitting answers contain same columns
n <- colnames(t.2)
test.1 <- test[,n[-53]]

# check if class of each column is same. 
test.2 <- test.1

x <- lapply(t.2, class)
y <- lapply(test.2, class)

# all.equal command shows following columns to be different. 
# all.equal(y, x[-53])
#[1] "Component "magnet_dumbbell_z": 1 string mismatch"
#[2] "Component "magnet_forearm_y": 1 string mismatch" 
#[3] "Component "magnet_forearm_z": 1 string mismatch" 
# not sure if numeric class is same as integer. But change it to make sure we don't have problems with predict function.
test.2[,'magnet_dumbbell_z'] <- as.numeric(test.2[,'magnet_dumbbell_z'])
test.2[,'magnet_forearm_y'] <- as.numeric(test.2[,'magnet_forearm_y'])
test.2[,'magnet_forearm_z'] <- as.numeric(test.2[,'magnet_forearm_z'])


# Modeling using 3 tree models. 
# 2 classification trees from rpart and ctree and one randomforest 

# Used kfold (10 folds) cross validatoin for all models to tune training set.

# specify cross validation option using trainControl to train function.
ctrl <- trainControl(method = "cv")

# ctree model
ctreeFit <- train(classe ~ ., data = t.2, method = "ctree", trControl = ctrl)

# see output results
ctreeFit

# plot accuracy plot to see which model is better
plot(ctreeFit)


# Rpart model
rpartFit <- train(classe ~ ., data = t.2, method = "rpart", trControl = ctrl)

# see output
rpartFit

# plot accuracy plot to see which model is better
plot(rpartFit)

# rf model
randomForestFit <- train(classe ~ ., data = t.2, method = "rf", trControl = ctrl)

# see output
randomForestFit

# plot accuracy plot to see which model is better
plot(randomForestFit)

# glm model
# glmFit <- train(classe ~ ., data = t.2, method = "glm", trControl = ctrl) 
# This gave an error. Needs to dig further why regression models don't work on this data set. 

# Extra trials
# preprocess data : See if preprocessing data helps accuracy any further
#preProcessT <- preProcess(t.2[,-53], method = c("center","scale","medianImpute"))
#t.3 <- predict(preProcessT, t.2[,-53])
#t.3 <- cbind(t.3, t.2[,53])
#n <- colnames(t.2)
#names(t.3) <- n
#test.3 <- predict(preProcessT, test.2)

# ctree model
#ctreeFit2 <- train(classe ~ ., data = t.3, method = "ctree", trControl = ctrl)

# see output results
#ctreeFit2

# plot accuracy plot to see which model is better
#plot(ctreeFit2)

# Rpart model
#rpartFit2 <- train(classe ~ ., data = t.3, method = "rpart", trControl = ctrl)

# see output
#rpartFit2

# plot accuracy plot to see which model is better
#plot(rpartFit2)

# rf model
#randomForestFit2 <- train(classe ~ ., data = t.3, method = "rf", trControl = ctrl)

# see output
#randomForestFit2

# plot accuracy plot to see which model is better
#plot(randomForestFit2)


# prediction of test set for submitting results.

# Based on output of above model output and plots, it is obvious to chose randomForest model for predicting test set for submitting the results.

# Use pretict function to predict.
predRandomForestFit <- predict(randomForestFit, test.2)

# output results
predRandomForestFit

# Just curious how different results I get with other models. I wasn't using them to tune further. I can't anyway since I don't have the right answer to tune.
# ctree is slightly different
#predCtreeFit <- predict(ctreeFit, test.2)
#predCtreeFit

# rpart is lot more different and is evident from crossvalidation out of sample accuracy
#predRpartFit <- predict(rpartFit, test.2)
#predRpartFit

# Use the function provided by coursera to write files for submission.
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# call the function.
pml_write_files(predRandomForestFit)
