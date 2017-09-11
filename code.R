urlTraining <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTesting <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
fileTraining <- "training.csv"
fileTesting <- "testing.csv"
#download.file(url = urlTraining, fileTraining)
#download.file(url = urlTesting, fileTesting)

data <- read.csv(fileTraining, na.strings=c("NA", "", "#DIV/0!"))

## Exploratory analysis

# many columns include a mostly NAs
unlist(lapply(1:160, function(i) {sum(is.na(data[, i]))}))
# these columns are
colnames(data)[unlist(lapply(1:160, function(i) {if (sum(is.na(data[, i])) > dim(data)[1] * .95) i }))]
# these variables are explained by a variable new_window, which are 'yes' for the rows that
# contain not-NA values in these columns. With the names min, max, avg, var, stddev etc. 
# it's obvious that these variables are summarising ones. Therefore, we remove these
# variables and new_window variable itself
# we also remove timestamp variable as it obviously don't contribute to outcome as well
# as X variable is a simple row number
# we eliminate variable user_name as we don't want it as a one of regressors
library(dplyr)
har <- data %>%
  select(-unlist(lapply(1:160, function(i) 
              {if (sum(is.na(data[, i])) > dim(data)[1] * .95) i })),
         -new_window, -X, -raw_timestamp_part_1, -raw_timestamp_part_2, 
         -cvtd_timestamp, -user_name, -num_window)

rm(data)

## Sampling

library(caret)
set.seed(1377)
inTrain <- createDataPartition(y = har$classe, p = 0.75, list = FALSE)

training <- har[inTrain,]
testing <- har[-inTrain,]

rm(har)

# Let's see whether our random sample contain a fair amount of observations
# of every user of every classe from original data
options(digits=3)
table(training$user_name, training$classe) /
  table(har$user_name, har$classe)

## Trees
treeFit <- train(classe ~ ., method = "rpart", data = training)
preds <- predict(treeFit, newdata = testing)

(treeTab <- table(testing$classe, preds))
#Accuracy:
sum(diag(treeTab)) / sum (treeTab)

## Preprocessing
goStandard <- function (x) {
  for (i in 1:53)
    x[, i] <- (x[, i] - mean(training[, i])) / sd(training[, i])
  x
}
trainingStd <- goStandard(training)
testingStd <- goStandard(testing)

preProc <- preProcess(trainingStd[,-54], method = "pca", thresh = 0.8)
trainPC <- predict(preProc, trainingStd[,-54])
testPC <- predict(preProc, testingStd[,-54])
  
treePCAFit <- train(x = trainPC,
                y = trainingStd$classe,
                method = "rpart")
predsPCA <- predict(treePCAFit, testPC)

(treePCATab <- table(testing$classe, predsPCA))
#Accuracy:
sum(diag(treePCATab)) / sum (treePCATab)


## Bagging
require(e1071)
bagFit <- train(x = trainPC, 
                y = trainingStd$classe, 
                method = "treebag")
preds <- predict(bagFit, testPC)

## random forest

#modfit <- train(y = training$classe, x = training, method = "rf")
inSmallTrain <- createDataPartition(y = training$classe, p = 0.3, list = FALSE)
smallTraining <- training[inSmallTrain,]

rfSmallFit <- randomForest(smallTraining[,-53], smallTraining$classe)
x <- varImp(rfSmallFit)
x[order(x[,1], decreasing = TRUE),]
mostImp <- head(rownames(x)[order(x[,1], decreasing = TRUE)], 10)

impTrain <- training[, c(mostImp, "classe")]
rfFullFit <- randomForest(impTrain, impTrain$classe)
predTrain <- predict(rfFullFit, testing, type = "class")

(tab <- table(testing$classe, predTrain))
#Accuracy:
sum(diag(treeTab)) / sum (treeTab)

rfFullFit$confusion[,1:5]
