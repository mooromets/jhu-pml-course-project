urlTraining <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTesting <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
fileTraining <- "training.csv"
fileTesting <- "testing.csv"
#download.file(url = urlTraining, fileTraining)
#download.file(url = urlTesting, fileTesting)

data <- read.csv(fileTraining, na.strings=c("NA", "", "#DIV/0!"))

## Exploratory analysis

library(caret)
set.seed(1377)
inTrain <- createDataPartition(y = data$classe, p = 0.75, list = FALSE)

training <- data[inTrain,]
testing <- data[-inTrain,]

