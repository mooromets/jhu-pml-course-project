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
library(dplyr)
har <- data %>%
  select(-unlist(lapply(1:160, function(i) {if (sum(is.na(data[, i])) > dim(data)[1] * .95) i })),
         -new_window, -X, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)

rm(data)

## Sampling

library(caret)
set.seed(1377)
inTrain <- createDataPartition(y = har$classe, p = 0.75, list = FALSE)

training <- har[inTrain,]
testing <- har[-inTrain,]

# Let's see whether our random sample contain a fair amount of observations
# of every user of every classe from original data
options(digits=3)
table(training$user_name, training$classe) /
  table(har$user_name, har$classe)
