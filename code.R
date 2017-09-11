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
# variables, rows where new_window is 'yes' and new_window variable itself
library(dplyr)
data <- data %>%
  filter(new_window == "no") %>%
  select(-unlist(lapply(1:160, function(i) {if (sum(is.na(data[, i])) > dim(data)[1] * .95) i })),
         -new_window) %>%
  # replace three columns for datetime with a single one (numeric)
  mutate(raw_timestamp = as.double(raw_timestamp_part_1 * 10^6 + raw_timestamp_part_2)) %>%
  select (-raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)



library(caret)
set.seed(1377)
inTrain <- createDataPartition(y = data$classe, p = 0.75, list = FALSE)

training <- data[inTrain,]
testing <- data[-inTrain,]

