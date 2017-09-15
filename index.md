# HAR Course Project
Sergey Sambor  
September 11, 2017  




## Foreword
In this course project we take Human Activity Data to try to pedict in wich manner people have done an excercise, how well have they done it.  

## Sampling data
We split our data from training file into two parts: one for training models, another - testing.

```r
set.seed(1377)
inTrain <- createDataPartition(y = data$classe, p = 0.75, list = FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]
```
Let's see whether our training random sample contain a fair amount of observations from every user with every 'classe' from original data

```
##           
##                A     B     C     D     E
##   adelmo   0.745 0.746 0.741 0.699 0.733
##   carlitos 0.745 0.739 0.781 0.761 0.741
##   charles  0.747 0.744 0.740 0.774 0.751
##   eurico   0.766 0.762 0.738 0.771 0.749
##   jeremy   0.749 0.771 0.752 0.739 0.778
##   pedro    0.748 0.747 0.754 0.746 0.755
```

## Cleaning data and some exploratory data analysis

```r
dim(training)
```

```
## [1] 14718   160
```
Data has 14718 observations and 160 variables.  
But many columns consist of mostly NAs 

```
##  [1]     0     0     0     0     0     0     0     0     0     0     0
## [12] 14426 14439 14718 14425 14439 14718 14416 14416 14426 14416 14416
## [23] 14426 14416 14416 14426 14416 14416 14416 14416 14416 14416 14416
## [34] 14416 14416 14416     0     0     0     0     0     0     0     0
```
These columns are

```r
mostlyNA <- unlist(lapply(1:160, function(i) {if (sum(is.na(training[, i])) >
                                                  dim(training)[1] * .95)
                                              i } ))
head(colnames(training)[mostlyNA], 26)
```

```
##  [1] "kurtosis_roll_belt"   "kurtosis_picth_belt"  "kurtosis_yaw_belt"   
##  [4] "skewness_roll_belt"   "skewness_roll_belt.1" "skewness_yaw_belt"   
##  [7] "max_roll_belt"        "max_picth_belt"       "max_yaw_belt"        
## [10] "min_roll_belt"        "min_pitch_belt"       "min_yaw_belt"        
## [13] "amplitude_roll_belt"  "amplitude_pitch_belt" "amplitude_yaw_belt"  
## [16] "var_total_accel_belt" "avg_roll_belt"        "stddev_roll_belt"    
## [19] "var_roll_belt"        "avg_pitch_belt"       "stddev_pitch_belt"   
## [22] "var_pitch_belt"       "avg_yaw_belt"         "stddev_yaw_belt"     
## [25] "var_yaw_belt"         "var_accel_arm"
```
In total 100 columns. These variables are explained by a variable new_window, which are 'yes' for the rows that contain not-NA values. With the names min, max, avg, var, stddev etc. it's obvious that these variables contain summarising info for each window. Therefore, we remove these variables and new_window variable itself.  
We also remove the next variables that are not covariants in our model: timestamp variables, X variable is just a row number, user_name and num_window is just a window number.  

```r
training <- training %>%
  select(-mostlyNA, -new_window, -X, -raw_timestamp_part_1, -raw_timestamp_part_2, 
         -cvtd_timestamp, -user_name, -num_window)
```

```r
dim(training)
```

```
## [1] 14718    53
```
Final data has only 53 variables with the last one as an outcome. Every covariate is a numeric value while outcome is a factor with 5 levels: A,B,C,D,E.  We perform the same cleaning on testing data sample.  


## Training and predicting
We are unable to use "glm" model as the outcome has more than two classes.  
Trees model using "rpart" from caret will look like this:

```r
treeFit <- train(classe ~ ., method = "rpart", data = training)
preds <- predict(treeFit, newdata = testing)
(treeTab <- table(testing$classe, preds))
```

```
##    preds
##        A    B    C    D    E
##   A 1263   17  111    0    4
##   B  386  313  250    0    0
##   C  406   27  422    0    0
##   D  380  122  302    0    0
##   E  121  130  234    0  416
```
Training with trees gives us very little accuracy: 0.492  
But it takes too long for more sophisticated methods, like bagging, random forest or boosting to train a model fit. For this issue we create a smaller partition of our traning data to figure out which covariates are the most important ones.

```r
inSmallTrain <- createDataPartition(y = training$classe, p = 0.3, list = FALSE)
smallTraining <- training[inSmallTrain,]
rfSmallFit <- randomForest(smallTraining[,-53], smallTraining$classe)
x <- varImp(rfSmallFit)
x[order(x[,1], decreasing = TRUE),]
```

```
##  [1] 258.4 180.7 176.3 159.0 142.4 135.3 114.0 106.8 105.8  98.7  97.4
## [12]  95.6  91.5  78.0  76.1  70.0  67.5  67.2  62.9  62.5  60.1  59.9
## [23]  59.6  58.5  56.3  55.1  53.3  51.7  50.3  47.0  46.9  43.9  41.7
## [34]  38.6  38.5  36.0  35.3  34.5  33.2  31.7  31.3  31.3  30.8  30.1
## [45]  26.7  26.7  24.4  23.7  23.7  23.6  23.3  18.9
```

```r
mostImp <- head(rownames(x)[order(x[,1], decreasing = TRUE)], 12)
```
We take the fist 12 covariates, that are roll_belt, pitch_forearm, yaw_belt, magnet_dumbbell_z, magnet_dumbbell_y, pitch_belt, roll_forearm, magnet_dumbbell_x, roll_dumbbell, magnet_belt_y, accel_belt_z, accel_dumbbell_y.  
And build a model fit using random forest with all training data within selected columns.

```r
impTrain <- training[, c(mostImp, "classe")]
rfFullFit <- randomForest(classe ~ ., data = impTrain)
predTest <- predict(rfFullFit, newdata = testing[, c(mostImp, "classe")])
(tab <- table(testing$classe, predTest))
```

```
##    predTest
##        A    B    C    D    E
##   A 1377    9    6    3    0
##   B   10  917   16    6    0
##   C    0   11  839    5    0
##   D    0    2    5  797    0
##   E    0    2    2    3  894
```
Accuracy: 0.984  

## Summary
Model built on random forest has high accuracy 98.369% and the expected out of the sample error is 0.016. For reaching thet level of accuracy 12 covarians were used: roll_belt, pitch_forearm, yaw_belt, magnet_dumbbell_z, magnet_dumbbell_y, pitch_belt, roll_forearm, magnet_dumbbell_x, roll_dumbbell, magnet_belt_y, accel_belt_z, accel_dumbbell_y. With that number of covariants took not too long, not longer than "rpart" with all covariates included.
