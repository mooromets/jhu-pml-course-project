#########
tmp <- data %>% filter(new_window == "yes")
plot(tmp$classe, tmp$avg_yaw_forearm)


## GLM 
# can be used for only 2 classes outome
# use multinom instead 
# https://stat.ethz.ch/R-manual/R-devel/library/nnet/html/multinom.html
unlist(lapply(1:53, function(i) {training(har[, i])}))
unlist(lapply(1:53, function(i) {training(har[, i])}))

goStandard <- function (x) {
  for (i in 1:53)
    x[, i] <- (x[, i] - mean(training[, i])) / sd(training[, i])
  x
}
trainingStd <- goStandard(training)
unlist(lapply(1:53, function(i) {mean(trainingStd[, i])}))
unlist(lapply(1:53, function(i) {sd(trainingStd[, i])}))

preProc <- preProcess(trainingStd[,-54], method = "pca", thresh = 0.9)
trainPC <- predict(preProc, trainingStd[,-54])
glmFit <- train(x = trainPC,
                y = trainingStd$classe,
                method = "glm")
warnings()
