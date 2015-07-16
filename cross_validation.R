## Usage:

# train_data <- fetch_train_data()
# features <- get_inst_features(train_data)
# result <- crossval(class ~ ., features)

source('get_features.R')
source('fetch_data.R')
library("rpart")
library("e1071")
library("neuralnet")
library("randomForest")

crossval <- function(form, x, fold = 10, cp = 0.01) {
  n <- nrow(x)
  prop <- n%/%fold
  set.seed(7)
  newseq <- rank(runif(n))
  k <- as.factor((newseq - 1)%/%prop + 1)
  
  y <- unlist(strsplit(as.character(form), " "))[2]
  vec.accuracy <- vector(length = fold)
  for (i in seq(fold)) {
    
    data = x[k != i, ]
    testdata = x[k == i, ]
    
    # Decision Tree
    fit <- rpart(form, data, method = "class")
    
    # SVM
    # fit <- svm(form, data)
    
    # Random Forest
    # fit <- randomForest(form, data)
    
    # Neural Network
    # fit <- neuralnet(form, data, hidden=3)
    
    # print(fit)
    
    # fcast <- compute(fit, testdata)$net.result
    fcast <- predict(fit, testdata, type = "class")
    
    cm <- ifelse(x[k == i, y] == fcast,1,0)
    accuracy <- sum(cm)/length(cm)
    vec.accuracy[i] <- accuracy
  }
  avg.accuracy <- mean(vec.accuracy)
  avg.error <- 1 - avg.accuracy
  cv <- data.frame(Accuracy = avg.accuracy, Error = avg.error)
  return(cv)
}

train_data <- fetch_train_data()
features <- get_inst_features(train_data)
features <- features[sample(nrow(features)),]
formula <- class ~ meanx + meany + meanz + variancex + variancey + variancez + differencex + differencey + differencez + zcrossx + zcrossy + zcrossz
result <- crossval(formula, features)