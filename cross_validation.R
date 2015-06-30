## Usage:

# train_data <- fetch_train_data()
# features <- get_inst_features(train_data)
# result <- crossval(class ~ ., features)

source('get_features.R')
source('fetch_data.R')
library("rpart")

crossval <- function(form, x, fold = 10, cp = 0.01) {
  n <- nrow(x)
  prop <- n%/%fold
  set.seed(7)
  newseq <- rank(runif(n))
  k <- as.factor((newseq - 1)%/%prop + 1)
  
  y <- unlist(strsplit(as.character(form), " "))[2]
  vec.accuracy <- vector(length = fold)
  for (i in seq(fold)) {
    # It depends on which classification method you use
    fit <- rpart(form, data = x[k != i, ], method = "class")
    fcast <- predict(fit, newdata = x[k == i, ], type = "class")
    cm <- table(x[k == i, y], fcast)
    accuracy <- (cm[1, 1] + cm[2, 2])/sum(cm)
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
result <- crossval(class ~ ., features)