source("fetch_data.R")
source("get_features.R")
source("helper.R")
library("rpart")
library("e1071")
library("randomForest")

train <- function(method = 'window' ){
  
  train_data <- fetch_train_data()
  if( method == 'window' ){
    samples <- get_samples(train_data)
    features <- get_window_features(samples)
    classifier <- rpart(class ~ . , data = features, method = "class")
    save(classifier, file = 'objects/win_classifier.RData')
  }
  else if( method == 'inst' ){
    features <- get_inst_features(train_data)
    classifier <- randomForest(class ~ . , data = features, method = "class")
    # classifier <- rpart(class ~ . , data = features, method = "class")
    # classifier <- svm(class ~ . , data = features, method = "class")
    save(classifier, file = 'objects/inst_classifier.RData')
  }
  
  return(classifier)
  
}