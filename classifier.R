source("fetch_training_data.R")
source("get_features.R")
source("helper.R")
library("rpart")

train <- function(method = 'window' ){
  
  train_data <- fetch_train_data()
  if( method == 'window' ){
    samples <- get_training_samples(train_data)
    features <- get_window_features(samples)
    classifier <- rpart(class ~ meanx + meany + meanz + variancex + variancey + variancez + stdx + stdy + stdz + zcrossx + zcrossy + zcrossz, data = features, method = "class")
  }
  else if( method == 'inst' ){
    features <- get_instantaneous_features(train_data)
    classifier <- rpart(class ~ meanx + meany + meanz + variancex + variancey + variancez , data = features, method = "class")
  }
  return(classifier)
  
}

classify <- function(test_sample, method = 'window'){
  
  if( method == 'window' ){
    features <- get_window_features(test_sample, train=FALSE)
  }
  else if( method == 'inst' ){
    features <- get_inst_features(test_sample, train=FALSE)
  }
  
}
