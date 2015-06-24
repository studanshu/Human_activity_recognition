source("fetch_training_data.R")
source("get_features.R")
source("helper.R")
library("rpart")

train <- function(method = 'window' ){
  
  train_data <- fetch_data()
  if( method == 'window' ){
    samples <- get_samples(train_data)
    features <- get_window_features(samples)
    classifier <- rpart(class ~ meanx + meany + meanz + variancex + variancey + variancez + stdx + stdy + stdz + zcrossx + zcrossy + zcrossz, data = features, method = "class")
    save(classifier, file = 'objects/win_classifier.RData')
  }
  else if( method == 'inst' ){
    features <- get_inst_features(train_data)
    classifier <- rpart(class ~ meanx + meany + meanz + variancex + variancey + variancez , data = features, method = "class")
    save(classifier, file = 'objects/inst_classifier.RData')
  }
  
  return(classifier)
  
}