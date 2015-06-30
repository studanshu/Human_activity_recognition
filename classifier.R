source("fetch_data.R")
source("get_features.R")
source("helper.R")
source("train.R")
library("rpart")

classify <- function(test_sample = NULL, method = 'window', user=""){
  
  if( is.null(test_sample) )
    test_sample <- fetch_test_data( user=user)
      
  if( method == 'window' ){
    if( file.exists('objects/win_classifier.RData') ){
      load('objects/win_classifier.RData')
    }
    else{
      classifier = train('window')
    }
    samples <- get_samples(test_sample)
    features <- get_window_features(samples, train=FALSE)
  }
  else if( method == 'inst' ){
    if( file.exists('objects/inst_classifier.RData') ){
      load('objects/inst_classifier.RData')
    }
    else{
      classifier = train('inst')
    }
    features <- get_inst_features(test_sample, train=FALSE)
  }
  
  cdis <- predict(classifier, features, type="class")
  n <- length(cdis)
  ctable <- table(cdis)
  class <- ctable / n
  return(class)
}