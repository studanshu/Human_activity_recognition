source("fetch_training_data.R")
source("get_features.R")
source("helper.R")
source("train.R")
library("rpart")

classify <- function(test_sample, method = 'window'){
  
  if( method == 'window' ){
    if( file.exists('win_classifier.RData') ){
      classifier = load('win_classifier.RData')
    }
    else{
      classifier = train('window')
    }
    features <- get_window_features(test_sample, train=FALSE)
  }

  else if( method == 'inst' ){
    if( file.exists('inst_classifier.RData') ){
      classifier = load('inst_classifier.RData')
    }
    else{
      classifier = train('inst')
    }
    features <- get_inst_features(test_sample, train=FALSE)
  }
  
  return(class)
}
