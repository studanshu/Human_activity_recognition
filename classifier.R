source("get_features.R")
source("helper.R")
source("train.R")
require('RMySQL')
source("fetch_data.R")
require('rjson')
library("rpart")

classify <- function(method = 'window', test_sample){
      
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

main_classifier <- function( user ){
  
  db_name = 'test'
  con <- dbConnect(RMySQL::MySQL(), group = db_name)
  
  result <- list()
  
  # Get session info for the user
  query = paste0("SELECT id, sensor, username FROM DataCollection_sensor WHERE username = '", user ,"'  AND sensor in ( 'OnResumeStart' ) ORDER BY username, time;") 
  res<-dbSendQuery(con, query)
  stops <- fetch(res, n = -1)
  
  for( j in seq(1,nrow(stops)) ){
    lowid <- stops[j,1]
    highid <- stops[j+1,1]
    test_sample <- fetch_test_data( user = user, lowid = lowid, highid = highid )
    result[[j]] = classify(method='inst', test_sample )
  }
  
  dbDisconnect(con)
  
  return( toJSON(result) )
}