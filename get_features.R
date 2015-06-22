source("fetch_training_data.R")
source("helper.R")

get_training_samples <- function(){
  
  train_data <- fetch_train_data()

  # visualize(train_data$sit[[1]], 'V1', c('V2', 'V3', 'V4'))

  classes = names(train_data)
  
  train_samples <- list()
  
  # Get training samples(windows) from the training data (2 minute windows with 1 minute overlap)
  
  for (i in (1:length(train_data))){
    
    train_samples[[classes[[i]]]] <- list()
    
    for(j in (1:length(train_data[[i]]))){
      
      groups = cut(train_data[[i]][[j]]$V1, breaks = "2 min")
      samples <- split(train_data[[i]][[j]], groups)
      train_samples[[classes[[i]]]] <- c( train_samples[[classes[[i]]]], samples )
      
      groups = as.POSIXct(groups)
      groups = groups + 60
      samples <- split(train_data[[i]][[j]], groups)
      train_samples[[classes[[i]]]] <- c( train_samples[[classes[[i]]]], samples )
      
    }
    
  }
  
  return(train_samples)
  
}

avg <-function( window, train = TRUE ){
  if( train ){
    return( c(mean(window$V2), mean(window$V3), mean(window$V4)) )
  }
  else{
    return( c(mean(window$value1), mean(window$value2), mean(window$value3)) )
  }
}

variance <-function( window, train = TRUE ){
  if( train ){
    return( c(var(window$V2), mean(window$V3), mean(window$V4)) )
  }
  else{
    return( c(var(window$value1), mean(window$value2), mean(window$value3)) )
  }
}

std <-function( window, train = TRUE ){
  if( train ){
    return( c(sd(window$V2), mean(window$V3), mean(window$V4)) )
  }
  else{
    return( c(sd(window$value1), mean(window$value2), mean(window$value3)) )
  }
}

zcr <- function( signal ){
  signSignal = sign(signal)
  z = head(signSignal, n=-1) - tail(signSignal, n=-1)
  return( sum(z != 0) )
}

zcross <- function( window, train = TRUE ){
  if( train ){
    window$V1 <- scale( window$V1 )
    window$V2 <- scale( window$V2 )
    window$V3 <- scale( window$V3 )
    return( c(zcr(window$V2), zcr(window$V3), zcr(window$V4)) )
  }
  else{
    window$value1 <- scale( window$value1 )
    window$value2 <- scale( window$value2 )
    window$value3 <- scale( window$value3 )
    return( c(zcr(window$value1), zcr(window$value2), zcr(window$value3)) )
  }
}

get_features <- function( samples, train = TRUE){
  
  # Get features for each 2 minute sample/window
  
  if( train ){
    featureSet <- data.frame(meanx=numeric(),
                             meany=numeric(),
                             meanz=numeric(),
                             variancex=numeric(),
                             variancey=numeric(),
                             variancez=numeric(),
                             stdx=numeric(),
                             stdy=numeric(),
                             stdz=numeric(),
                             zcrossx=numeric(),
                             zcrossy=numeric(),
                             zcrossz=numeric(),
                             class=numeric(),
                             stringsAsFactors=FALSE )
  }
  else{
    featureSet <- data.frame(meanx=numeric(),
                             meany=numeric(),
                             meanz=numeric(),
                             variancex=numeric(),
                             variancey=numeric(),
                             variancez=numeric(),
                             stdx=numeric(),
                             stdy=numeric(),
                             stdz=numeric(),
                             zcrossx=numeric(),
                             zcrossy=numeric(),
                             zcrossz=numeric(),
                             stringsAsFactors=FALSE )
  }
  
  for ( i in (1:length(samples)) ){
    windows <- samples[[i]]
    for ( j in (1:length(windows)) ){
      window <- windows[[j]]
      row <- c( avg(window, train), variance(window, train), std(window, train), zcross(window, train) )
      if( train ){
        row <- c( row, i ); 
      }
      featureSet = rbind( featureSet, row )
    }
  }
  return( featureSet )
}