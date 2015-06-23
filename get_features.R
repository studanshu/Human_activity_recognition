
get_training_samples <- function( train_data ){

  classes = names(train_data)
  
  train_samples <- list()
  
  # Get training samples(windows) from the training data (2 minute windows with 1 minute overlap)
  
  for (i in seq_along(train_data)){
    
    train_samples[[classes[[i]]]] <- list()
    
    for(j in seq_along(train_data[[i]])){
      
      groups = cut(train_data[[i]][[j]]$date, breaks = "2 min")
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
  if(length(signSignal) > 1){
    z = head(signSignal, n=-1) - tail(signSignal, n=-1)
    return( sum(z != 0) )  
  }
  else{
    return(0)
  }
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

get_window_features <- function( samples, train = TRUE){
  
  # Get features for each 2 minute sample/window
  
  if( train ){
    featureSet <- data.frame(matrix(ncol = 13))
  }
  else{
    featureSet <- data.frame(matrix(ncol = 12))
  }
  
  for ( i in seq_along(samples) ){
    windows <- samples[[i]]
    for ( j in seq_along(windows) ){
      window <- windows[[j]]
      row <- c( avg(window, train), variance(window, train), std(window, train), zcross(window, train) )
      if( train ){
        row <- c( row, i ); 
      }
      featureSet <- rbind( featureSet, row )
    }
  }
  row.names(featureSet) <- NULL
  colNames <- c('meanx', 'meany', 'meanz', 'variancex', 'variancey', 'variancez', 'stdx', 'stdy', 'stdz', 'zcrossx', 'zcrossy', 'zcrossz')
  if( train ){
    colNames <- c( colNames, 'class')
  }
  colnames(featureSet) <- colNames
  return( featureSet )
  
}

get_inst_features <- function( data, train = TRUE ){
  
  if( train ){
    featureSet <- data.frame(meanx=numeric(),
                             meany=numeric(),
                             meanz=numeric(),
                             variancex=numeric(),
                             variancey=numeric(),
                             variancez=numeric(),
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
                             stringsAsFactors=FALSE )
  }
  
  for (i in seq_along(data)){
    for(j in seq_along(data[[i]]) ){
      signal <- data[[i]][[j]]
      n <- nrow(signal)
      if(n<5) break;
      
      f1 <- loess( V2 ~ time, span = 0.5, signal )
      meanx = predict( f1, signal )
      variancex = abs( signal$V2 - meanx )
      f2 <- loess( V3 ~ time, span = 0.5, signal )
      meany = predict( f2, signal )
      variancey = abs( signal$V3 - meany )
      f3 <- loess( V4 ~ time, span = 0.5, signal )
      meanz = predict( f3, signal )
      variancez = abs( signal$V4 - meanz )
      
      m <- cbind(meanx, meany, meanz, variancex, variancey, variancez)
      
      if( train ){
        class = rep(i, n)  
        m <- cbind(m, class)
      }
      
      featureSet <- rbind( featureSet, m )
      
    }
  }
  row.names(featureSet) <- NULL
  return(featureSet)
}