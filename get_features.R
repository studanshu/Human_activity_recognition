
get_samples <- function( data ){
  
  classes = names(data)
  
  samples <- list()
  
  # Get samples(windows) from the data (2 minute windows with 1 minute overlap)
  
  for (i in seq_along(data)){
    
    samples[[classes[[i]]]] <- list()
    
    for(j in seq_along(data[[i]])){
      
      groups = cut(data[[i]][[j]]$date, breaks = "2 min")
      samples <- split(data[[i]][[j]], groups)
      samples[[classes[[i]]]] <- c( samples[[classes[[i]]]], samples )
      
      groups = as.POSIXct(groups)
      groups = groups + 60
      curr_samples <- split(data[[i]][[j]], groups)
      samples[[classes[[i]]]] <- c( samples[[classes[[i]]]], curr_samples )
      
    }
    
  }
  
  return(samples)
  
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

zcr <- function( signal, mean = 0 ){
  n = length(signal)
  signSignal = sign(signal - mean)
  if(length(signSignal) > 1){
    z = head(signSignal, n=-1) - tail(signSignal, n=-1)
    return( sum(z != 0) / n )
  }
  else{
    return(0)
  }
}

zcross <- function( window, train = TRUE ){
  if( train ){
    return( c(zcr(window$V2), zcr(window$V3), zcr(window$V4)) )
  }
  else{
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
    classes = names(data)
    levels = 1:length(data)
    featureSet <- data.frame(meanx=numeric(),
                             meany=numeric(),
                             meanz=numeric(),
                             variancex=numeric(),
                             variancey=numeric(),
                             variancez=numeric(),
                             differencex=numeric(),
                             differencey=numeric(),
                             differencez=numeric(),
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
                             differencex=numeric(),
                             differencey=numeric(),
                             differencez=numeric(),
                             zcrossx=numeric(),
                             zcrossy=numeric(),
                             zcrossz=numeric(),
                             stringsAsFactors=FALSE )
  }
  
  for (i in seq_along(data)){
    for(j in seq_along(data[[i]]) ){
      signal <- data[[i]][[j]]
      n <- nrow(signal)
      if( n < 5 ) next
      f1 <- loess( V2 ~ time, span = 0.5, signal )
      meanx = predict( f1, signal )
      variancex = abs( signal$V2 - meanx )
      differencex = abs( head(signal$V2, n=-1) - tail(signal$V2, n=-1) )
      differencex[[length(differencex) + 1]] = differencex[[length(differencex)]]
      zcrossx = zcr( signal$V2, meanx )
      f2 <- loess( V3 ~ time, span = 0.5, signal )
      meany = predict( f2, signal )
      variancey = abs( signal$V3 - meany )
      differencey = abs( head(signal$V3, n=-1) - tail(signal$V3, n=-1) )
      differencey[[length(differencey) + 1]] = differencey[[length(differencey)]]
      zcrossy = zcr( signal$V3, meany )
      f3 <- loess( V4 ~ time, span = 0.5, signal )
      meanz = predict( f3, signal )
      variancez = abs( signal$V4 - meanz )
      differencez = abs( head(signal$V4, n=-1) - tail(signal$V4, n=-1) )
      differencez[[length(differencez) + 1]] = differencez[[length(differencez)]]
      zcrossz = zcr( signal$V4, meanz )
      
      m <- cbind(meanx, meany, meanz, variancex, variancey, variancez, differencex, differencey, differencez, zcrossx, zcrossy, zcrossz)
      
      if( train ){
        class = rep(i, n)  
        m <- cbind(m, class)
      }
      
      featureSet <- rbind( featureSet, m )
      
    }
  }
  row.names(featureSet) <- NULL
  if( train ){
    featureSet$class <- factor( featureSet$class, levels = levels, labels = classes)
  }
  return(featureSet)
}