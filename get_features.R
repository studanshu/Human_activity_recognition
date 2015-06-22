source("fetch_training_data.R")
source("helper.R")

train_data <- fetch_train_data()

# visualize(train_data$sit[[1]], 'V1', c('V2', 'V3', 'V4'))

classes = names(train_data)

train_samples <- list()

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

