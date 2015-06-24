require("RMySQL")
source("helper.R")

fetch_data <- function(data_path='data')
{
  paths <- list.dirs(path=data_path, recursive = FALSE)
  classes <- list.dirs(path=data_path, recursive = FALSE, full.names = FALSE)
  
  train_data = list()
  for ( i in seq_along(paths) ){
    
    files <- list.files(path=paths[[i]], recursive = FALSE, full.names = TRUE)
    activity_data = list()
    
    for ( j in seq_along(files) ){
      data = read.csv(files[[j]], header=FALSE, sep=';')
      #data = data[,3:ncol(data)-1]
      #write.table(data, file=files[[j]], sep=';', row.names = FALSE, col.names = FALSE)
      data$time <- data$V1/1000
      data$date <- as.POSIXct(data$time, origin="1970-01-01")
      activity_data[[j]] <- data
    }
    
    train_data[[classes[[i]]]] <- activity_data
  }
  
  return(train_data)
}

fetch_test_data <- function(db_name = "test", data_table = "DataCollection_sensor")
{
  con <- dbConnect(RMySQL::MySQL(), group = db_name)
  
  data <- dbReadTable(con, data_table)
  as.POSIXct(data$time, tz="")
  
  sensor_data = data[which(data$sensor == '1' & data$username == sit_user),]
  write.csv(sensor_data, file = "data/sensor_data.csv", row.names=FALSE)
  
  sit_data = data[which(data$time>sit_time_start & data$time<sit_time_end & data$sensor == '1' & data$username == sit_user),]
  write.csv(sit_data, file = "data/sit_data.csv", row.names=FALSE)
  
  sleep_data = data[which(data$time>sleep_time_start & data$time<sleep_time_end & data$sensor == '1' & data$username == sleep_user),]
  write.csv(sleep_data, file = "data/sleep_data.csv", row.names=FALSE)
  
  walk_data = data[which(data$time>walk_time_start & data$time<walk_time_end & data$sensor == '1' & data$username == walk_user),]
  write.csv(walk_data, file = "data/walk_data.csv", row.names=FALSE)
  
  test_data = list('sensor' = sensor_data, 'sit' = sit_data, 'sleep' = sleep_data, 'walk' = walk_data)
  return(test_data)
}