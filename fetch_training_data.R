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

fetch_test_data <- function(db_name = "test", data_table = "DataCollection_sensor", user = "")
{
  con <- dbConnect(RMySQL::MySQL(), group = db_name)
  
  query = paste0("SELECT * FROM ", data_table, " WHERE username = '", user,"' AND sensor = '1' ;") 
  res<-dbSendQuery(con, query)
  data <- fetch(res, n = -1)
  data$date <- as.POSIXct(data$time, tz="")
  data$time <- as.numeric(data$date)
  data$V1 <- data$time
  names(data)[names(data)=="value1"] <- "V2"
  names(data)[names(data)=="value2"] <- "V3"
  names(data)[names(data)=="value3"] <- "V4"
  test <- list()
  test[[1]] <- list()
  test[[1]][[1]] <- data
  dbDisconnect(con)
  return(test)
}