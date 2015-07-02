require("RMySQL")
source("helper.R")
require("ggplot2")

fetch_train_data <- function(data_path='data')
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
      
#       # visualize ggplot
#       p <- ggplot(data=data, aes(x=time)) +
#         geom_line(aes(y=V2, color="x"), size = 1, lineend = "round") +
#         geom_line(aes(y=V3, color="y"), size = 1, lineend = "round") +
#         geom_line(aes(y=V4, color="z"), size = 1, lineend = "round") +
#         scale_color_manual("",
#                            breaks = c("x", "y", "z"),
#                            values = c("blue", "red", "green")) +
#         theme_bw(base_size=14) +
#         ggtitle(files[[j]])
#       print(p)
      
    }
    
    train_data[[classes[[i]]]] <- activity_data
  }
  
  return(train_data)
}

fetch_test_data <- function(db_name = "test", user =  NULL, lowid, highid)
{
  
  # Connect to mysql server
  con <- dbConnect(RMySQL::MySQL(), group = db_name)
  
  # Get accelerometer data of the user
  query = paste0("SELECT * FROM DataCollection_sensor WHERE username = '", user ,"'  AND sensor = '1' ORDER BY username, time;") 
  res<-dbSendQuery(con, query)
  test_data <- fetch(res, n = -1)
  
  test <- list()
  test[[user]] <- list()
  
  data <- subset( test_data, test_data$id > lowid & test_data$id < highid )
  data$date <- as.POSIXct(data$time, tz="")
  data$time <- as.numeric(data$date)
  data$V1 <- data$time
  names(data)[names(data)=="value1"] <- "V2"
  names(data)[names(data)=="value2"] <- "V3"
  names(data)[names(data)=="value3"] <- "V4"
  test[[user]][[1]] <- data
    
  dbDisconnect(con)
  
  return(test)
}