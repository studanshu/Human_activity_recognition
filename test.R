#!/usr/bin/env Rscript

require('RMySQL')
source("classifier.R")
source("fetch_data.R")

db_name = 'test'
con <- dbConnect(RMySQL::MySQL(), group = db_name)

# args <- commandArgs(TRUE)
# user <- as.character(args[1])
result <- list()
len <- list()

# Get session info for the user
query = paste0("SELECT id, sensor, username FROM DataCollection_sensor WHERE username = '", user ,"'  AND sensor in ( 'OnResumeStart' ) ORDER BY username, time;") 
res<-dbSendQuery(con, query)
stops <- fetch(res, n = -1)

for( j in seq(1,nrow(stops)) ){
  lowid <- stops[j,1]
  highid <- stops[j+1,1]
  test_sample <- fetch_test_data( user = user, lowid = lowid, highid = highid )
  len[[j]] = length(test_sample)
  result[[j]] = classify(method='inst', test_sample )
}

dbDisconnect(con)
