require("RMySQL")
# require("ggplot2")
library("rcharts")

con <- dbConnect(RMySQL::MySQL(), group = "test")

walk_user <- "NC7HX83P5F"
sit_user <- "YJLDC76AR6"
sleep_user <- "0LQO4O9VAL"
deepali <- "YJLDC76AR6"
sit_time_start <- "2015-06-18 14:05:00"
sit_time_end <- "2015-06-18 14:30:00"
sleep_time_start <- "2015-06-17 22:35:00"
sleep_time_end <- "2015-06-17 22:50:00"
walk_time_start <- "2015-06-18 13:40:00"
walk_time_end <- "2015-06-18 14:05:00"

data <- dbReadTable(con, "DataCollection_sensor")
data$time <- strptime(data$time, "%Y-%m-%d %H:%M:%S")

sit_data = data[which(data$time>sit_time_start & data$time<sit_time_end & data$sensor == '1' & data$username == sit_user),]
sleep_data = data[which(data$time>sleep_time_start & data$time<sleep_time_end & data$sensor == '1' & data$username == sleep_user),]
walk_data = data[which(data$time>walk_time_start & data$time<walk_time_end & data$sensor == '1' & data$username == walk_user),]

# ggplot(sit_data, aes(time)) + geom_line(aes(y=value1), colour="red") + geom_line(aes(y=value2), colour="green") + geom_line(aes(y=value3), colour="blue")
# ggplot(sleep_data, aes(time)) + geom_line(aes(y=value1), colour="red") + geom_line(aes(y=value2), colour="green") + geom_line(aes(y=value3), colour="blue")
# ggplot(walk_data, aes(time)) + geom_line(aes(y=value1), colour="red") + geom_line(aes(y=value2), colour="green") + geom_line(aes(y=value3), colour="blue")

m1 <- mPlot(x = 'time', y = c('value1', 'value2', 'value3'), type = 'Line', data = sit_data)
m1$set(pointSize = 0, linewidth = 1)
m1

fresh_data = sit_data[which(sit_data$time>"2015-06-18 13:37:10" & sit_data$time < "2015-06-18 14:05:48" ),]
groups = cut(as.POSIXct(fresh_data$time, tz=""), breaks = "5 min")
new = split(fresh_data, groups)

# variance <- matrix(nrow = 6, ncol = 3)
# print (nrow(new))
# for (i in 1:6){
# 	v1=var(new[[i]]$value1)
# 	v2=var(new[[i]]$value2)
# 	v3=var(new[[i]]$value3)
# 	variance[i,]<-c(v1, v2, v3)
# }