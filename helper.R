library("rCharts")

visualize <- function(data, x_value, y_value, type = 'Line'){
	m1 <- mPlot(x = x_value, y = y_value, type = type, data = data)
	m1$set(pointSize = 0, lineWidth = 1)
	m1	
}