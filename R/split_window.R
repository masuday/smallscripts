split_window <- function(x, width = 2){
   if(width<1){ stop("width should be a positive integer.") }
   x <- as.integer(x)
   minx <- min(x)
   maxx <- max(x)-width+1
   if(maxx < minx){ stop("width is out of range.") }
   return( lapply(minx:maxx, function(x,n){seq(x,x+n-1)}, width) )
}

split_window(5:10, width=2)
split_window(c(5,4,4,2,2,1,2,1,3,3,4,2), width=2)

data <- data.frame(1:18,c(5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,10))
colnames(data) <- c("y","day")
dayslist <- split_window(data$day,width=2)
subset(data, data$day %in% dayslist[[1]], y)
daysmean <- sapply(dayslist, mean)

lapply(dayslist, function(days){ subset(data, data$day %in% days, y) })
ymean <- sapply(lapply(dayslist, function(days){ subset(data, data$day %in% days, y) }), function(x){sapply(x,mean)} )

split_window(data$day,width=2) |>
  lapply(function(days){ subset(data, data$day %in% days, y) }) |>
  sapply(function(x){ sapply(x,mean) }) -> ymean2

