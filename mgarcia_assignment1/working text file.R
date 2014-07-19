## Sample code for using melt and tapply instead of aggregate
daysum.DF <- melt(tapply(DF$steps, INDEX=DF$date, FUN=sum, na.rm=TRUE))

## The nice thing about aggregate() I find is that you can use the formula interface.
## So if I want the maximum number of steps taken each day I can just write
aggregate(steps ~ date, dataset, max)

library(VIM)
set.seed(1328311)
impDF <- hotdeck(x, variable = "steps", ord_var = "date", domain_var = "interval", donorcond = ">50", impNA = TRUE)

dateDF <- transform(newDF, weekend=as.POSIXlt(date, format='%Y/%m/%d')$wday %in% c(0, 6))
dateDF$dayOfWeek <- "weekday" 
dateDF$dayOfWeek[dateDF$weekend == "TRUE"]  <- "weekend" 
dateDF$dayOfWeek <- as.factor(dateDF$dayOfWeek)
dateDF$interval <- factor(dateDF$interval)

weekEnd <- subset(dateDF, dayOfWeek == "weekend")
weekEndMean <- aggregate(steps ~ interval, data = weekEnd, mean)
weekDay <- subset(dateDF, dayOfWeek == "weekday")
weekDayMean <- aggregate(steps ~ interval, data = weekDay, mean)

plotEND <- ggplot(data=weekEndMean, aes(x=weekEndMean$interval, y=weekEndMean$steps)) + geom_line() + labs(title = "WEEKEND") + labs(x = "interval") + labs(y = "No. of steps") + geom_line(size=1, col = "blue") + coord_cartesian(ylim = c(0, 200))
plotDAY <- ggplot(data=weekDayMean, aes(x=weekDayMean$interval, y=weekDayMean$steps)) + geom_line() + labs(title = "WEEKDAY") + labs(x = "interval") + labs(y = "No. of steps") + geom_line(size=1, col = "red") + coord_cartesian(ylim = c(0, 200))
library(gridExtra)
grid.arrange(plotEND, plotDAY, nrow = 2)
