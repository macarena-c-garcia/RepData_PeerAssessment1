## Sample code for using melt and tapply instead of aggregate
daysum.DF <- melt(tapply(DF$steps, INDEX=DF$date, FUN=sum, na.rm=TRUE))

## The nice thing about aggregate() I find is that you can use the formula interface.
## So if I want the maximum number of steps taken each day I can just write
aggregate(steps ~ date, dataset, max)

setwd("/Users/usaid/Desktop/PeerAssessment2/") 