## Loading and preprocessing the data
data <- read.csv('activity.csv', header=T)
str(data)
head(data)
data$date <- as.Date(data$date)
str(data)

## Histogram, Mean & Median
sum.steps <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=T)
names(sum.steps) <- c("date", "sum")
hist(sum.steps$sum, breaks=seq(from=0, to=25000, by=2500),
     xlab="Total number of steps each day", main="Histogram")                  
mean(sum.steps$sum, na.rm=T)
median(sum.steps$sum, na.rm=T)

## Average daily activity pattern
avg.steps <- aggregate(x=data$steps, by=list(data$interval), FUN=mean, na.rm=T)
names(avg.steps) <- c("interval", "avg")
plot(avg.steps$interval, avg.steps$avg, type="l", lwd=1, col="red",
xlab="5-min interval", ylab="Avg number of steps", main="Time-series plot")

max(avg.steps$avg)
avg.steps[which(avg.steps$avg == max(avg.steps$avg)), ]

## Missing Values
sum(is.na(data$steps))

na <- which(is.na(data$steps))
avg <- rep(mean(data$steps, na.rm=T))
avg <- round(avg)
data[na, "steps"] <- avg
sum(is.na(data$steps))

sum.steps2 <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=T)
names(sum.steps2) <- c("date", "sum")
hist(sum.steps2$sum, breaks=seq(from=0, to=25000, by=2500),
     xlab="Total number of steps each day", main="Histogram(NA Replaced)")   
mean(sum.steps2$sum, na.rm=T)
median(sum.steps2$sum, na.rm=T)

## Weekdays & Weekends
data$weekdays <- factor(weekdays(data$date))
levels(data$weekdays)
levels(data$weekdays) <- c("weekday", "weekday", "weekday", "weekday", "weekend", "weekend", "weekday")
levels(data$weekdays)

newdata <- aggregate(data$steps, by=list(data$weekdays,  data$interval), mean)
names(newdata) <- c("daytype", "interval", "avgsteps")

library("lattice")
xyplot(avgsteps ~ interval | daytype, newdata, type = "l",
       layout = c(1,2), xlab="5-Min Interval",
       ylab="Avg Steps", main="Avg Number of Steps Per Interval")