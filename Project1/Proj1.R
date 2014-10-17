Sys.setlocale("LC_TIME", "en_US")
require(ggplot2)

unzip("activity.zip", exdir = "/Users/dfernandezcanon/Documents/Development/R/Project/ReproducibleResearch/Project1")

data <- read.csv("activity.csv", header=TRUE, sep=",", colClasses = c("numeric","character","numeric"))
data$date <- as.Date(data$date, "%Y-%m-%d")

#What is mean total number of steps taken per day?

#aggregate steps per day
stepsPerDay <- aggregate(data$steps, by=list(data$date),FUN = sum, na.rm=TRUE)
colnames(stepsPerDay) <- c("date","steps")
hist(stepsPerDay$steps, breaks = 50, xlab="Total steps taken each day")
mean(stepsPerDay$steps, na.rm=TRUE)
median(stepsPerDay$steps, na.rm=TRUE)

#What is the average daily activity pattern?
dailyAverage <- aggregate(data$steps, by=list(data$interval), FUN = mean, na.rm=TRUE)
colnames(dailyAverage) <- c("interval","steps")
plot(dailyAverage$interval, dailyAverage$steps, type = "l", xlab = "5-minutes interval", ylab ="average number of steps taken")

dailyAverage[which.max(dailyAverage$steps),]

#Imputing missing values

#count the total number of misssing values 
index <- which(is.na(data$steps))
length(index)

#new dataset using the mean for that 5-minute interval
dataNoNA <- data
dataNoNA[index,1] <- dailyAverage[as.factor(dataNoNA[index,3]),2]

#aggregate steps per day
stepsPerDayNoNA <- aggregate(dataNoNA$steps, by=list(dataNoNA$date),FUN = sum)
colnames(stepsPerDayNoNA) <- c("date","steps")
hist(stepsPerDayNoNA$steps, breaks = 50, xlab="Total steps taken each day")
mean(stepsPerDay$steps, na.rm=TRUE)
mean(stepsPerDayNoNA$steps)
median(stepsPerDay$steps, na.rm=TRUE)
median(stepsPerDayNoNA$steps)

#Are there differences in activity patterns between weekdays and weekends?

DayOfWeek <- function(date) {
  if (weekdays(date) %in% c('Saturday', 'Sunday')) {
    return('Weekend')
  } else {
    return('Weekday')
  }
}

dataNoNA$weekpart <- sapply(dataNoNA$date, DayOfWeek)
stepsWeekPart <- aggregate(dataNoNA$steps, by=list(dataNoNA$interval, dataNoNA$weekpart),FUN = mean)
colnames(stepsWeekPart) <- c("interval","weekpart","steps")

ggplot(stepsWeekPart, aes(interval, steps)) + geom_line() + facet_grid(weekpart ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
