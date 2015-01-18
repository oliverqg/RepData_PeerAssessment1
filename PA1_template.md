# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv",sep=",",na.strings="NA",header=TRUE)
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
dailysteps <- ddply(activity,.(date),summarize, steps =sum(steps))
ggplot(dailysteps) + geom_histogram(aes(x=steps))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
mean <- mean(dailysteps$steps,na.rm=TRUE)
med <- median(dailysteps$steps,na.rm=TRUE)
```
The mean total number of steps is 10766.1886792453, and the median is 10765

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avsteps_interval <- ddply(activity,.(interval),summarize,avgsteps=mean(steps,na.rm=TRUE))
ggplot(avsteps_interval) + geom_line(aes(x=interval,y=avgsteps))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxsteps <- max(avsteps_interval$avgsteps)
avs <- avsteps_interval[avsteps_interval$avgsteps==maxsteps,"interval"]
```

The interval with the maximun number of steps is 835 with 206.1698113 steps in average.

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
The number of missing values is: 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
avgsteps <- function(x) {
  y <- avsteps_interval[avsteps_interval==x,"avgsteps"]
}

fixed_activity <- activity
fixed_activity$steps <- ifelse(is.na(fixed_activity$steps),avgsteps(fixed_activity$interval),fixed_activity$steps)
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
fixed_activity <- activity
fixed_activity$steps <- ifelse(is.na(fixed_activity$steps),avgsteps(fixed_activity$interval),fixed_activity$steps)
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
dailysteps2 <- ddply(fixed_activity,.(date),summarize, steps =sum(steps))
ggplot(dailysteps2) + geom_histogram(aes(x=steps))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean2 <- mean(dailysteps2$steps,na.rm=TRUE)
med2 <- median(dailysteps2$steps,na.rm=TRUE)
```
The total number of steps mean after fixing missing values is 10766.1886792453, and the median is 10766.1886792453

The mean for first part 10766.1886792453 and for second part 10766.1886792453 are the same.

The median for first part 10765 and for second part 10766.1886792453 are practically equal



## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
fixed_activity <- fixed_activity %>% mutate(date=as.Date(date)) %>% 
  mutate(we = as.factor(ifelse((weekdays(date) %in% c('Saturday','Sunday')),'weekend','weekday')))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
avsteps_interval2 <- ddply(fixed_activity,.(interval,we),summarize,avgsteps=mean(steps,na.rm=TRUE))
xyplot(avsteps_interval2$avgsteps~avsteps_interval2$interval|avsteps_interval2$we,type="l",layout=c(1,2),xlab="Interval",ylab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
