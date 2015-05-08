# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
- load file


```r
activity <- read.csv("activity.csv", header=TRUE)
totalSteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

## What is mean total number of steps taken per day?
- Calculate the total number of steps taken per day and plot hist


```r
hist(totalSteps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
spdMean <- mean(totalSteps$steps)
spdMedian <- median(totalSteps$steps)
```

- The mean total number of steps taken per day is 1.0766189\times 10^{4} steps.
- The median total number of steps taken per day is 10765 steps.

## What is the average daily activity pattern?

- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsInterval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(steps ~ interval, data = stepsInterval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepsInterval[which.max(stepsInterval$steps), ]$interval
```

```
## [1] 835
```

## Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. I just use the mean for that 5-minute interval, etc.


```r
interval2steps <- function(interval) {
    stepsInterval[stepsInterval$interval == interval, ]$steps
}
```

- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activityFilled <- activity  # Make a new dataset with the original data
count = 0  # Count the number of data filled in
for (i in 1:nrow(activityFilled)) {
    if (is.na(activityFilled[i, ]$steps)) {
        activityFilled[i, ]$steps <- interval2steps(activityFilled[i, ]$interval)
        count = count + 1
    }
}
cat("Total", count, "NA values were filled.\n\r")
```

```
## Total 2304 NA values were filled.
## 
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalSteps2 <- aggregate(steps ~ date, data = activityFilled, sum)
hist(totalSteps2$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 


```r
cat("The mean total number of steps taken per day is ", mean(totalSteps2$steps), " steps")
```

```
## The mean total number of steps taken per day is  10766.19  steps
```

```r
cat("The median total number of steps taken per day is ", median(totalSteps2$steps), "steps")
```

```
## The median total number of steps taken per day is  10766.19 steps
```

- Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

: The mean value is the same as the value before imputing missing data because we put the mean value for that particular 5-min interval. The median value shows a little difference : but it depends on where the missing values are.

## Are there differences in activity patterns between weekdays and weekends?
` Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activityFilled$day = ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6 == 0, "weekend", "weekday")
# For Sunday and Saturday : weekend, Other days : weekday
activityFilled$day = factor(activityFilled$day, levels = c("weekday", "weekend"))
```
` Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
stepsInterval2 = aggregate(steps ~ interval + day, activityFilled, mean)
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.3
```

```r
xyplot(steps ~ interval | factor(day), data = stepsInterval2, aspect = 1/2, 
    type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
