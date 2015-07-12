---
title: "Reproducible Research - Peer Assessment 1"
author: "Konrad Gorski"
date: "Sunday, July 12, 2015"
output: html_document
---

# Reading and preprocessing data

Let's read our data.

```r
if (!file.exists("activity.csv")) {
    unzip("activity.zip")
}
data <- read.csv(file = "activity.csv")
head(data, xlab = "Daily steps", ylab = "Frequency")
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

The only required preprocessing step would be replacing the date field with a POSIXt format.


```r
data$date <- as.Date(data$date)
```


# What is mean total number of steps taken per day?

First, let's see what's the distribution of the daily steps count.

```r
daily_steps <- tapply(data$steps, as.factor(data$date), sum, na.rm=TRUE)
hist(daily_steps, breaks=50, main="Histogram of daily steps count")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


```r
steps_mean <- mean(daily_steps)
steps_median <- median(daily_steps)
```

The mean of the total number of steps taken per day is **9354.23**, while the median value is **10395** steps.


# What is the average daily activity pattern?


```r
steps_avg <- tapply(data$steps, as.factor(data$interval), mean, na.rm=TRUE)
intervals <- seq(0, 24*60 - 1, 5)
timelabel <- sprintf("%02d:%02d", intervals/60, intervals %% 60)
plot(steps_avg, type = "l", axes=F,
     xlab="Time of day",
     ylab="Average steps made")
box()
axis(1, at=seq(0, 287, 48), labels=timelabel[c(TRUE, rep(FALSE, 47))])
title("Daily activity pattern")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
max_interval <- as.integer(names(steps_avg[steps_avg == max(steps_avg)]))
max_interval_start <- sprintf("%02.0f:%02.0f", max_interval / 100, max_interval %% 100)
max_interval_end <- sprintf("%02.0f:%02.0f", (max_interval + 5) / 100, (max_interval + 5) %% 100)
```
The maximum number of steps is done in interval **08:35 - 08:40**.


## Imputing missing values

```r
total_na <- sum(is.na(data$steps))
```
Total number of missing values is **2304**.

Let's replace the missing values with the average value for this 5-min interval.


```r
data$steps_filled <- data$steps
na_filter = is.na(data$steps)
data$steps_filled[na_filter] <- as.integer(
    rep(steps_avg, 
    as.integer(data$date[length(data$date)]-data$date[1])+1)[na_filter])
daily_steps_filled <- tapply(data$steps_filled, as.factor(data$date), sum)
hist(daily_steps_filled, breaks=50, main="Histogram of daily steps count - NAs filled with average")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
steps_mean_filled <- mean(daily_steps_filled)
steps_median_filled <- median(daily_steps_filled)
```
After filling the data, the mean of the total number of steps taken per day is **10749.77**, while the median value is **10641** steps.



The values have changed.

# Are there differences in activity patterns between weekdays and weekends?

First, let's add the column with information about whether a measurement is done on a weekday or not. Then we calculate the averages for two cases separately and plot them.

Note a different approach than suggested in the exercise, chosen to avoid locale problems (names of the days of the week in languages other than English).


```r
data$isweekday <- strftime(data$date, "%u") < 6

wkf <- data$isweekday == TRUE
steps_avg_weekday <- tapply(data$steps[wkf], as.factor(data$interval)[wkf], mean, na.rm=TRUE)
steps_avg_weekend <- tapply(data$steps[!wkf], as.factor(data$interval)[!wkf], mean, na.rm=TRUE)
par(mfrow=c(2,1))
plot(steps_avg_weekday, type = "l", axes=F,
     xlab="Time of day",
     ylab="Average steps made")
box()
axis(1, at=seq(0, 287, 48), labels=timelabel[c(TRUE, rep(FALSE, 47))])
title("Daily weekday activity pattern")
plot(steps_avg_weekend, type = "l", axes=F,
     xlab="Time of day",
     ylab="Average steps made")
axis(1, at=seq(0, 287, 48), labels=timelabel[c(TRUE, rep(FALSE, 47))])
title("Daily weekend activity pattern")
box()
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 
The activity during weekends is significantly different compared to the weekday. Movement begins later in the day, but continues throughout the day, whereas during the week the most active time is morning (probably work commute time).
