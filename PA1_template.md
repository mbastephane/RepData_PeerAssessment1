---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
1. Load the data (ie read.csv())

```r
unzip("activity.zip")
activity_raw <- read.csv("activity.csv", na.strings=c("NA","NaN"))
head(activity_raw)
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

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity =  activity_raw[!is.na(activity_raw$steps),]
head(activity)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

## What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day

```r
steps_per_day <- aggregate(activity$steps, by = list(activity$date), FUN=sum)
names(steps_per_day) <- c("date", "nb_steps")
head(steps_per_day)
```

```
##         date nb_steps
## 1 2012-10-02      126
## 2 2012-10-03    11352
## 3 2012-10-04    12116
## 4 2012-10-05    13294
## 5 2012-10-06    15420
## 6 2012-10-07    11015
```

2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
print(ggplot(steps_per_day, aes(x=date, y=nb_steps)) + geom_bar(stat="identity"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean_step_per_day = aggregate(activity$steps, by = list(activity$date), FUN=mean)
median_step_per_day = aggregate(activity$steps, by = list(activity$date), FUN=median)
names(mean_step_per_day) = c("date","mean")
names(median_step_per_day) = c("date","median")
head(mean_step_per_day)
```

```
##         date     mean
## 1 2012-10-02  0.43750
## 2 2012-10-03 39.41667
## 3 2012-10-04 42.06944
## 4 2012-10-05 46.15972
## 5 2012-10-06 53.54167
## 6 2012-10-07 38.24653
```

```r
head(median_step_per_day)
```

```
##         date median
## 1 2012-10-02      0
## 2 2012-10-03      0
## 3 2012-10-04      0
## 4 2012-10-05      0
## 5 2012-10-06      0
## 6 2012-10-07      0
```

## What is the average daily activity pattern?
1.Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avg_steps_interval = aggregate(activity$steps, by = list(activity$interval), FUN=mean)
names(avg_steps_interval) = c("interval","average")
print(ggplot(avg_steps_interval, aes(x = interval, y=average)) + geom_line() + labs(x = "5-min-interval"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max(avg_steps_interval$average)
```

```
## [1] 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nb_missing_val = length(activity_raw$steps[is.na(activity_raw$steps)])
nb_missing_val
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
activity_no_NA_w_avg = merge(activity_raw, mean_step_per_day)[is.na(activity_raw$steps),"mean"]
head(activity_no_NA_w_avg)
```

```
## [1] 0.4375 0.4375 0.4375 0.4375 0.4375 0.4375
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_filled_NA = activity_raw
activity_filled_NA$steps[is.na(activity_raw$steps)] = activity_no_NA_w_avg
head(activity_filled_NA)
```

```
##    steps       date interval
## 1 0.4375 2012-10-01        0
## 2 0.4375 2012-10-01        5
## 3 0.4375 2012-10-01       10
## 4 0.4375 2012-10-01       15
## 5 0.4375 2012-10-01       20
## 6 0.4375 2012-10-01       25
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_per_day_filled_NA <- aggregate(activity_filled_NA$steps, by = list(activity_filled_NA$date), FUN=sum)
names(steps_per_day_filled_NA) <- c("date", "nb_steps")

print(ggplot(steps_per_day_filled_NA, aes(x=date, y=nb_steps)) + geom_bar(stat="identity"))
```

```
## Warning: Removed 1 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean_step_per_day_fNA = aggregate(activity_filled_NA$steps, by = list(activity_filled_NA$date), FUN=mean)
median_step_per_day_fNA = aggregate(activity_filled_NA$steps, by = list(activity_filled_NA$date), FUN=median)
names(mean_step_per_day_fNA) = c("date","mean")
names(median_step_per_day_fNA) = c("date","median")
head(mean_step_per_day_fNA)
```

```
##         date     mean
## 1 2012-10-01  0.43750
## 2 2012-10-02  0.43750
## 3 2012-10-03 39.41667
## 4 2012-10-04 42.06944
## 5 2012-10-05 46.15972
## 6 2012-10-06 53.54167
```

```r
head(median_step_per_day_fNA)
```

```
##         date median
## 1 2012-10-01 0.4375
## 2 2012-10-02 0.0000
## 3 2012-10-03 0.0000
## 4 2012-10-04 0.0000
## 5 2012-10-05 0.0000
## 6 2012-10-06 0.0000
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity_filled_NA$day_type = factor(grepl("S(at|un)", weekdays(as.Date(activity_filled_NA$date))), levels = c(TRUE, FALSE), labels = c("weekend","weekday"))
head(activity_filled_NA)
```

```
##    steps       date interval day_type
## 1 0.4375 2012-10-01        0  weekday
## 2 0.4375 2012-10-01        5  weekday
## 3 0.4375 2012-10-01       10  weekday
## 4 0.4375 2012-10-01       15  weekday
## 5 0.4375 2012-10-01       20  weekday
## 6 0.4375 2012-10-01       25  weekday
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
steps_per_day_filled_NA_wd <- aggregate(steps~interval+day_type, activity_filled_NA, mean)
print(ggplot(steps_per_day_filled_NA_wd, aes(interval, steps)) + geom_line() + facet_wrap(~day_type))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
