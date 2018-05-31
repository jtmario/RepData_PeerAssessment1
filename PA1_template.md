---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
=======================================================================================

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
  1. Load the data (i.e. read.csv())

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv')
```
  
  2. Process/transform the data into a format suitable for analysis

```r
#Change to date format
activityData$date <- as.Date(activityData$date)
```
  
## What is mean total number of steps taken per day?

  1. Calculate the total number of steps taken per day and list first 6 lines

```r
stepsDay <- aggregate(activityData$steps, by=list(activityData$date), FUN=sum, na.rm=TRUE)
names(stepsDay) <- c("Day","Steps")
head(stepsDay)
```

```
##          Day Steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

  2. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
qplot(stepsDay$Steps, geom="histogram",  binwidth = 2500, main="Histogram of total steps by day", xlab="Steps",ylab="Frequency")  
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

  3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(stepsDay$Steps)
median(stepsDay$Steps)
```

The mean is **9354** and the median is **10395**

  
## What is the average daily activity pattern?

  1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averageStepsTime <- aggregate(activityData$steps, by=list(activityData$interval), FUN=mean, na.rm=TRUE)
names(averageStepsTime) <- c("interval","meanSteps")
ggplot(data=averageStepsTime, aes(x=interval, y = meanSteps)) + 
  geom_line() + 
  xlab("Interval") +
  ylab("Average Steps") +
  ggtitle("Average steps by 5-minute interval")+
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

  2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max <- averageStepsTime$interval[which.max(averageStepsTime$meanSteps)]
```
The 5-minute interval is **835** and it´s equal to **206**

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
