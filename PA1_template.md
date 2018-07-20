---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
#Reproducible Research Week 2 Course Project 1

##1. Code for Downloading and Reading In the Dataset
-Download Data

```r
if(!file.exists("./ActivityMonitoringData")){
        dir.create("./ActivityMonitoringData")}
fileurl<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl, destfile = "./ActivityMonitoringData/data.zip")
unzip("./ActivityMonitoringData/data.zip", exdir = "./ActivityMonitoringData")
file.remove("./ActivityMonitoringData/data.zip")
```

```
## [1] TRUE
```
- Read in Data

```r
ActivityData<- read.csv("./ActivityMonitoringData/activity.csv")
Complete_ActivityData<- na.omit(ActivityData)
```

##2. Histogram of the Total Number of Steps Each Day
- This histogram shows the frequency of the average steps/day:

```r
library(ggplot2)
Total_Steps.Day<- aggregate(steps~date,data = Complete_ActivityData, sum)

qplot(Total_Steps.Day$steps, geom = "histogram",
      main = "Frequency of Total Steps per Day", 
      xlab = "Steps", 
      ylab = "Frequency")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/histogram1-1.png)<!-- -->

##3. Mean and Median Number of Steps per Day
-Mean of Total Steps per Day

```r
mean(Total_Steps.Day$steps)
```

```
## [1] 10766.19
```
-Median of Total Steps per Day

```r
median(Total_Steps.Day$steps)
```

```
## [1] 10765
```

##4. Time series plot of the average number of steps taken
- I first made a dataframe of the average number of steps per interval and then plotted it in a time series.


```r
Steps_Interval <- aggregate(steps~interval, data = Complete_ActivityData, mean)  
with(Steps_Interval, qplot(interval, steps, geom = "line", 
                           main = "Time Interval of Average Steps",
                           xlab = "Interval",
                           ylab = "Average Number of Steps"))
```

![](PA1_template_files/figure-html/timeseries1-1.png)<!-- -->

##5. The 5-minute interval that, on average, contains the maximum number of steps

```r
Max.Interval<- which.max(Steps_Interval$steps)
Steps_Interval[Max.Interval,]
```

```
##     interval    steps
## 104      835 206.1698
```

##6. Code to describe and show a strategy for imputing missing data
- Code to find the total number of values missing:

```r
Missing<- is.na(ActivityData$steps)
sum(Missing)
```

```
## [1] 2304
```
- Next I went through the dataframe and replaced each NA value with the mean for that interval:

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
replace_with_mean <- function(x){
        replace(x, is.na(x), mean(x, na.rm = TRUE))
}
Imputed_Dataset<- as.data.frame(ActivityData %>% group_by(interval) %>% 
                           mutate(steps = replace_with_mean(steps)))
```

##7. Histogram of the total number of steps taken each day after missing values are imputed
-I first made a dataframe of the average number of steps per day using the Imputed Values and then plotted it in a histogram.

```r
Total_Steps.Day2<- aggregate(steps~date, data = Imputed_Dataset, sum)
qplot(Total_Steps.Day2$steps, geom = "histogram",
      main = "Frequency of Total Steps per Day with Imputed Dataset", 
      xlab = "Steps", 
      ylab = "Frequency")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

-Mean of Total Steps per Day with Imputed Values

```r
mean(Total_Steps.Day2$steps)
```

```
## [1] 10766.19
```
-Median of Total Steps per Day with Imputed Values

```r
median(Total_Steps.Day2$steps)
```

```
## [1] 10766.19
```
- By comparing these values with the previous values taken from the dataset that omitted the NA values, you can see that the Mean is the same while the Median is slightly different. 

##8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
- I created a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
Imputed_Dataset$weekday.or.weekend<-
        as.factor(ifelse(weekdays(as.Date(Imputed_Dataset$date)) %in% 
        c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday",  "Weekend"))
```
-Panel plot of average number of steps taken per interval broken into "Weekdays" 
and "Weekends"

```r
Imputed_Dataset2<- aggregate(steps~interval + weekday.or.weekend, data = Imputed_Dataset,
                             mean)
names(Imputed_Dataset2)[3]<- "mean.steps"
ggplot(Imputed_Dataset2, mapping = aes(x = interval, y = mean.steps)) + geom_line() +
        facet_grid(weekday.or.weekend~.) + 
        xlab("Interval") + 
        ylab("Mean Number of Steps") + 
        ggtitle("Average Number of Steps per Interval and Weekday/Weekend")
```

![](PA1_template_files/figure-html/weekdayweekendpanelplot-1.png)<!-- -->

