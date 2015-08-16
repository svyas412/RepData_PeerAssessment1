---
#title: "PeerAssessment1"
#author: "Salil Vyas"
date: "Tuesday, August 11, 2015"
output: html_document
---
#Researcher:  Salil Vyas
## Research Project:  Analyzing data produced by personal activity monitoring device

###Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This Analysys makes use of data from a personal activity monitoring device "Fitbit". This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

###Data
The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

2. date: The date on which the measurement data was taken in YYYY-MM-DD format

3.  interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

####Load Lilbraries as needed


```r
#Load Lilbraries as needed
library(dplyr)
library(knitr)
```





###Loading and preprocessing the data



```r
#set Working Directory
setwd ("C:/testR/labs-r/repres/data")


#Check and Read raw data file names activity.csv if not read already to save time and processing.

if(!exists("activity")){
  activity <- read.csv("activity.csv", colClasses = c("numeric", "character",                                                 "numeric"))
}
df<-data.frame(activity)
#Check the Activity data
head(activity)
```

```
##   steps       date interval daytype
## 1    NA 2012-10-01        0 Weekday
## 2    NA 2012-10-01        5 Weekday
## 3    NA 2012-10-01       10 Weekday
## 4    NA 2012-10-01       15 Weekday
## 5    NA 2012-10-01       20 Weekday
## 6    NA 2012-10-01       25 Weekday
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval" "daytype"
```


###What is mean total number of steps taken per day?


```r
# 1.   Calculate the total number of steps taken per day
per_day<-group_by(activity, date )
per_day<-(summarize(per_day, sum(steps))) #total number of steps per day
df_per_day <- data.frame(per_day)
names(df_per_day)
```

```
## [1] "date"       "sum.steps."
```


```r
# 2.   Histogram of the total number of steps taken each day
hist(df_per_day$sum.steps., main = "Total steps by day", xlab = "day", col = "blue")
```

![plot of chunk Histogram of the total number of steps taken each day](figure/Histogram of the total number of steps taken each day-1.png) 



```r
# 3.   Mean and median of the total number of steps taken per day
Mean <- mean(df_per_day$sum.steps., na.rm=TRUE)
Median <- median(df_per_day$sum.steps., na.rm=TRUE)
```


Mean and median of the total number of steps taken per day are 'r nrow(Mean)' and 'r nrow(Median)' respectively

###What is the average daily activity pattern?


```r
#1 time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken

per_int<-tapply(activity$steps, activity$interval, mean, na.rm=TRUE)

plot(row.names(per_int), per_int, type = "l", xlab = "5-min interval", 
     ylab = "Average across all Days", main = "Average number of steps taken", 
     col = "red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


```r
#1 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

summary(per_int)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.486  34.110  37.380  52.830 206.200
```

```r
max_steps<-which.max(per_int)
names(max_steps)
```

```
## [1] "835"
```

###Imputing missing values


```r
activity_NA <- sum(is.na(activity))
activity_NA
```

```
## [1] 2304
```



###Imputing missing values


```r
#Devise Strategy
Avg_Steps <- aggregate(steps~interval, data = activity, FUN=mean) #Calculate Average Steps by interval
Rep_NA<-numeric() #Replace NA values with Numeric value - Initiate
for (i in 1:nrow (activity)) {             
  obs<-activity[i, ]
  if (is.na (obs$steps)) {
    steps<-subset(Avg_Steps, interval == obs$interval)$steps   #If Steps=NA then replace the value with Average Steps for the interval
  } else {
      steps<-obs$steps
  }
  Rep_NA <- c(Rep_NA, steps)
}
```



```r
#Create a new dataset that is equal to the original dataset but with missing data filled in

new_ds <-activity
new_ds$steps <-Rep_NA

#histogram
Steps_Total <- aggregate(steps~date, data=new_ds, FUN=sum)
hist(Steps_Total$steps, col="blue", xlab="day", main="Total Number of Steps Taken Each Day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
#mean
mean(Steps_Total$steps)
```

```
## [1] 10766.19
```

```r
median(Steps_Total$steps)
```

```
## [1] 10766.19
```




###Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
days<-weekdays(activity$date)
daytype <- vector()

for (i in 1:nrow(activity)) {
    if (days[i] == "Sunday") {
        daytype[i] <- "Weekend"
    }
      else if (days[i] == "Saturday") {
        daytype[i] <- "Weekend"
    }
        
      else {
        daytype[i] <- "Weekday"
  }
}

activity$daytype <- daytype
activity$daytype <- factor(activity$daytype)
```

