# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
pro1<-read.csv("~/coursera/activity.csv")
```

## What is mean total number of steps taken per day?

```r
istrue1<-complete.cases(pro1)
pro1_1<-subset(pro1,istrue1==TRUE)
pro1_1<-group_by(pro1_1,date)
pro1_1<-summarize(pro1_1,sum(steps))

names(pro1_1)[2]<-"total_steps"
hist(pro1_1$total_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean_1<-mean(pro1_1$total_steps)
median_1<-median(pro1_1$total_steps)
mean_1
```

```
## [1] 10766.19
```

```r
median_1
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
pro1_2<-subset(pro1,istrue1==TRUE)
pro1_2<-group_by(pro1_2,interval)
pro1_2<-summarize(pro1_2,mean(steps))
pro1_2$interval<-sprintf("%04d",pro1_2$interval)
pro1_2$interval<-strptime(pro1_2$interval,format="%H%M")
pro1_2$interval<-format(pro1_2$interval,"%H:%M")
names(pro1_2)[2]<-"average_steps"
g<-ggplot(pro1_2,aes(interval,average_steps,group=1))
g+geom_line()+labs(title="average steps per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values
 My soluation is to replace the the NA values by the average steps of the interval.
The dataset after replacing the NA values by average steps is the variable pro1_8
 

```r
pro1_3<-subset(pro1,istrue1==FALSE)

pro1_4<-pro1_3
pro1_4$interval<-sprintf("%04d",pro1_4$interval)
pro1_4$interval<-strptime(pro1_4$interval,format="%H%M")
pro1_4$interval<-format(pro1_4$interval,"%H:%M")

pro1_5<-merge(pro1_4,pro1_2,by.x="interval",by.y="interval")
pro1_5<-pro1_5[,c(1,3,4)]
names(pro1_5)[3]<-"steps"
pro1_6<-subset(pro1,istrue1==TRUE)
pro1_6$interval<-sprintf("%04d",pro1_6$interval)
pro1_6$interval<-strptime(pro1_6$interval,format="%H%M")
pro1_6$interval<-format(pro1_6$interval,"%H:%M")
pro1_7<-rbind(pro1_5,pro1_6)

pro1_8<-pro1_7[order(pro1_7$date),]
pro1_9<-group_by(pro1_8,date)
pro1_9<-summarize(pro1_9,sum(steps))
names(pro1_9)[2]<-"total_steps"


hist(pro1_9$total_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean_2<-mean(pro1_9$total_steps)
mean_2
```

```
## [1] 10766.19
```

```r
median_2<-median(pro1_9$total_steps)
median_2
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
The differences are obvious as we can see from the plot

```r
wk<-as.Date(pro1_8$date)
wk<-weekdays(wk)
pro1_10<-cbind(pro1_8,wk)
istrue2<-pro1_10$wk=="Saturday"
istrue3<-pro1_10$wk=="Sunday"
data_weekend<-subset(pro1_10,istrue2==TRUE|istrue3==TRUE)
data_weekday<-subset(pro1_10,istrue2==FALSE&istrue3==FALSE)

dat1<-group_by(data_weekday,interval)
dat1<-summarize(dat1,mean(steps))
names(dat1)[2]<-"average_steps"
dat2<-group_by(data_weekend,interval)
dat2<-summarize(dat2,mean(steps))
names(dat2)[2]<-"average_steps"
type<-sample("weekday",size=nrow(dat2),replace=TRUE)
dat11<-cbind(dat1,type)
type<-sample("weekend",size=nrow(dat2),replace=TRUE)
dat22<-cbind(dat2,type)
dat<-rbind(dat11,dat22)

k<-ggplot(dat,aes(interval,average_steps,group=1))
k+geom_line()+facet_grid(.~type)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
