# Reproducible Research: Peer Assessment 1
Amy Burnett Cross  


## Loading and preprocessing the data

Load the data (previously saved in the working directory).

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.4.3
```

```
## -- Attaching packages ---------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.4
## v tibble  1.3.4     v dplyr   0.7.4
## v tidyr   0.7.2     v stringr 1.2.0
## v readr   1.1.1     v forcats 0.2.0
```

```
## Warning: package 'tibble' was built under R version 3.4.3
```

```
## Warning: package 'tidyr' was built under R version 3.4.3
```

```
## Warning: package 'readr' was built under R version 3.4.3
```

```
## Warning: package 'purrr' was built under R version 3.4.3
```

```
## Warning: package 'dplyr' was built under R version 3.4.3
```

```
## Warning: package 'forcats' was built under R version 3.4.3
```

```
## -- Conflicts ------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
unzip("activity.zip")
activity <- read.csv("activity.csv")
## Process/transform the data (if necessary) into a format suitable for your analysis
```

Filter all na values.

```r
activity <- activity[!is.na(activity$steps),]
```



## Mean total number of steps taken per day
Calculate the total number of steps taken per day.

```r
daily_activity <- activity %>% group_by(date) %>%
    summarise(total_steps = sum(steps))

head(daily_activity)
```

```
## # A tibble: 6 x 2
##         date total_steps
##       <fctr>       <int>
## 1 2012-10-02         126
## 2 2012-10-03       11352
## 3 2012-10-04       12116
## 4 2012-10-05       13294
## 5 2012-10-06       15420
## 6 2012-10-07       11015
```

Print a histogram of the number of steps per day.

```r
hist(daily_activity$total_steps,
     main = "Histogram of Daily Activity",
     xlab = "Total Steps per Day",
     ylab = "Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate the mean number of steps per day.

```r
mean(daily_activity$total_steps)
```

```
## [1] 10766.19
```

Calculate the median number of steps per day.

```r
median(daily_activity$total_steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?


```r
interval_avg_steps <- activity %>%
    group_by(interval) %>%
    summarise(avg_int_activity = mean(steps))

j <- ggplot(interval_avg_steps, aes(interval, avg_int_activity))
j <- j + geom_line()
j <- j + ggtitle("Average pattern of steps throughout the day")
j <- j + ylab("Average steps")
j
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Interval with the maximum average steps

```r
max_interval <- interval_avg_steps[which.max(interval_avg_steps$avg_int_activity), ]
print(max_interval)
```

```
## # A tibble: 1 x 2
##   interval avg_int_activity
##      <int>            <dbl>
## 1      835         206.1698
```



#Imputing missing values

Calculate total number of missing values in dataset.

```r
activity <- read.csv("activity.csv")

length(which(is.na(activity$steps)))
```

```
## [1] 2304
```

Impute values of missing data with mean steps for the day. Create new data frame that includes imputed values. Now there are no NAs in the data frame.

```r
activity$steps <- as.numeric(activity$steps)

imputed_activity <- activity %>% 
        group_by(interval) %>%
        mutate(int_avg=mean(steps, na.rm = TRUE),
               imputed_steps = if_else(is.na(steps), int_avg, steps)) %>%
        select(c(imputed_steps, date, interval))
```

Calculate daily steps using imputed values.

```r
daily_imputed_activity <- imputed_activity %>% group_by(date) %>%
    summarise(total_imputed_steps = sum(imputed_steps))

head(daily_imputed_activity)
```

```
## # A tibble: 6 x 2
##         date total_imputed_steps
##       <fctr>               <dbl>
## 1 2012-10-01            10766.19
## 2 2012-10-02              126.00
## 3 2012-10-03            11352.00
## 4 2012-10-04            12116.00
## 5 2012-10-05            13294.00
## 6 2012-10-06            15420.00
```


Create a histogram of the total number of steps taken each day.

```r
hist(daily_imputed_activity$total_imputed_steps,
     main = "Histogram of Daily Activity with Imputed Values",
     xlab = "Total steps per Day",
     ylab = "Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Calculate the mean total number of steps taken per day.

```r
mean(daily_imputed_activity$total_imputed_steps)
```

```
## [1] 10766.19
```

Calculate the median total number of steps taken per day.

```r
median(daily_imputed_activity$total_imputed_steps)
```

```
## [1] 10766.19
```

The mean and median of daily activity differs very little (less than 2 steps per day) when imputed values are included. This makes sense because the method used to impute values mimics daily patterns of activity.



# Differences in activity patterns between weekdays and weekends


```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.4.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
week_activity <- imputed_activity %>% mutate(day_of_week = weekdays(ymd(date))) %>%
                                      mutate(weekend_status = as.factor(if_else(day_of_week 
                                                                                 %in% (c("Saturday", "Sunday")), 
                                                                                 "Weekends", "Weekdays"))) %>%
                                      select(interval, imputed_steps, weekend_status) %>%
                                      group_by(interval, weekend_status) %>%
                                      summarise(imputed_steps = mean(imputed_steps)) 
```

Create a panel plot of time series of activity on weekends and weekdays.

```r
week_plot <- ggplot(week_activity, 
       aes(interval, imputed_steps, group = 1)) + 
    geom_line() +
    facet_wrap(~weekend_status, ncol = 1, nrow = 3) +
    ggtitle("Average Weekday and Weekend Steps") +
    xlab("Interval") +
    scale_x_continuous(breaks = 5) +
    ylab("Total Steps") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_classic() +
    theme(plot.title = element_text(lineheight=.8, face="bold"))

week_plot
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->





