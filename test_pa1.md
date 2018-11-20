---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
    self_contained: false
    fig_width: 7
---

**Reproducible Research - Activity Monitoring Data**
====================================================


```r
knitr::opts_chunk$set(echo = TRUE)
```

#

This is the R Markdown file for the Project

## **1. Loading and preprocessing the data**

Load library that will be used throughout the code


```r
library(tidyverse)
```

```
## -- Attaching packages ---- tidyverse 1.2.1 --
```

```
## v ggplot2 3.0.0     v purrr   0.2.5
## v tibble  1.4.2     v dplyr   0.7.6
## v tidyr   0.8.1     v stringr 1.3.1
## v readr   1.1.1     v forcats 0.3.0
```

```
## -- Conflicts ------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

1.1 Load the data (i.e. read.csv())


```r
x <- read.csv("activity.csv")
head(x)
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


1.2  Preprocess the data


```r
x$date <- as.Date(x$date)# convert to Date
str(x)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## **2.  What is the mean total number of steps taken per day?**

2.1 Make a histogram of the total number of steps taken each day

2.2 Calculate and report the *mean* and *median* total number of steps taken per day




- The code below shows both the histogram as well as the mean and median

- First, we apply some data transformations:


```r
x1 <- group_by(x,date)
x2 <- summarize(x1, steps = sum(steps, na.rm = TRUE))
head(x2)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <date>     <int>
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

- Then, we plot


```r
ggplot(data = x2, aes(x = steps, color = I('black'), fill = I('grey99')))+
  geom_histogram(bins = 12) +
  ggtitle("Histogram") +
  labs(x = "Steps per Day", y = "Number of Days", subtitle = "Distribution of Number of Steps per Day") +
  geom_vline(aes(xintercept = mean(steps, na.rm = TRUE), color = "red")) +
  geom_text(mapping = aes(label = round(mean(steps, na.rm = TRUE),0),y = -1, x = mean(steps, na.rm = TRUE), color = 'red', hjust = 1.1)) +
  geom_text(mapping = aes(label = "Mean", y = -0.3, x = mean(steps, na.rm = TRUE), color = 'red', hjust = 1.1)) +
  geom_vline(aes(xintercept = median(steps, na.rm = TRUE), color = "blue")) +
  geom_text(mapping = aes(label = round(median(steps, na.rm = TRUE),0),y = -1, x = median(steps, na.rm = TRUE), color = 'blue', hjust = -0.1)) +
  geom_text(mapping = aes(label = "Median", y = -0.3, x = median(steps, na.rm = TRUE), color = 'blue', hjust = -0.1))
```

![](test_pa1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


**Mean of steps taken each day:** 9354

**Median of steps taken each day:** 10395




## **3. What is the average daily activity pattern?**

3.1 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


First, some data transformations 


```r
x3 <- group_by(x,interval)
x4 <- summarize(x3, total_steps = sum(steps, na.rm = TRUE), mean_steps = mean(steps, na.rm = TRUE))
```


Then, generating the time series plot 


```r
ggplot(data = x4) + 
  geom_line(mapping = aes(x = interval, y = mean_steps, color = I('blue'))) +
  ggtitle("Time Series Chart") + 
  labs(x = "Time Interval", y = "Average # of Steps", subtitle = "Average # of Steps by Time Interval") +
  geom_vline(aes(xintercept = interval[mean_steps == max(mean_steps)])) +
  geom_text(mapping = aes(label = interval[mean_steps == max(mean_steps)],y = -8, x = interval[mean_steps == max(mean_steps)], hjust = 1.2)) +
  geom_text(mapping = aes(label = "Interval", y = 0, x = interval[mean_steps == max(mean_steps)], hjust = 1.1))
```

![](test_pa1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


The **5-minute interval** that contains the maximum number of steps (as shown in the chart) is the **8:35**


## **4. Imputing Missing Values**

4.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(x$steps))
```

```
## [1] 2304
```

There are **2304** rows with missing values




4.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

First, we explore the data to see where the missing values appear to be


```r
s  <- split(x$steps, x$date)
s1 <- aggregate(steps ~ date, data = x,  function(y) {sum(is.na(y))}, na.action = NULL)
head(s1,10)
```

```
##          date steps
## 1  2012-10-01   288
## 2  2012-10-02     0
## 3  2012-10-03     0
## 4  2012-10-04     0
## 5  2012-10-05     0
## 6  2012-10-06     0
## 7  2012-10-07     0
## 8  2012-10-08   288
## 9  2012-10-09     0
## 10 2012-10-10     0
```

Based on that, missing values correspond to entire days. So, to impute missing values:

1. First, I identify which weekday correspond to the missing value

2. Then, I obtain the average for same weekdays without missing values

3. Finally, I create a new dataframe, imputing the resulting value for the corresponding weekday and interval on those ones with null values




4.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.



```r
x$weekday <- weekdays(x$date)
x_notna <- subset(x, !is.na(x$steps))
x_notna <- group_by(x_notna, weekday, interval) %>%
  summarize(steps = round(mean(steps),0))


x_new <- left_join(x, x_notna, by = c("interval", "weekday")) %>%
  mutate(steps.x = ifelse(is.na(steps.x),steps.y, steps.x)) %>%
  select(-(steps.y)) %>%
  rename(steps = steps.x)

head(x_new)
```

```
##   steps       date interval weekday
## 1     1 2012-10-01        0  Monday
## 2     0 2012-10-01        5  Monday
## 3     0 2012-10-01       10  Monday
## 4     0 2012-10-01       15  Monday
## 5     0 2012-10-01       20  Monday
## 6     5 2012-10-01       25  Monday
```




4.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 


With the new data frame, I proceed to do the data transformations before plotting


```r
x1_new <- group_by(x_new,date)
x2_new <- summarize(x1_new, steps = sum(steps, na.rm = TRUE))
head(x2_new)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <date>     <dbl>
## 1 2012-10-01  9978
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

Now we plot


```r
ggplot(data = x2_new, aes(x = steps, color = I('black'), fill = I('grey99')))+
  geom_histogram(bins = 12) +
  ggtitle("Histogram") +
  labs(x = "Steps per Day", y = "Number of Days", subtitle = "Distribution of Number of Steps per Day - With Imputed Values") +
  geom_vline(aes(xintercept = mean(steps, na.rm = TRUE), color = "red")) +
  geom_text(mapping = aes(label = round(mean(steps, na.rm = TRUE),0),y = -1, x = mean(steps, na.rm = TRUE), color = 'red', hjust = 1.1)) +
  geom_text(mapping = aes(label = "Mean", y = -0.3, x = mean(steps, na.rm = TRUE), color = 'red', hjust = 1.1)) +
  geom_vline(aes(xintercept = median(steps, na.rm = TRUE), color = "blue")) +
  geom_text(mapping = aes(label = round(median(steps, na.rm = TRUE),0),y = -1, x = median(steps, na.rm = TRUE), color = 'blue', hjust = -0.1)) +
  geom_text(mapping = aes(label = "Median", y = -0.3, x = median(steps, na.rm = TRUE), color = 'blue', hjust = -0.1))
```

![](test_pa1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


With imputed values:

- Mean increases from **9354** to **10821** steps

- Median increases from **10395** to **11015** steps




## **5.  Are there differences in activity patterns between weekdays and weekends?**

5.1 Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
x_new <- mutate(x_new, weekday.factor = ifelse(weekday %in% c("Saturday","Sunday"),c('Weekend'),c('Weekday')))
x_new$weekday.factor <- as.factor(x_new$weekday.factor)
```


5.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

First, some data transformations


```r
x3_new <- group_by(x_new, weekday.factor, interval)
x4_new <- summarize(x3_new, total_steps = sum(steps, na.rm = TRUE), mean_steps = mean(steps, na.rm = TRUE))
head(x4_new)
```

```
## # A tibble: 6 x 4
## # Groups:   weekday.factor [1]
##   weekday.factor interval total_steps mean_steps
##   <fct>             <int>       <dbl>      <dbl>
## 1 Weekday               0         103     2.29  
## 2 Weekday               5          20     0.444 
## 3 Weekday              10           8     0.178 
## 4 Weekday              15           9     0.2   
## 5 Weekday              20           4     0.0889
## 6 Weekday              25          70     1.56
```


Then, we plot


```r
ggplot(data = x4_new) + 
  geom_line(mapping = aes(x = interval, y = mean_steps, color = I('blue'))) +
  ggtitle("Time Series Chart") + 
  facet_wrap(~ weekday.factor, nrow = 2) +
  labs(x = "Time Interval", y = "Average # of Steps", subtitle = "Average # of Steps by Time Interval")
```

![](test_pa1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

- **On weekdays:** there is more activity in **morning hours (around 8.35am). 

- **On weekends:** the acitivity is more spread out during day time.




















