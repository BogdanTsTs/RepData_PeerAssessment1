# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activivty_data = read.csv(unzip(zipfile = "activity.zip",files = "activity.csv")
                          ,stringsAsFactors = F)
activivty_data[,2]=as.Date(activivty_data[,2])
```

## What is mean total number of steps taken per day?


```r
steps_per_day = tapply(X = activivty_data$steps, 
                       INDEX =  factor(activivty_data$date),
                       FUN = function(x) sum(x,na.rm =T))
hist(steps_per_day)
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean_spd = mean(steps_per_day)
mean_spd
```

```
## [1] 9354.23
```

```r
median_spd = median(steps_per_day)
median_spd
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
steps_per_interval = tapply(X = activivty_data$steps, 
                       INDEX =  factor(activivty_data$interval),
                       FUN = function(x) mean(x,na.rm =T))
plot(steps_per_interval,type="l")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
names(steps_per_interval)[ which.max(steps_per_interval)] 
```

```
## [1] "835"
```


## Imputing missing values


```r
na_steps = is.na(activivty_data$steps)
sum(na_steps)
```

```
## [1] 2304
```

```r
activivty_data2 = activivty_data
activivty_data2[na_steps,1] = steps_per_interval[ as.character( 
                                            activivty_data[ na_steps , 3])]
steps_per_day2 = tapply(X = activivty_data2$steps, 
                       INDEX =  factor(activivty_data2$date),
                       FUN = sum )
hist(steps_per_day2)
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
mean_spd2 = mean(steps_per_day2)
mean_spd2
```

```
## [1] 10766.19
```

```r
mean_spd2-mean_spd
```

```
## [1] 1411.959
```

```r
median_spd2 = median(steps_per_day2)
median_spd2
```

```
## [1] 10766.19
```

```r
median_spd2-median_spd
```

```
## [1] 371.1887
```


## Are there differences in activity patterns between weekdays and weekends?


```r
wday = factor(as.POSIXlt(activivty_data2$date)$wday>5,labels = c("weekday","weekend"))
activity_weekday = activivty_data2[wday == "weekday",]
activity_weekend = activivty_data2[wday == "weekend",]
spi_wday = tapply(X = activity_weekday$steps, 
                       INDEX =  factor(activity_weekday$interval),
                       FUN = mean)
spi_wend = tapply(X = activity_weekend$steps, 
                       INDEX =  factor(activity_weekend$interval),
                       FUN = mean)
spi_data = data.frame(steps = c(spi_wday,spi_wend), interval = 
              as.numeric(c(names(spi_wday),names(spi_wend))), day =
              rep(c("weekday","weekend"),c(length(spi_wday),length(spi_wend))) )
library("lattice")
xyplot(steps~interval|day,data = spi_data,type = "l",layout = c(1,2))
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


