1.  Code for reading in the dataset and/or processing the data

<!-- -->

    data <- read.csv("activity.csv", header=T, sep = ",")

    #Get general idea of the data.
    head(data)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    tail(data)

    ##       steps       date interval
    ## 17563    NA 2012-11-30     2330
    ## 17564    NA 2012-11-30     2335
    ## 17565    NA 2012-11-30     2340
    ## 17566    NA 2012-11-30     2345
    ## 17567    NA 2012-11-30     2350
    ## 17568    NA 2012-11-30     2355

    dim(data)

    ## [1] 17568     3

    names(data)

    ## [1] "steps"    "date"     "interval"

    str(data)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    #Total number of days in data = 61 days
    unique(data$date)

    ##  [1] 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06
    ##  [7] 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12
    ## [13] 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18
    ## [19] 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24
    ## [25] 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30
    ## [31] 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05
    ## [37] 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11
    ## [43] 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17
    ## [49] 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23
    ## [55] 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29
    ## [61] 2012-11-30
    ## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30

    # Keep a list of all possible intervals for plotting
    uniqueIntervals <- unique(data$interval)

    #There are 288 intervals per day
    tail(head(data, 289))

    ##     steps       date interval
    ## 284    NA 2012-10-01     2335
    ## 285    NA 2012-10-01     2340
    ## 286    NA 2012-10-01     2345
    ## 287    NA 2012-10-01     2350
    ## 288    NA 2012-10-01     2355
    ## 289     0 2012-10-02        0

    #Loop through each interval for each day
    sumperday <- c()
    for (i in 1:61) {
            startofday <- ((i*288)-287)
            endofday <- (i*288)
            temp <- data[startofday:endofday, 1]
            #add steps taken each day, ignore NA.
            sumperday <- c(sumperday, sum(temp, na.rm = T))
    }
    head(sumperday)

    ## [1]     0   126 11352 12116 13294 15420

    str(sumperday)

    ##  int [1:61] 0 126 11352 12116 13294 15420 11015 0 12811 9900 ...

1.  Histogram of the total number of steps taken each day

<!-- -->

    hist(sumperday, breaks=20, col="blue", xlab = "Number of Steps", main = "Total Number of Steps per Day")

![](PA1_template_files/figure-markdown_strict/Histogram%20of%20total%20steps-1.png)

1.  Mean and median number of steps taken each day

<!-- -->

    mean(sumperday)

    ## [1] 9354.23

    median(sumperday)

    ## [1] 10395

1.  Time series plot of the average number of steps taken

<!-- -->

    #Plot average number of steps
    avedaily <- tapply(data$steps, data$interval, mean, na.rm=T)
    plot(avedaily ~ unique(data$interval), type="l", xlab="5 Min Interval", main="Average number Steps")

![](PA1_template_files/figure-markdown_strict/Time%20series%20plot-1.png)

    head(avedaily)

    ##         0         5        10        15        20        25 
    ## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396

    tail(avedaily)

    ##      2330      2335      2340      2345      2350      2355 
    ## 2.6037736 4.6981132 3.3018868 0.6415094 0.2264151 1.0754717

1.  The 5-minute interval that, on average, contains the maximum number
    of steps

<!-- -->

    #Max occurs at 5 min interval segment 8:35 am.
    which.max(avedaily)

    ## 835 
    ## 104

    #Plot average number of steps
    avedaily <- tapply(data$steps, data$interval, mean, na.rm=T)
    plot(avedaily ~ unique(data$interval), type="l", xlab="5 Min Interval", main="Average number Steps with Max in Red at 835 or 8:35am")
    abline(v=850, col="red", lty=3)

![](PA1_template_files/figure-markdown_strict/Interval%20with%20max%20steps-1.png)

1.  Code to describe and show a strategy for imputing missing data

<!-- -->

    #There are 2304 rows containing NA, or missing data.
    sum(is.na(data$steps))

    ## [1] 2304

    #Create another copy of data
    data2 <- data
    totalsteps <- c()

    # Loop through each day and interval and fill with mean. 
    for (i in 1:61) {
            startofday <- ((i*288)-287)
            endofday <- (i*288)
            temp <- data2[startofday:endofday, 1]
            #add steps taken each day. Remove NA.
            totalsteps <- c(totalsteps, mean(temp, na.rm=T))
    }

    head(totalsteps)

    ## [1]      NaN  0.43750 39.41667 42.06944 46.15972 53.54167

1.  Histogram of the total number of steps taken each day after missing
    values are imputed

<!-- -->

    #Histogram of the total number of steps taken each day after missing values are imputed
    hist(totalsteps, xlab="Steps", main = "With NA's Filled in", breaks = 20, col = "red")

![](PA1_template_files/figure-markdown_strict/Histogram%20with%20filled%20NA-1.png)

    hist(sumperday, xlab = "Steps", main = "Total Steps, NA's not Filled", breaks = 20, col = "blue")

![](PA1_template_files/figure-markdown_strict/Histogram%20with%20filled%20NA-2.png)

1.  Panel plot comparing the average number of steps taken per 5-minute
    interval across weekdays and weekends

<!-- -->

    #Make another copy of data to work with.
    datNew <- data
    dates <- strptime(datNew$date, "%Y-%m-%d")
    datNew$date <- dates

    # Keep a list of all possible intervals
    uniqueIntervals <- unique(datNew$interval)

    #Use POSIXlt date function to group days from 0:6 (Sunday thru Saturday)
    wdays <- dates$wday

    whichDays <- rep(0, 17568)

    #Weekdays are 1:5, weekends are 6,0 (Saturday, Sunday)
    whichDays[wdays > 0 & wdays < 6] <- 1
    whichDays[wdays == 6 | wdays == 0] <- 2
    head(whichDays)

    ## [1] 1 1 1 1 1 1

    #Since whichDays is just a string of 1's and 2's, create a factor with 2 levels.
    daysFactor <- factor(whichDays, levels=c(1,2))
    datNew$weekWeekend <- daysFactor

    #Sort into Weekdays and weekends.
    weekdays <- datNew[datNew$weekWeekend == "1", ]
    weekends <- datNew[datNew$weekWeekend == "2", ]

    #Split into a list for plotting later.
    splitWeekdays <- split(weekdays$steps, weekdays$interval)
    splitWeekends <- split(weekends$steps, weekends$interval)

    #Take mean, but ignore NA's.
    meanWeekdays <- sapply(splitWeekdays, mean, na.rm=TRUE)
    meanWeekends <- sapply(splitWeekends, mean, na.rm=TRUE)

    #Plot Weekday and Weekend data
    par(mfcol=c(2,1))
    plot(uniqueIntervals, meanWeekdays, type="l",
    main="Average number steps per interval - weekdays", 
    xlab="Interval", ylab="Ave # steps - weekdays", 
    lwd=2, col="blue")
    plot(uniqueIntervals, meanWeekends, type="l",
    main="Average number steps per interval - weekends", 
    xlab="Interval", ylab="Average # steps - weekends", 
    lwd=2, col="blue")

![](PA1_template_files/figure-markdown_strict/Plot%20weekend%20weekday-1.png)
