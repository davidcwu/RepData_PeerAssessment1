1.  Code for reading in the dataset and/or processing the data

<!-- -->

    data <- read.csv("activity.csv", header=T, sep = ",")

    #Get general idea of the data.
    #Total number of days in data = 61 days
    tail(unique(data$date), 2)

    ## [1] 2012-11-29 2012-11-30
    ## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30

    # Keep a list of all possible intervals for plotting
    uniqueIntervals <- unique(data$interval)

    #There are 288 intervals per day
    tail(head(data, 289), 2)

    ##     steps       date interval
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

1.  Histogram of the total number of steps taken each day

<!-- -->

    #par(mfcol=c(2,1))
    hist(sumperday, breaks=20, col="blue", xlab = "Number of Steps", main = "Total Number of Steps per Day")

![](PA1_template_files/figure-markdown_strict/Histogram%20of%20total%20steps-1.png)

1.  Mean and median number of steps taken each day

<!-- -->

    mean(sumperday)

    ## [1] 9354.23

    median(sumperday)

    ## [1] 10395

1.  Time series plot of the average number of steps taken (see Step 5)

<!-- -->

    #Plot average number of steps
    avedaily <- tapply(data$steps, data$interval, mean, na.rm=T)
    #plot(avedaily ~ unique(data$interval), type="l", xlab="5 Min Interval", main="Average number Steps")

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

1.  Histogram of the total number of steps taken each day after missing
    values are imputed

<!-- -->

    #Histogram of the total number of steps taken each day after missing values are imputed
    par(mfcol=c(2,1))
    hist(totalsteps, xlab="Steps", main = "With NA's Filled in", breaks = 20, col = "red")
    hist(sumperday, xlab = "Steps", main = "Total Steps, NA's not Filled", breaks = 20, col = "blue")

![](PA1_template_files/figure-markdown_strict/Histogram%20with%20filled%20NA-1.png)

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
