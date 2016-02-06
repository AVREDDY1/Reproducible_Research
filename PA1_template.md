---
title: "Reproducible Research: Peer Assessment 1"
author: Anshuman Reddy
output: 
  html_document:
    keep_md: true
---
#Loading and Preparing the Project data
###loads necessary files
```{r}
#loads ggplot2 and plyr
library(ggplot2)
library(plyr)
library(dplyr) 
```
###reads data
```{r}
#reads activity files
datum<-read.csv("activity.csv")
```
###checks data
```{r}
#takes out NA 
fixed <- datum[!is.na(datum$steps),]
#displayes the head of the data
head(datum)
#displayes the internal structure of the data
str(datum)

###processes data
```

```{r}
fixed$day <- weekdays(as.Date(fixed$date))
fixed$DateTime<- as.POSIXct(fixed$date, format="%Y-%m-%d")
```
# Mean total number of steps taken per day
```{r}
## calculates the total steps per date
complete <- aggregate(fixed$steps ~ fixed$date, FUN=sum, )
colnames(complete)<- c("Date", "Steps")
```
###Histogram of mean total number of steps
```{r}
## Creates the historgram of the total steps per day
hist(complete$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
#total steps/day
```
### mean and median of  without missing values
```{r}
## the median of steps
as.integer(median(complete$Steps))
## the mean of steps
as.integer(mean(complete$Steps))

```
# Average daily activity pattern

###Average steps taken and Times series plot
```{r}
##creates an average number of steps per interval
work <- ddply(fixed, .(interval), summarize, Avg = mean(steps))
##Creates a line plot of average number of steps per interval
plot2 <- ggplot(work, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
plot2 + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```
###tells which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
```{r}
##this calculates the maximum amount of steps by interval
stepup <- max(work$Avg)

## this shows which interval contains the maximum average number of steps
work[work$Avg==stepup,1]
```
The maximum number of steps for a 5-minute interval was 206 steps.

The 5-minute interval which had the maximum number of steps was the 835 interval.
#Imputing missing Values

###Calculate and report the total number of missing values in the dataset
```{r}

##numbers of missing vaules
miss<-datum[is.na(datum$steps),]
nrow(miss)
```
###create new dataset like original with missing mdata
```{R}
file<-filter(datum,!is.na(steps))
datum[is.na(datum[1]),][1]<-mean(file$steps)

```
### proof that missing values have been filled
```{R}
##head with missing vaulues filled out
head(datum)
##str with missing values filled out
str(datum)

```
###Histogram of total number of steps per day with missing day found out.
```{R}
##sets up data
stepday<-group_by(datum,date) %>% summarize(sum(steps))
##creates histogram
hist(stepday$"sum(steps)",nclass=6,main="Histogram of total number of steps per day with replaced NA with the mean", xlab="Total no. of steps per day")
```
### mean and median with missing values found out
```{R}
##calculates mean
stepdaymean<-summary(stepday[2])[4]
##calculates median
stepdaymedian<-summary(stepday[2])[3]
##shows mean and median
stepdaymean; stepdaymedian
```

### Do values differ with or without missing data? What is the impact of imputing the missing data
The values don't change much. The mean is the same but the median because the same value as the mean. Filling in missing data with estimates of the total daily number of steps doesn't really affect the mean and median.

##Are there differences in activity patterns between weekdays and weekends?

###creates a new factor variable in the dataset with weekday and weekend
```{R}
certain<-datum
##creates new factor variable
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}
#prepares variable for the panel plot
certain$date <- as.Date(certain$date)
certain$day <- sapply(certain$date, FUN = weekday.or.weekend)
```

###Panel plot containing a time series plot of the week
```{R}
average <- aggregate(steps ~ interval + day, data = certain, mean)
##creates panel plot
ggplot(average, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```