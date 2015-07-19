# PA1_template.R
##
## 

# load libraries
library(dplyr)
library(lattice)

###  Initialize common variables
srcDir <- "/Users/prasadkodibagkar/Documents/Data Science/workspace/RepData_PeerAssessment1"
dataDir <- "/Users/prasadkodibagkar/Documents/Data Science/workspace/RepData_PeerAssessment1"
activityDataFile <- "activity.csv"

# Load activity File

activityDF <- read.csv(paste(dataDir,"/", activityDataFile,sep = ""))

str(activityDF)

# Filter the observations with missing values

cleanActivityDF <- activityDF[!is.na(activityDF$steps),]

# Summarize Steps by date

activitySummaryByDate <- cleanActivityDF %>%
    group_by(date) %>%
    summarize(daily_steps = sum(steps))

# Plot a histogram for the daily steps
hist(activitySummaryByDate$daily_steps, col="red",xlab="Total Steps", main="Total number of steps by day" )

# Calculate the mean value for daily steps
mean(activitySummaryByDate$daily_steps)

# Calculate the median value for daily steps
median(activitySummaryByDate$daily_steps)

# Create a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis)

# Summarize Steps by day

activitySummaryByInterval <- cleanActivityDF %>%
    group_by(interval) %>%
    summarize(avg_steps = mean(steps))



# Calculate the interval with the max average steps
maxAvgSteps <- max(activitySummaryByInterval$avg_steps)
maxInterval <- activitySummaryByInterval[activitySummaryByInterval$avg_steps == maxAvgSteps,]$interval

# Plot the average steps by interval and add a line showing the interval with the max average steps

plot(activitySummaryByInterval$interval, activitySummaryByInterval$avg_steps,type="l", 
     xlab= "Interval", ylab= "Average Steps",main="Average Steps By Interval", col="black" , lwd=1)

abline(v=maxInterval, h=maxAvgSteps, col="red")
axis(1, at= maxInterval, col.axis="red")

## Imputing missing values

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print(nrow(activityDF[is.na(activityDF$steps),]))

#Copy and create an Imputed Data frame
imputedActivityDF <- activityDF

# Fill missing values in the dataset mean steps for that 5 minute interval 

# Function to get mean steps for a specific 5 minute interval
getAverageStepsForInterval <- function(intvl){
    return(activitySummaryByInterval[activitySummaryByInterval$interval==intvl,][["avg_steps"]])
}

for ( i in 1:nrow(imputedActivityDF))
{
    if (is.na(imputedActivityDF[i,"steps"])) {
        imputedActivityDF[i,"steps"] <- getAverageStepsForInterval(imputedActivityDF[i,"interval"])
    }
}

# Summarize Steps by day

imputedActivitySummary <- imputedActivityDF %>%
    group_by(date) %>%
    summarize(daily_steps = sum(steps))

# Plot a histogram for the daily steps
hist(imputedActivitySummary$daily_steps, col="red",xlab="Total Steps", main="Total number of steps by day" )

# Calculate the mean value for daily steps
mean(imputedActivitySummary$daily_steps)

# Calculate the median value for daily steps
median(imputedActivitySummary$daily_steps)
    
# Are there differences in activity patterns between weekdays and weekends?

# Add a factor variable with two levels -- "weekday" and "weekend"
imputedActivityDF <- imputedActivityDF %>%
    transform(day_of_week = ifelse(as.numeric(format(as.Date(date),"%w")) <=5 ,"weekday","weekend"))
#    transform(day_of_week = format(as.Date(date),"%w"))
print(str(imputedActivityDF));

# Summarize Steps by weekdays and interval

imputedActivitySummaryByWeekday <- imputedActivityDF %>%
    group_by(day_of_week,interval) %>%
    summarize(avg_steps = mean(steps))

# Panel Plot of average steps by interval to compare weekday and weekend activity

p <- xyplot(avg_steps ~ interval | day_of_week, data = imputedActivitySummaryByWeekday,layout = c(1, 2),
            type="l",xlab="Interval",ylab="Number of steps",main="Comparison between weekend and weekday activity") 
print(p)

#mutate(day_of_week = ifelse(as.numeric(as.Date(date,format="%w")) > )

print(head(activityDF))
print(head(cleanActivityDF,20))
print(head(activitySummaryByDate,20))
print(head(activitySummaryByInterval,20))
print(str(activitySummaryByInterval))