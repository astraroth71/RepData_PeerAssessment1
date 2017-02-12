#removes all variables from environment
rm(list=ls(all=TRUE)) 
# load library we are going to use
library(data.table)
library(lattice)
# set the working directory
setwd("/home/gbartolotta/Coursera/Reproducible_Data/RepData_PeerAssessment1/")
filename <- "activity.csv"
zipFilename <- "activity.zip"
# check if the csv file is in the folder if exists download it 
# if not download from internet
if(!file.exists(filename)){
        if(!file.exists(zipFilename)){
                download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", zipFilename, method = "curl")
        }
        unzip(zipFilename)
}

# function to create the histogram
create_hist <- function(mydata, title){
        hist(mydata, main = title, breaks = 24)
        mean_val <- round(mean(mydata), 1)
        mean_val
        median_val <- round(median(mydata), 1)
        median_val
        abline(v=mean_val, lwd = 1, col = 'blue')
        abline(v=median_val, lwd = 1, col = 'red')
        legend('topright', lty = 1, lwd = 1, col = c("blue", "red"),
               cex = .8, 
               legend = c(paste('Mean: ', mean_val),
                          paste('Median: ', median_val))
        )        
}

# read the csv
activity <- read.csv(filename, header = T)
names(activity)
str(activity)
head(activity)

# Convert csv in data table and Take the total amount of steps per day
dt <- data.table(activity)
dt_sum <- dt[, list(totalSteps = sum(steps, na.rm = TRUE)), by = date]
paste('Mean: ', mean(dt_sum$totalSteps))
paste('Median: ', median(dt_sum$totalSteps))
# plot the histogram
png("plots/plot1.png")
create_hist(dt_sum$totalSteps, title = "Total Steps per Day")
dev.off()

# daily activity pattern
dt_by_interval <- dt[, list(avg_steps = mean(steps, na.rm = TRUE)), by = interval]

# plot 
png("plots/plot2.png")
plot(avg_steps~interval, data=dt_by_interval, type="l", width=500, xlab = "5 Min. interval", ylab = "Avg. Number of Steps", main ="Average steps per interval")

max_v <- dt_by_interval[which.max(dt_by_interval$avg_steps),]
# draw the line with the max val
abline(v=max_v$interval, lwd = 1, col = 'red')
points(max_v$interval,  max_v$avg_steps, col = 'red', lwd = 1, pch = 6)
legend('topright', lty = 1, lwd = 1, col = c("red"),
       cex = .9, 
       legend = c(paste('Max average = ', round(max_v$avg_steps, 1), ' steps on \n', max_v$interval, 'th interval', sep = ""))
)        
dev.off()

# Input missing data based on average
# Calculate how many data are missing
missing <- sum(is.na(activity$steps))
missing

# Join the datatable created earlier that summarizes the average number of steps per interval to the original dataset
# Then I will fill the missing values with the average over 5 minutes
setkey(dt, interval)
setkey(dt_by_interval, interval)

dt_missing <- dt[dt_by_interval]
# dt_missing

# Here I have created a function that replace the value x with y if x is NA
fillValues <- function(x, y) {
        v <- if(is.na(x)) y else x
        return(v)
}

# add a column new_steps with the new steps value
dt_missing$new_steps <- mapply(fillValues, dt_missing$steps, dt_missing$avg_steps)

# calculate the sum of steps per day
dt_missing_summary <- dt_missing[, list(new_steps = sum(new_steps, na.rm = TRUE)), by = date]
# head(dt_missing_summary, 30)
# plot the new histogram with the NA value replaced
png("plots/plot3.png")
create_hist(dt_missing_summary$new_steps, title = "Total Step per day with NA replaced by mean")
dev.off()

# create a function to retrieve weekday or weekend
weekpart <- function(d){
        part <- if(weekdays(d) %in% c("Saturday", "Sunday")) "weekend" else "weekday"
        return(part)
}

# Add Type of day of the Week to the dataset
dt_missing$daytype = as.factor(sapply(as.Date(dt_missing$date), FUN=weekpart))
# head(dt_missing,40)

#Summarize Dataset: Mean grouped by interval and daytype
dt_missing_summary = dt_missing[, list(avg_steps = mean(new_steps)), by = list(interval, daytype)]
# head(dt_missing_summary, 30)
png("plots/plot4.png")
xyplot(avg_steps~interval | daytype, data = dt_missing_summary,
       type = 'l',
       xlab = 'Interval',
       ylab = 'Number of Steps',
       main = "Activity patterns between weekday and weekend",
       col = c("red"),
       lwd = 1,
       layout = c(1,2))
dev.off()