fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# Download and unzip the data file if it does not exist already
if (!file.exists("activity.csv")) {
  download.file(fileUrl, destfile="activity.zip", method="curl")
  unzip("activity.zip")
}

# Loading and preprocessing the data
data <- read.csv("activity.csv")


# What is mean total number of steps taken per day?
# 1- Calculate the total number of steps taken per day

library(dplyr)
per_day <- group_by(data, date)
step_per_day <- summarise(per_day, total_step = sum(steps, na.rm = TRUE))

# 2- Make a histogram of the total number of steps taken each day
hist(step_per_day$total_step, col = "blue", main = "Total number of steps per day",
     xlab = "number of steps")

# 3- Calculate and report the mean and median of the total number of steps taken per day
mean_steps_per_day <- mean(step_per_day$total_step, na.rm = TRUE)
median_steps_per_day <- median(step_per_day$total_step, na.rm = TRUE)


# What is the average daily activity pattern?
# 1- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)

interval_per_day <- summarise(group_by(per_day, interval), 
                              avg_steps = mean(steps, na.rm = TRUE))

plot(interval_per_day$interval, interval_per_day$avg_steps, type = "l",
     main = "average daily activity", xlab = "5-minute interval",
     ylab = "average number of steps")

# 2- Which 5-minute interval, on average across all 
# the days in the dataset, contains the maximum number of steps?

max_steps <- filter(interval_per_day, avg_steps == max(interval_per_day$avg_steps, na.rm = TRUE) )
max_steps$interval


# Imputing missing values
# 1- Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)

total_NA <- length(data[is.na(data)])

# 2- trategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.

filled_data <- data %>%
  group_by(interval) %>%
  mutate(steps = replace(steps, is.na(steps), mean(steps, na.rm=TRUE)))


# 4- Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

filled_per_day <- group_by(filled_data, date)
filled_step_per_day <- summarise(filled_per_day, total_step = sum(steps))

# Make a histogram of the total number of steps taken each day
hist(filled_step_per_day$total_step, col = "blue", main = "Total number of steps per day",
     xlab = "number of steps")

# Calculate and report the mean and median of the total number of steps taken per day
mean_filled_steps_per_day <- mean(filled_step_per_day$total_step, na.rm = TRUE)
median_filled_steps_per_day <- median(filled_step_per_day$total_step, na.rm = TRUE)

# Comparaison

par(mfrow = c(1, 2))
hist(step_per_day$total_step, col = "blue", main = "With NA",
     xlab = "number of steps")
hist(filled_step_per_day$total_step, col = "blue", main = "Without NA",
     xlab = "number of steps")

# Are there differences in activity patterns between weekdays and weekends?
# Using the new filled-in data set
# 1- Create a new factor variable in the dataset with two levels – “weekday” 
# and “weekend” indicating whether a given date is a weekday or weekend day.
filled_data["weekday"] <- weekdays(as.Date(filled_data$date))

filled_data <- filled_data %>%
  mutate(weekday = replace(weekday, weekday == "sábado" | weekday == "domingo", "weekend"))
         
filled_data <- filled_data %>%
  mutate(weekday = replace(weekday, weekday != "sábado" | weekday != "domingo", "weekday"))

# 2- Make a panel plot containing a time series plot (i.e. type = "l") of 
# the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis)
