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
step_per_day <- summarize(per_day, total_step = sum(steps, na.rm = TRUE))

# 2- Make a histogram of the total number of steps taken each day
hist(step_per_day$total_step, col = "blue", main = "Total number of steps per day",
     xlab = "number of steps")

# 3- Calculate and report the mean and median of the total number of steps taken per day
mean_steps_per_day <- mean(step_per_day$total_step, na.rm = TRUE)
median_steps_per_day <- median(step_per_day$total_step, na.rm = TRUE)

