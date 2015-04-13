fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# Download and unzip the data file if it does not exist already
if (!file.exists("activity.csv")) {
  download.file(fileUrl, destfile="activity.zip", method="curl")
  unzip("activity.zip")
}

# Read the data
data <- read.csv("activity.csv")
