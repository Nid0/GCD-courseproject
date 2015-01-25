## Getting and Cleaning Data: Course Project.

library(dplyr)
library(data.table)

## Create a folder for working data in the working directory:
if (!file.exists("CP")) {
        dir.create("CP")
}

activity_labels <- read.table("~/downloads/UCI-HAR-Dataset/activity_labels.txt")
features <- read.table("~/downloads/UCI-HAR-Dataset/features.txt")
activity_labels
features

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "~/downloads/UCI-HAR-Dataset.zip"
download.file(fileUrl, destfile, method="curl")
unzip(destfile)

setwd("~/downloads/UCI-HAR-Dataset")

## Defining data frames &
## 1-Merges the training and the test sets to create one data set:
test1 <- read.table("test/X_test.txt")
train1 <- read.table("train/X_train.txt")
X <- merge(test1, train1, all=TRUE)

test2 <- read.table("test/y_test.txt")
train2 <- read.table("train/y_train.txt")
y <- merge(test2, train2, all=TRUE)

test3 <- read.table("test/subject_test.txt")
train3 <- read.table("train/subject_train.txt")
sub <- merge(test3, train3, all=TRUE)


## 2-Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("features.txt")
indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices]
names(X) <- features[indices, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X)) 


## 3-Uses descriptive activity names to name the activities in the data set
activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
y[,1] = activities[y[,1], 2]
names(y) <- "activity"


## 4-Appropriately labels the data set with descriptive variable names. 
names(sub) <- "subject"
cleaned <- cbind(sub, y, X)
write.table(cleaned, "mergeddata.txt")


## 5-From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
uniqueSubjects = unique(sub)[,1]
numSubjects = length(unique(sub)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
        for (a in 1:numActivities) {
                result[row, 1] = uniqueSubjects[s]
                result[row, 2] = activities[a, 2]
                tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
                result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}
write.table(result, "average_dataset.txt")
