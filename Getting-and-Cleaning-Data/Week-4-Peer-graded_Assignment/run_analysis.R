##-----------------------Week 4 Peer graded Assignment------------------------##
##                                                                            ##
##                                                                            ##
##  This script is part of the Getting and Cleaning Data Module of the Data   ##
##  Science Specialization, offered by Johns Hopkins University through       ##
##  Coursera platform.                                                        ##
##                                                                            ##
##----------------------------------------------------------------------------##

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each 
##    measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject.

## Load packages
library(dplyr)
library(stringr)

## Set work directory
#
setwd("C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Peer-graded_Assignment/")

## Set url and file
#
lk <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

dataset <- "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Peer-graded_Assignment/data/dataset.zip"

## Downloads
#
if(!dir.exists("./data")){
        dir.create("data")
}

if(!file.exists("./data/dataset.zip")){
download.file(url = lk,
              destfile = dataset,
              mode = "wb")
        
        ## Unzip files
        unzip(zipfile = dataset,
              exdir = "./data")
}




## List files in the current work directory
#
list.files(path = "./data", full.names = T, recursive = TRUE)

## Read feature list and activity labels
#
features <- read.table(file = "./data/UCI HAR Dataset/features.txt",
                       col.names = c("id", "feature"))[,2]

str(features)
tail(features)

activity_labels <- read.table(file = "./data/UCI HAR Dataset/activity_labels.txt",
                       col.names = c("code", "activity"))

head(activity_labels)

## Extract only the names of eache variable

## Renames the variables
#
features <- str_replace_all(features, pattern = "\\(", replacement = "")
features <- str_replace_all(features, pattern = "\\)", replacement = "")
features <- str_replace_all(features, pattern = "\\,", replacement = ".")
features

## Read datasets
## training sets
#
X_train <- read.table(file = "./data/UCI HAR Dataset/train/X_train.txt",  
                      col.names = features)

y_train <- read.table(file = "./data/UCI HAR Dataset/train/y_train.txt", 
                      col.names = "code")


subject_train <- read.table(file = "./data/UCI HAR Dataset/train/subject_train.txt", 
                            col.names = "subject")

## test sets
#
X_test <- read.table(file = "./data/UCI HAR Dataset/test/X_test.txt",
                     col.names = features)

y_test <- read.table(file = "./data/UCI HAR Dataset/test/y_test.txt",  
                     col.names = "code")

subject_test <- read.table(file = "./data/UCI HAR Dataset/test/subject_test.txt",  
                           col.names = "subject")

## Merge data sets
#
X <- rbind(X_train, X_test)  ## Merge X_training and X_test


Y <- rbind(y_train, y_test)  ## Merge y_training and y_test

## Uses descriptive activity names to name the activities in the data set
#
Y <- Y %>%
        mutate(activity = "") %>%
        full_join(activity_labels, by = "code", keep = TRUE) %>%
        mutate(activity = activity.y) %>%
        select(activity)

## Merge the data set subject training and subject test
#
subject_train_test <- rbind(subject_train, subject_test)

## Merge all data sets
#
df <- cbind(subject_train_test, Y, X)

## get more information about the new data frame
#
str(df)

## Extracts only the measurements on the mean and standard deviation for each 
## measurement. 
#
df2 <- df %>%
        select(contains(c("mean", "std", "subject", "activity")))

## Appropriately labels the data set with descriptive variable names
#
names(df2) <- gsub("Acc", "Accelerometer", names(df2))
names(df2) <- gsub("Gyro", "Gyroscope", names(df2))
names(df2) <- gsub("BodyBody", "Body", names(df2))
names(df2) <- gsub("Mag", "Magnitude", names(df2))
names(df2) <- gsub("^t", "Time", names(df2))
names(df2) <- gsub("^f", "Frequency", names(df2))
names(df2) <- gsub("tBody", "TimeBody", names(df2))
names(df2) <- gsub("-mean()", "Mean", names(df2), ignore.case = TRUE)
names(df2) <- gsub("-std()", "STD", names(df2), ignore.case = TRUE)
names(df2) <- gsub("-freq()", "Frequency", names(df2), ignore.case = TRUE)
names(df2) <- gsub("angle", "Angle", names(df2))
names(df2) <- gsub("gravity", "Gravity", names(df2))

as_tibble(df2)

## Creates a second, independent tidy data set with the average of each variable 
## for each activity and each subject.
#
df3 <- df2 %>%
        group_by(activity, subject) %>%
        summarize_all(list(mean))

as_tibble(df3)

## Save tidy data set
write.csv(df3, 
          "C:/Data-Science-Foundations-using-R-Specialization/Getting-and-Cleaning-Data/Week-4-Peer-graded_Assignment/tidyDF.csv",
          row.names = FALSE)
