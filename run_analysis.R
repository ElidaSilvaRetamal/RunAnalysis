## Getting and Cleaning Data Course Project

### Libraries:
library(dplyr)
library(Hmisc)
library(plyr)
library(tidyverse)

#=======================================================================================
### Step 0. Getting, downloading, unzipping  and reading dataset
    
### 0.1 - Getting the directory for the data:
if (!file.exists("data")) {
    dir.create("data")
}

### 0.2 - Dowloading the file:
fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/df1.zip")

### 0.3 Unzip dataSet to data directory
unzip(zipfile = "./data/df1.zip", exdir = "./data")

### 0.4 Reading files and assigning column names:
#### 0.4.1 features:
features <- read.table('./data/UCI HAR Dataset/features.txt', col.names = c("identifier", "signals"))
head(features,15)
dim(features)
#### 0.4.2 activity_labels
activities<- read.table("./data/UCI HAR Dataset/activity_labels.txt", col.names = c("activity","labels"))
head(activities)
#### 0.4.3 subject_train, X_train and y_train
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", col.names = features$signals)

y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names = "activity")

#### 0.4.4 subject_test, X_test and y_test
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", col.names = features$signals)

y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = "activity" )

#### 0.4.5 Size data set - Number rows and columns.

dim(X_train)  #[1] 7352  561
dim(y_train)  #[1] 7352    1
dim(subject_train) #[1] 7352    1
dim(X_test) #[1] 2947  561
dim(y_test) #[1] 2947    1
dim(subject_test) #[1] 2947    1
dim(activities) #[1] 6 2
dim(features) #[1] 561   2

#===============================================================================================
### Step 1.  Merges the training and the test sets to create one data set.

my <- rbind(y_train, y_test); dim(my)
mX <- rbind(X_train, X_test); dim(mX)
msubject <- rbind(subject_train,subject_test); dim(msubject)
merge_dataset <- cbind(my, mX, msubject); dim(merge_dataset)
#View(merge_dataset)

#===============================================================================================
#Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.

#2.1 Tidy dataset:
data_tidy <- merge_dataset %>%
    select(subject,
           activity,
           contains("mean"),
           contains("std"))
dim(data_tidy) # [1] 10299    88
view(data_tidy)
head(data_tidy)
#============================================================================================
#Step 3. Uses descriptive activity names to name the activities in the data set

data_tidy$activity <- activities[data_tidy$activity, 2]

#==========================================================================================
#Step 4. Appropriately labels the data set with descriptive variable names.
# 4.1  list of names from data_tidy

names(data_tidy)

# 4.2 Names that will be replaced by descriptive names:

# t: Time
# f: Frequency
# Freq..: Frequency
# Acc: Accelerometer
# Gyro: Gyroscope
# Mag: Magnitude
# BodyBody: Body
# gravity: Gravity
# mean: Mean 

names(data_tidy) <- gsub("Freq","Frequency",names(data_tidy), ignore.case = TRUE)
names(data_tidy) <- gsub("^t","Time",names(data_tidy))
names(data_tidy) <- sub(".tB","TimeB",names(data_tidy), ignore.case = FALSE)
names(data_tidy) <- gsub("^f","Frequency",names(data_tidy))
names(data_tidy) <- gsub("Acc","Accelerometer",names(data_tidy))
names(data_tidy) <- gsub("Mag","Magnitude",names(data_tidy))
names(data_tidy) <- gsub("gravity","Gravity",names(data_tidy))
names(data_tidy) <- gsub("Gyro","Gyroscope",names(data_tidy))
names(data_tidy) <- gsub("BodyBody","Body",names(data_tidy))
names(data_tidy) <- gsub("mean", "Mean", names(data_tidy))

# 4.3  data_tidy with descriptive names
names(data_tidy)

#=============================================================================================
# Step 5. From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.

second_data <- data_tidy %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))

head(second_data)
names(second_data)
view(second_data)
