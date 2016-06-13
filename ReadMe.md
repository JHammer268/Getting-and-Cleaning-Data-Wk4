---
title: "ReadMe"
author: "JMitchell"
date: "June 12, 2016"
output: html_document
---

## Week 4 project: Getting and Cleaning Data.

### Documentation of methodolgy used for Merging and Cleaning the UCI Smartphone Dataset.  

* This documentation provides a writeup of each step followed by the code from run_analysis.R that perfomed the given step.

### Inital Setup and Data Import:

```{r}
## Import packages

library(dplyr)
library(tidyr)

## Read in Train and Test files, activity files, and subject files and merge into one Dataframe
## using cbind() and dyplr's bind_rows()

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("download.zip")) {
        download.file(fileURL, destfile="download.zip")
}
unzip("download.zip")
```

### Step 1: Merge training and test data into one data set.

* This is a three step process:
   1. Read and merge the train files:
      * **x_train.txt:** Column data for 561 parameters  
      * **y_train.txt:**    Numeric activity id's for each row of x_train.txt  
      * **subject_train.txt:**  Numeric subject id's for each row of x_train.txt
      * bind on columns into temp variable "train".
      
   2. Read and merge the test files:
      * **x_test.txt:** Column data for 561 parameters  
      * **y_test.txt:**    Numeric activity id's for each row of x_test.txt  
      * **subject_train.txt:**  Numeric subject id's for each row of x_test.txt
      * bind on columns into temp variable "test".
   3. Merge the train and test data by row into **"allData"**

```{r}
########## Merging train Data ##############

train <- read.table("./UCI HAR Dataset/train/X_train.txt")

trainActivites <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names="activity_id")
train <- cbind(trainActivites, train)

trainSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names="subject")
train <- cbind(trainSubjects, train)

############ Merging Test Data ############

test <- read.table("./UCI HAR Dataset/test/X_test.txt")

testActivites <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names="activity_id")
test <- cbind(testActivites, test)

testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names="subject")
test <- cbind(testSubjects, test)

###  Merging Train and Test into one DataFame ###
allData <- bind_rows(train, test)

#----------- End of Merge Code --------------#
```

### 2. Extract only the measurments on the mean and standard deviation for each measurement.
* There are 6 steps in this section. This is the bulk of the project.
   1. Import **features.txt** which contains the variable(column) names for the original 561 columns of allData.  
   2. Remove 84 variables that had dublicate names.  None of these varaibles contained mean or std data.
   3. Clean up variable names by removing excess characters such as "()" and"-".
   4. Name the columns of allData with the cleaned up name list.
   5. Use a text search to create an index of columns containing Mean and Std data.
      * Note: 13 columns contained "MeanFrequency" data.  I removed these columns from the dataset because MeanFrequency is not equivalent to Mean, which was specified in the project instructions.
   6. Filter the data to select only Mean and Std columns.
   
```{r}
#--------- Begin Data Cleanup Code ----------#

## Read in features.txt which contains column(varible) names
## and perform some initial cleanup.

features <- read.table("./UCI HAR Dataset/features.txt", col.names =c("row_num", "name"),stringsAsFactors =FALSE)

## There are 84 columns with duplicate variable names.
## These columns make dyplr's select() function unhappy and
## none of these has mean or std data so I will remove them:

duplicates <- duplicated(features$name)
allData <- allData[,!duplicates]
features <- features[!duplicates,]

#drop a redundant column containing row numbers.
features <- select(features, name) 


## Clean up the variable Names. 

features$name <- gsub("-","", features$name)
features$name <- sub("\\,","", features$name)
features$name <- sub("\\,","", features$name)
features$name <- sub("\\(","", features$name)
features$name <- sub("\\)","", features$name)
features$name <- sub("mean","Mean", features$name)
features$name <- sub("std","Std", features$name)

names(allData) <- c("subject", "activity_id", features$name)

## Generate index of variables containing "mean" or "std"
meanColumns <- grep(".*Mean.*", features$name)

stdColumns <- grep(".*Std.*", features$name)

columnIndex <- sort(c(meanColumns, stdColumns)) #merge into 1 list and sort

# Generate index of variable names that contain "meanFreq"  
meanFreq <- grep(".*MeanFreq.*",features$name)

#generate list of mean and std variables exluding meanFreq variables
columnIndex <- columnIndex[!(columnIndex %in% meanFreq)]

## Since columnIndex was generated from the features dataframe I need to
## account for 2 extra rows I created in df for subject and activity before
## using it in select().
columnIndex <- columnIndex + 2 

## Now select only "subjct", "activity_id"  and mean and std columns from allData

allData <- select(allData, subject, activity_id, columnIndex)
```
### 3. Use Descriptive activity names for the dataset.
* Three Steps:
  1.  Read in **activity_labels.txt** which contains the numeric code and corresponding name of each activity: "laying", "sitting", "standing", etc.
  2. Merge allData with activity on the numeric activity_id.
  3. reselect columns of allData to drop the numeric activity id and bring the descriptive name to column position 2.
  
```{r}
## Read in activity names ##

activities <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                         col.names = c("id","activity"), 
                         stringsAsFactors = FALSE)

## Merge activity names into allData based on activity_id and reorder
allData <- merge(allData,activities, by.x="activity_id", by.y="id")
        
allData <- select(allData, subject, activity, 3:68)
```

### 4. Appropriately label the dataset with descriptive variable names.

* This was accomplished as part of step 2 above, immediately after cleaning up the variable names.

### 5. Create a second, independent tidy dataset with the average of each variable, for each activity, for each subject.
* This was the shortest part of the assignment with only two lines of code. The resulting table is tidy based on the following criteria:
   1. Variable names are unique, descriptive, and do not contain superfluous characters..  I intentionally retained some capital letters in the names to maintain readability.
   2. Each column contains 1 variable. While it would be possible to separate X,Y,Z data from reading type and create a "tall" dataset, this would introduce a great deal of Null values and therefore I believe the "wide" format to be more tidy.
   3. each row contains one set of values for a given subject and activity.
   
```{r}
## Extract the mean values for each subect, group, and variable into a summary table

summaryData<- allData%>% group_by(subject, activity) %>%
                    summarize_each(funs(mean))

write.csv(summaryData, file="summaryData.csv")
```

## The End!
Thanks for going through this description.  My code got the job done, but I'm pretty sure it's not the most elegant solution possible, so any feedback you have on my code would be much appreciated!