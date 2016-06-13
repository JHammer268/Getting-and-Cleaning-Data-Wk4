## Getting and Cleaning Data.  Final Project
## Goal: Load, merg, and clean the data, then condense into  
## Average data for each variable.
##
##

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

## Read in activity names ##

activities <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                         col.names = c("id","activity"), 
                         stringsAsFactors = FALSE)

## Merge activity names into allData based on activity_id and reorder
allData <- merge(allData,activities, by.x="activity_id", by.y="id")
        
allData <- select(allData, subject, activity, 3:68)

## Extract the mean values for each subect, group, and variable into a summary table

summaryData<- allData%>% group_by(subject, activity) %>%
                    summarize_each(funs(mean))

write.csv(summaryData, file="summaryData.csv")
