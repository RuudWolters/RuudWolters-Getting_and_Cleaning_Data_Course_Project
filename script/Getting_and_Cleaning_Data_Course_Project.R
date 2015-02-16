library(reshape2)
library("stringr")

setwd("C:/Users/Wolters/Google Drive/Coursera/Statistiek/R_WORK/cousera/getting and cleaning data/data")

# If we downloaded and created a dataset and unpacked it. We don't want to do this again
# So we test if we saved the dataset. Without any modifications!!!!! 

#check if we are missing any dataset
      if (  !file.exists("HAR_trainingX_dataset.rds") |
            !file.exists("HAR_trainingActivity_dataset.rds") |
            !file.exists("HAR_trainingSubjectid_dataset.rds") |
            !file.exists("HAR_testingX_dataset.rds") |
            !file.exists("HAR_testingActivity_dataset.rds") |
            !file.exists("HAR_testingSubjectid_dataset.rds") |
            !file.exists("HAR_features_dataset.rds") 
        )
{
      # We test if there is already a download (but no dataset saved!) download once!
      if (!file.exists("HAR_Dataset.zip"))
      {
            # if we don't have the download we download the original dataset
            download.file("https://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI%20HAR%20Dataset.zip", "HAR_Dataset.zip", quiet = FALSE, mode = "wb")
      }
      # We unzip it
      unzip("HAR_Dataset.zip", overwrite = TRUE, exdir = ".", unzip = "internal")

      # We read the training datasets
      # and we save them
      trainingX = read.table("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
      trainingActivity = read.table("./UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
      trainingSubjectid = read.table("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
      saveRDS (trainingX, file="HAR_trainingX_dataset.rds")
      saveRDS (trainingActivity, file="HAR_trainingActivity_dataset.rds")
      saveRDS (trainingSubjectid, file="HAR_trainingSubjectid_dataset.rds")
      
      # We read the testing datasets
      # and we save them
      testingX = read.table("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
      testingActivity = read.table("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
      testingSubjectid = read.table("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
      saveRDS (testingX, file="HAR_testingX_dataset.rds")
      saveRDS (testingActivity, file="HAR_testingActivity_dataset.rds")
      saveRDS (testingSubjectid, file="HAR_testingSubjectid_dataset.rds")
      
      # We read the faetures dataset and we save this
      features <- read.table("./UCI HAR Dataset//features.txt")
      saveRDS (features, file="HAR_features_dataset.rds")   
} else
{
      # ok we downloaded, unpacked the dataset and combined it and it was saved, so we can reuse it.
      training <- readRDS("HAR_trainingX_dataset.rds")
      trainingActivity <- readRDS("HAR_trainingActivity_dataset.rds")
      trainingSubjectid <- readRDS("HAR_trainingSubjectid_dataset.rds")
      testing <- readRDS("HAR_testingX_dataset.rds")
      testingActivity <- readRDS("HAR_testingActivity_dataset.rds")
      testingSubjectid <- readRDS("HAR_testingSubjectid_dataset.rds")
      features <- readRDS("HAR_features_dataset.rds")
}


# Before this point the only manipulation to the dataset was the combination of X, Y and subject
# From this point, the dataset is manipulated!!!!
#
#Fix Column Names:
colnames(training) <- t(features[2])
colnames(testing) <- t(features[2])

#combine (training and testing) datasets with the features and subjectid
training$activity <- trainingActivity[, 1]
training$subjectid <- trainingSubjectid[, 1]
testing$activity <- testingActivity[, 1]
testing$subjectid <- testingSubjectid[, 1]

#########################
#
# Assignment !
# 1.: Merges the training and the test sets to create one data set.

# Merge training and test sets together
completeDataSet <- rbind(training, testing)

#########################
#
# Assignment !
# 2.: Extracts only the measurements on the mean and standard deviation for each measurement.

# Get the columns for mean and std. dev.
columnsWanted <- grep(".*mean().*|.*std().*", features[,2])

# create a new features df with only the wanted columns
features <- features[columnsWanted,]

# Add the last two columns ( activity and subjectid )
columnsWanted <- c(columnsWanted, 562, 563)

# create a df with all the wanted data
subsetDataSet <- completeDataSet[,columnsWanted]

#########################
#
# Assignment !
# 3.: Uses descriptive activity names to name the activitiy in the data set

subsetDataSet$activitiy <- as.character(subsetDataSet$activity)
subsetDataSet$activitiy[subsetDataSet$activitiy == 1] <- "Walking"
subsetDataSet$activitiy[subsetDataSet$activitiy == 2] <- "Walking Upstairs"
subsetDataSet$activitiy[subsetDataSet$activitiy == 3] <- "Walking Downstairs"
subsetDataSet$activitiy[subsetDataSet$activitiy == 4] <- "Sitting"
subsetDataSet$activitiy[subsetDataSet$activitiy == 5] <- "Standing"
subsetDataSet$activitiy[subsetDataSet$activitiy == 6] <- "Laying"
subsetDataSet$activitiy <- as.factor(subsetDataSet$activitiy)

#########################
#
# Assignment !
# 4.: Appropriately labels the data set with descriptive variable names.

# first char then dot 
# then every capital to a dot and to lowercase
colnames(subsetDataSet) <- tolower(str_replace_all(colnames(subsetDataSet), "([A-Z]{1})", ".\\1"))
#remove multiple dots
colnames(subsetDataSet) <- str_replace_all(colnames(subsetDataSet), "[\\.]+", ".")
#remove dots at the end
colnames(subsetDataSet) <- str_replace_all(colnames(subsetDataSet), "[\\.]+$", "")
#remove ( and ) with -
colnames(subsetDataSet) <- str_replace_all(colnames(subsetDataSet), "\\(\\)-", "")
#remove ( and ) without -
colnames(subsetDataSet) <- str_replace_all(colnames(subsetDataSet), "\\(\\)", "")
#make mean (mean)
colnames(subsetDataSet) <- str_replace_all(colnames(subsetDataSet), "-mean", "(mean)")
#make std (std)
colnames(subsetDataSet) <- str_replace_all(colnames(subsetDataSet), "-std", "(std)")


#########################
#
# Assignment !
# 5.: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject..

subsetDataSetClean <- melt(subsetDataSet, id.vars=c("subjectid", "activitiy"))
subsetDataSetClean <- dcast(subsetDataSetClean, subjectid + activitiy ~ variable, mean)
saveRDS (subsetDataSetClean, file="HAR_clean_dataset.rds")
write.table(subsetDataSetClean,file="HAR_clean_dataset.txt", row.name=FALSE)


