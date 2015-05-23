# R script called run_analysis.R that does the following. 
#  1. Merges the training and the test sets to create one data set.
#  2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#  3. Uses descriptive activity names to name the activities in the data set
#  4. Appropriately labels the data set with descriptive variable names. 
#  5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

require("reshape2")
require("data.table")

get_data <- function(fileX, fileY, fileSubject, labelsX, labelsX_selection, labelsY) {
    dataX <- read.table(fileX)
    dataY <- read.table(fileY)
    dataSubject <- read.table(fileSubject)
    
    names(dataX) <- labelsX
    dataX        <- dataX[,labelsX_selection]
    dataY[,2]    <- labelsY[dataY[,1]]
    names(dataY) <- c("activity_id", "activity_label")
    names(dataSubject) <- "subject"
    
    result <- cbind( as.data.table(dataSubject), dataX, dataY )
    result
}

activity_labels  <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
feature_labels   <- read.table("./UCI HAR Dataset/features.txt")[,2]
feature_mean_std <- grepl("mean|std", feature_labels)

# Collect test data
dataTest <- get_data( 
    fileX       = "./UCI HAR Dataset/test/X_test.txt",
    fileY       = "./UCI HAR Dataset/test/y_test.txt",
    fileSubject = "./UCI HAR Dataset/test/subject_test.txt",
    labelsX           = feature_labels,
    labelsX_selection = feature_mean_std,
    labelsY           = activity_labels
)

# Collect train data
dataTrain <- get_data(
    fileX       = "./UCI HAR Dataset/train/X_train.txt",
    fileY       = "./UCI HAR Dataset/train/y_train.txt",
    fileSubject = "./UCI HAR Dataset/train/subject_train.txt",
    labelsX           = feature_labels,
    labelsX_selection = feature_mean_std,
    labelsY           = activity_labels
)


# Combine them both
data        <- rbind(dataTest, dataTrain)
id_labels      <- c("subject", "activity_id", "activity_label")
measure_labels <- setdiff( colnames(data), id_labels)
melt_data   <- melt(data, id=id_labels, measure.vars = measure_labels)


# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, subject + activity_label ~ variable, fun.aggregate = mean)

write.table(tidy_data, file = "./tidy_data.txt")
