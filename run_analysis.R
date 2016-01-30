#allows for Ddplyr
library(plyr)

#Read and mergers raw data 
trainX <- read.table("UCI HAR Dataset/train/X_train.txt")
testX <- read.table("UCI HAR Dataset/test/X_test.txt")
trainsub <- read.table("UCI HAR Dataset/train/subject_train.txt")
testsub <- read.table("UCI HAR Dataset/test/subject_test.txt")
trainY <- read.table("UCI HAR Dataset/train/y_train.txt")
testY <- read.table("UCI HAR Dataset/test/y_test.txt")
features<- read.table("UCI HAR Dataset/features.txt")
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activityL <- read.table("UCI HAR Dataset/activity_labels.txt")

## Merges the training and the test data into sets
DataX <- rbind(trainX, testX)
DataY <- rbind(trainY,testY)
DataSub<-rbind(trainsub,testsub)
NewData<-cbind(DataX, DataY, DataSub)

# get only columns with mean() or std() in their names
meanstd <- grep("-(mean|std)\\(\\)", features[, 2])

# subsets columns
DataX <- DataX[, meanstd]

# correct column names

names(DataX) <- features[meanstd, 2]

# update values with correct activity names

DataY[, 1] <- activityL[DataY[, 1], 2]

# correct column names

names(DataY) <- "activity"


# labels the data set with  variable names

names(DataSub) <- "subject"

# Create a second, independent tidy data set

final <- ddply(NewData, .(subject, activity), function(x) colMeans(x[, 1:66]))
write.table(final, "tidydata.txt", row.name=FALSE)
