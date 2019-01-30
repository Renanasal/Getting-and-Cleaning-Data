## Step 1: Merges the training and the test sets to one data sett
# read data into data-frame
test_x <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")

train_x <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
subject_train <-read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")

featuresNames <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
activitylabels <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")

# add name column 
names(subject_test) <- "subject"
names(subject_train) <- "subject"

names(test_x) <-featuresNames$V2
names(train_x) <- featuresNames$V2

names(test_y) <- "activity"
names(train_y) <- "activity"

# merge the data-frames 
train <- cbind(subject_train,train_y,train_x)
test <- cbind(subject_test,test_y,test_x)
combind <-rbind(train,test, deparse.level = 1)

#step 2: extract the mean and std 
meanstd <- grepl(".*mean.*", names(combind)) | grepl(".*std*", names(combind))
meanstd[1:2] <- TRUE
combind <- combind[,meanstd]

#step 3: Uses descriptive activity names 
combind$activity <- as.character(combind$activity)
for (i in 1:6) {
  combind$activity[combind$activity ==i] <-as.character(activitylabels[i,2])
}
combind$activity <- as.factor(combind$activity)

#step 4: labels the data set with descriptive variable names
names(combind) <- gsub("^t","Time",names(combind))
names(combind) <- gsub("^f","Frequency",names(combind))
names(combind) <- gsub("Acc","Accelerometer", names(combind))
names(combind) <- gsub("Gyro","Gyroscope", names(combind))
names(combind) <- gsub("Mag","Magnitude", names(combind))
names(combind) <- gsub("BodyBody","Body", names(combind))
names(combind) <- gsub("-mean()","Mean", names(combind))
names(combind) <- gsub("-std()","STD", names(combind))
names(combind) <- gsub("BodyBody","Body", names(combind))

#step 5: creates a second tidy data
combind$subject <-as.factor(combind$subject)
combind <-data.table(combind)

tidydata <- aggregate(.~subject +activity, combind,mean)
tidydata <- tidydata[order(tidydata$subject,tidydata$activity),]
write.table(tidydata, file ="tidydata.txt", row.names = FALSE)
