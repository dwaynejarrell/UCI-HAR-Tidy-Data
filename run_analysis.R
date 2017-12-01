###Install dplyr package for data manipulation
install.packages("dplyr")
library(dplyr)

###Read in features dataset to get raw variable names from second column
temp <- read.table("features.txt", sep = "")
attributeNames <- temp$V2

###Read in training dataset with all measurements and assign names from the features file
Xtrain <- read.table("train/X_train.txt", sep = "")
names(Xtrain) <- attributeNames

###Read in test dataset with all measurements and assign names from the features file
Xtest <- read.table("test/X_test.txt", sep = "")
names(Xtest) <- attributeNames

###Combine train and test data rows using rbind
Xtraintest <- rbind(Xtrain, Xtest)

###Identify mean and std variables from the raw variable names
name_index = grep("mean\\(|std\\(",attributeNames)

###Using the index created in the last step, subset columns in the combined dataset to include only mean, std
Xall_meanstd <- Xtraintest[,name_index]

###Read in activity labels
activities <- read.table("activity_labels.txt", sep = "")
names(activities) <- c("activity_code","activity")

###Read in activity files for train and test, name the only column "activity_code"
ytrain <- read.table("train/y_train.txt", sep = "")
names(ytrain) <- "activity_code"
ytest <- read.table("test/y_test.txt", sep = "")
names(ytest) <- "activity_code"

###Combine activity files and add a sequence number to keep the order when merging
ytraintest <- rbind(ytrain, ytest)
ytraintest <- cbind(ytraintest, sequence=seq.int(1,nrow(ytraintest),1))

###Merge activity data with labels to clearly identify each activity, sort by sequence number
traintest_act <- merge(ytraintest, activities, by.x = "activity_code", by.y = "activity_code")
traintest_act <- traintest_act[order(traintest_act$sequence),]

###Read in subject numbers for train and test, combine rows using rbind
subject_train <- read.table("train/subject_train.txt", sep = "")
names(subject_train) <- "subject"
subject_test <- read.table("test/subject_test.txt", sep = "")
names(subject_test) <- "subject"
subjects <- rbind(subject_train, subject_test) 
        
###Create final tidy dataset by combining columns from subjects, activities and measurements
meanstd_activity <- cbind(subjects, activity=traintest_act$activity, Xall_meanstd)

###Clean up measurement variable names nbny eliminating special characters
names(meanstd_activity) <- gsub("-|\\(|\\)","",names(meanstd_activity))

###Create a new tidy dataset that contains means by subject and activity for every measure
averages <- 
        meanstd_activity %>%
        group_by(subject, activity) %>%
        summarize_all(mean)

###Write out the means dataset to a text file
write.table(averages, "HAR_meanstd_averages.txt", row.names = FALSE)

