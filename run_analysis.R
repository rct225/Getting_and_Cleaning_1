setwd("./UCI HAR Dataset")

# Load labels and features
activityLabels <- read.table("./activity_labels.txt", header = FALSE)
features <- read.table("./features.txt", header = FALSE)

# Load test data
subjectTest <- read.table("./test//subject_test.txt", header = FALSE)
xTest <- read.table("./test//X_test.txt", header = FALSE)
yTest <- read.table("./test//y_test.txt", header = FALSE)

# Load train data
subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
xTrain <- read.table("./train//X_train.txt", header = FALSE)
yTrain <- read.table("./train//y_train.txt", header = FALSE)

#create columnnames for activity labels
colnames(activityLabels)  <- c('activityId','activityType')

#create column name training subjects
colnames(subjectTrain)  <- "subjectId"

#create columm names for x data using the features data
colnames(xTrain)        <- features[,2] 

#create column name for y data
colnames(yTrain)        <- "activityId"

#bind all the training data into one frame
trainingData <- cbind(yTrain,subjectTrain,xTrain)

colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"

#bind all the testing data into one frame
testData <- cbind(yTest,subjectTest,xTest)

# combine test data and training data
final <- rbind(trainingData, testData)

# store the columnames for processing
finalNames <- colnames(final)

#grab the data columns (Std Dev and Mean) we are interested in by names
stdData <- final[grepl("std..", finalNames)]
meanData <- final[grepl("mean..", finalNames)]

# grab the activity and subject id columns too
activity <- final[grepl("activity..", finalNames)]
subject <- final[grepl("subject..", finalNames)]

# bind the columns into a final form
finalData <- cbind(activity,subject, meanData, stdData)

# add labels for each activity by merging data from activity labels
finalData <- merge(finalData,activityLabels,by='activityId',all.x=TRUE)

# update finalNames with the new column names
finalNames <- colnames(finalData)

# loop through the columns by index and replace column names with
# more descriptive names
for (i in 1:length(finalNames)) 
{
  finalNames[i] = gsub("\\()","",finalNames[i])
  finalNames[i] = gsub("-std$","StdDev",finalNames[i])
  finalNames[i] = gsub("-mean","Mean",finalNames[i])
  finalNames[i] = gsub("^(t)","time",finalNames[i])
  finalNames[i] = gsub("^(f)","freq",finalNames[i])
  finalNames[i] = gsub("([Gg]ravity)","Gravity",finalNames[i])
  finalNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",finalNames[i])
  finalNames[i] = gsub("[Gg]yro","Gyro",finalNames[i])
  finalNames[i] = gsub("AccMag","AccMagnitude",finalNames[i])
  finalNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",finalNames[i])
  finalNames[i] = gsub("JerkMag","JerkMagnitude",finalNames[i])
  finalNames[i] = gsub("GyroMag","GyroMagnitude",finalNames[i])
};

# update colnames again
colnames(finalData) <- finalNames

# create a dataset with the mean values for all obeservations
# by subjectID and Activity ID
tidyOut <- aggregate(finalData[,3:81], 
                     by=list(activityId = finalData$activityId,
                             subjectId = finalData$subjectId), mean)

#add labels for each activity by merging data from activity labels
tidyOut <- merge(tidyOut,activityLabels,by='activityId',all.x=TRUE)

# write out tidy dataset to file
write.table(tidyOut, './tidyData.txt',row.names=FALSE,sep='\t');
