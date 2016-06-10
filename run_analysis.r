run_analysis <- function(data.dir = "D:/coursera/Data_Science/Data") 
  {
  if(!file.exists(data.dir))
    {
       dir.create("data")
    }
  
  dataset.file <- paste(data.dir, "dataset.zip", sep="/")
  dataset.dir <- paste(data.dir, "UCI HAR Dataset", sep="/")
  
  # prettify names remove () characters  return names seperated by underscores
  # @param array names
  # @return array
  prettify_names <- function(names) {
    names <- gsub("[()]", "", names)
    names <- gsub("-", "_", names)
    
    names
  }
  
  
  # load datafiles in provided directory
  # We expect three files, starting with either: subject_, y_, x_
  #
  # For the X data, only return the columns provided by meanCols and stdCols
  #
  # @param string type
  # @paraam string directory
  # @param array meanCols
  # @param array stdCols
  # @return data.frame
  load_data <- function(type, directory, meanCols, stdCols) {
    subject <- read.table(sprintf("%s/%s/subject_%2$s.txt", directory, type))
    names(subject) <- "subject"
    activity <- read.table(sprintf("%s/%s/y_%2$s.txt", directory, type))
    names(activity) <- "activity"
    tempdata <- read.table(sprintf("%s/%s/X_%2$s.txt", directory, type))
    meanData <- tempdata[,meanCols[,1]]
    names(meanData) <- meanCols[,2]
    
    stdData <- tempdata[,stdCols[,1]]
    names(stdData) <- stdCols[,2]
    
    data.frame(subject, activity, meanData, stdData)
  }
  
  # load features definition and find the column numbers that correspond to `mean` and `std`
  features <- read.table(paste(dataset.dir, "features.txt", sep="/"))
  features[,2] <- prettify_names(features[,2])
  
  meanColumns <- features[grep("mean", features[,2], ignore.case = TRUE),]
  stdColumns <- features[grep("std", features[,2], ignore.case = TRUE),]
  
  # load activity definition
  activity <- read.table(paste(dataset.dir, "activity_labels.txt", sep="/"))
  
  # load test data and traing data.
  testData <- load_data("test", dataset.dir, meanColumns, stdColumns)
  trainData <- load_data("train", dataset.dir, meanColumns, stdColumns)
  
  combinedData <- rbind(testData, trainData)
  
  # replace activity id by activity name
  combinedData$activity <- activity[match(combinedData$activity, activity[,1]), 2]
  
  # return tidy aggregate
  # for every activity and subject, aggregate the mean for every column
  aggregate(. ~ activity + subject, data = combinedData, mean)
}
  # Step1. Merges the training and the test sets to create one data set.
  # setwd("D:/coursera/Data_Science/Data")
  trainData <- read.table("./data/train/X_train.txt")
dim(trainData) # 7352*561
head(trainData)
trainLabel <- read.table("./data/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./data/train/subject_train.txt")
testData <- read.table("./data/test/X_test.txt")
dim(testData) # 2947*561
testLabel <- read.table("./data/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./data/test/subject_test.txt")
joinData <- rbind(trainData, testData)
dim(joinData) # 10299*561
joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) # 10299*1
joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) # 10299*1

# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
features <- read.table("./data/features.txt")
dim(features)  # 561*2
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
joinData <- joinData[, meanStdIndices]
dim(joinData) # 10299*66
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) # 10299*68
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
head(result)
write.table(result, "data_with_means.txt") # write out the 2nd dataset
