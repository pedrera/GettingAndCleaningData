runAnalysis <- function() {

  # Downloading data
  zip <- "dataSet.zip"
  dir <- "UCI HAR Dataset"
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  if(!file.exists(zip)) {
    download.file(url, zip, method = "curl")
  }
  
  # Unzipping file
  if(!file.exists(dir)) {
    unzip(zip, exdir = ".")
  }

  # Getting data
  XTrain <<- read.table(paste(dir, "train/X_train.txt", sep = "/"))
  XTest <<- read.table(paste(dir, "test/X_test.txt", sep = "/"))
  yTrain <- read.table(paste(dir, "train/y_train.txt", sep = "/"))
  yTest <- read.table(paste(dir, "test/y_test.txt", sep = "/"))
  subjectTrain <- read.table(paste(dir, "train/subject_train.txt", sep = "/"))
  subjectTest  <- read.table(paste(dir, "test/subject_test.txt", sep = "/"))
  dataNames <- read.table(paste(dir, "features.txt", sep = "/"))[, 2]
  
  # Getting And Setting names
  XTrainXTestMerged <- rbind(XTrain, XTest)
  names(XTrainXTestMerged) <- dataNames
  
  # Extracting mean and standard deviation
  meanStd <- grep("(mean|std)\\(\\)", names(XTrainXTestMerged))
  lData <- XTrainXTestMerged[, meanStd]
  
  # Getting activity data and Setting names
  yMerged <- rbind(yTrain, yTest)[, 1]
  actNames <- c("walking",
                "walking Upstairs",
                "walking downstairs",
                "sitting",
                "standing",
                "laying")
  act <- actNames[yMerged]
  
  # Changing label the data
  names(lData) <- gsub("^t", "Time", names(lData))
  names(lData) <- gsub("^f", "Frequency", names(lData))
  names(lData) <- gsub("-mean\\(\\)", "Mean", names(lData))
  names(lData) <- gsub("-std\\(\\)", "StdDev", names(lData))
  names(lData) <- gsub("-", "", names(lData))
  names(lData) <- gsub("BodyBody", "Body", names(lData))
  
  # Adding activities and subject
  subjectTrainTest <- rbind(subjectTrain, subjectTest)[, 1]
  tidyData <- cbind(Subject = subjectTrainTest, Activity = act, lData)
  
  # Column means for all but the subject and activity columns
  limitMeansFunction <- function(data) { colMeans(data[,-c(1,2)]) }
  tidyMeans <- ddply(tidyData, .(Subject, Activity), limitMeansFunction)
  names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])
  
  # Writting file
  write.table(tidyMeans, "tidyDataMeans.txt", row.names = FALSE)
  
  # Return
  tidyMeans
}