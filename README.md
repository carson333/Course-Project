## Read in the data if it does not exist
if (!exists("merged")) {
  test <- read.table("UCI HAR Dataset/test/X_Test.txt")
  test_labels <- read.table("UCI HAR Dataset/test/Y_Test.txt")
  train <- read.table("UCI HAR Dataset/train/X_train.txt")
  train_labels <- read.table("UCI HAR Dataset/train/Y_train.txt")
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  features <- read.table("UCI HAR Dataset/features.txt")
## Merge the Train and Test data
  subjects <- rbind(subject_test, subject_train)
  merged <- rbind(test, train)
  merged <- cbind(subjects, merged)
  x <- rbind(test_labels, train_labels)
  labels <- NULL
  for (i in 1:10299) {
    if (x[i,1] == 1) {
      labels <- c(labels, "Walking")
      } else if (x[i,1] == 2) {
        labels <- c(labels, "Walking_Upstairs")
      } else if (x[i,1] == 3) {
        labels <- c(labels, "Walking_Downstairs")
      } else if (x[i,1] == 4) {
        labels <- c(labels, "Sitting")
      } else if (x[i,1] == 5) {
        labels <- c(labels, "Standing")
      } else if (x[i,1] == 6) {
        labels <- c(labels, "Laying")
      }
    }
  merged <- cbind(labels, merged)
  features <- as.vector(features[,2])
  features <- c("Activity", "Subjects", features)
  colnames(merged) <- features
}
## Select only the Mean and Standard Deviation of each measurement
selectData <- cbind(merged[grep("Activity",features,value=T)],
                    merged[grep("Subjects",features,value=T)],
                    merged[grep("ean",features,value=T)], 
                    merged[grep("std",features,value=T)])
## Get the average of each variable for each activity and each subject
data <- NULL
averages <- NULL
for (i in 1:30) {
  data <- selectData[selectData$Subject %in% i,]
  final <- split(data, data$Activity)
  for (n in 1:6) {
    l <- as.data.frame(final[[n]])
    row <- c(l[1,2])
    for (m in 3:88) {
      row <- c(row, mean(l[,m]))
    }
    averages <- rbind(averages, row)
  }
}
averages <- as.data.frame(averages)
activities <- rep(c("Laying","Sitting","Standing","Walking",
                    "Walking_Downstairs","Walking_Upstairs"),
                  times=30)
averages <- cbind(activities, averages)
colnames(averages) <- colnames(data)
