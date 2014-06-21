if (!exists("merge_data")) {
  ## Read in the data if it does not exist
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
  merge_data <- t(merged)
  subjects <- t(subjects)
  merge_data <- rbind(subjects, merge_data)
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
  colnames(merge_data) <- labels
  features <- as.vector(features[,2])
  features <- c("Subjects", features)
  rownames(merge_data) <- features
}
## Select only the Mean and Standard Deviation of each measurement
selectData <- rbind(merge_data[grep("Subjects",features,value=T),],
                    merge_data[grep("ean",features,value=T),], 
                    merge_data[grep("std",features,value=T),])
