# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the act in the data set
# 4. Appropriately labels the data set with descriptive activity names.
# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

#Let's start!

# 1 Merging
a <- read.table("train/X_train.txt")
b <- read.table("test/X_test.txt")
X <- rbind(a, b)

a <- read.table("train/subject_train.txt")
b <- read.table("test/subject_test.txt")
S <- rbind(a, b)

a <- read.table("train/y_train.txt")
b <- read.table("test/y_test.txt")
Y <- rbind(a, b)

# 2 extracting std and means with help of regular expressions

features <- read.table("features.txt")
indices_of_right_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2]) # search for matches
X <- X[, indices_of_right_features]
names(X) <- features[indices_of_right_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X)) # Replace all occurences of a pattern 
names(X) <- tolower(names(X)) # from upper  to lower case

# 3 name the act in data

act <- read.table("activity_labels.txt")
act[, 2] = gsub("_", "", tolower(as.character(act[, 2]))) #replacing
Y[,1] = act[Y[,1], 2]
names(Y) <- "activity"

# 4.labeling the data with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "clean_data.txt")

# 5. Creating tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(act[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = act[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==act[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "tidy_data.txt") # Ta-Da

