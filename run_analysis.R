

library(dplyr)

## Creates the list with necessary info
read.table_info <- list(
  # list for filename
  file = list(
    activity_labels = "UCI HAR Dataset/activity_labels.txt",
    features = "UCI HAR Dataset/features.txt",
    subject_train = "UCI HAR Dataset/train/subject_train.txt",
    y_train = "UCI HAR Dataset/train/y_train.txt",
    X_train = "UCI HAR Dataset/train/X_train.txt",
    subject_test = "UCI HAR Dataset/test/subject_test.txt",
    y_test = "UCI HAR Dataset/test/y_test.txt",
    X_test = "UCI HAR Dataset/test/X_test.txt"
  ),
  # list for column type
  colClasses = list(
    activity_labels = c("integer", "character"),
    features = c("integer", "character"),
    subject_train = "integer",
    #y_train and y_test is only 1 column of integer
    y_train = "integer",
    X_train = rep("numeric", 561),
    subject_test = "integer",
    y_test = "integer",
    X_test = rep("numeric", 561)
  ),
  # list for no of rows
  nrows = list( 
    activity_labels = 6,
    features = 561,
    subject_train = 7352,
    y_train = 7352,
    X_train = 7352,
    subject_test = 2947,
    y_test = 2947,
    X_test = 2947
  )
)

# apply function read.table across read.table_info.
data_of_files <- with(read.table_info,
                   Map(read.table,
                       file = file, colClasses = colClasses, nrows = nrows,
                       quote = "", comment.char = "", stringsAsFactors = FALSE))
# data_of_files has 8 elements with keys.

# STEP 1
# column bind training & test data. 1st column is 'subject', then 'activity', 
#   then the rest of features
# Stack training & test data. 
sub_act_features <- with(data_of_files,
                    rbind(cbind(subject_train, y_train, X_train),
                          cbind(subject_test,  y_test,  X_test)))
# after rbind(train,test), the size is 10299 row * 563 variables


# STEP 2
#from features.txt, get index for those rows with mean() or std()
feature_index <- grep("mean\\(\\)|std\\(\\)", data_of_files$features[[2]])

# Take all rows, column = subject, activity, the rest of mean() or std()
tidy_data <- sub_act_features[ , c(1, 2, feature_index + 2)]

# STEP 3
# mapping of levels to labels is in activity_labels.txt
# activity is in column 2
tidy_data[[2]] <- factor(tidy_data[[2]],
                           levels = data_of_files$activity_labels[[1]],
                           labels = data_of_files$activity_labels[[2]])

# STEP 4
# Extract the name of features from features.txt
feature_description <- data_of_files$features[[2]][feature_index]

# Create a tidy data set with meaningful labels 
names(tidy_data) <- c("subject", "activity", feature_description)
write.csv(tidy_data[1:100,], "tidy_data.csv", row.names = FALSE)
write.table(tidy_data[1:100,], "tidy_data.txt", row.names = FALSE)

# STEP 5 
# Group by using 'subject' and 'activity'.
# Collapse those group of data using mean()
tidy_data_mean <- tidy_data %>%
  group_by(subject, activity) %>%
  summarize_all(.funs = mean) %>%
  ungroup()

write.csv(tidy_data_mean, "tidy_data_mean.csv", row.names = FALSE)
write.table(tidy_data_mean, "tidy_data_mean.txt", row.names = FALSE)