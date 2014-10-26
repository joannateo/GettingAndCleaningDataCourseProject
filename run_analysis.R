## Read the features list
features <- read.table('UCI HAR Dataset/features.txt', as.is = TRUE)$V2


## Step 0. Create a function for reading all the information contained in the
## data sets and merge it into one single data frame ----
readDataSet <- function(directory)
{
    ## Get the files in the directory
    files <- list.files(directory, full.names = TRUE)
    
    ## Retrieve the names of the files that contain the desired information
    X_file <- files[grep('X_[^/]+\\.txt', files)]
    y_file <- files[grep('y_[^/]+\\.txt', files)]
    subject_file <- files[grep('subject_[^/]+\\.txt', files)]
    
    ## Read the features table
    X <- read.table(X_file)
    names(X) <- features
    
    ## Read the activity labels table
    y <- read.table(y_file)
    names(y) <- 'activity'
    
    ## Read the subjects table
    subject <- read.table(subject_file)
    names(subject) <- 'subject'
    
    ## Return the binding of the three tables
    cbind(subject, y, X)
}


## Step 01. Read the training and test data sets and merge them into a single
## data frame ----
training_data <- readDataSet('UCI HAR Dataset/train/')
training_data$origin <- 'train' # Track the origin of these rows
test_data <- readDataSet('UCI HAR Dataset/test/')
test_data$origin <- 'test'
data <- rbind(training_data, test_data)
## Remove the training_data and test_data data frames to free memory
rm(training_data, test_data)


## Step 02. Extract only the measurements on the mean and the standard
## deviation for each measurement ----
selected_features <- features[grepl('mean\\(\\)', features) |
                                  grepl('std\\(\\)', features)]
selected_fields <- c('origin', 'subject', 'activity', selected_features)
## Overwrite the data variable since we will not need it again
data <- data[, selected_fields]


## Step 03. Use descriptive names to name the activities ----
activities <- read.table('UCI HAR Dataset/activity_labels.txt')
names(activities) <- c('activity', 'activity.name')
data <- merge(data, activities)
## Delete the original activity column and rename the activity.name column to
## activity
data$activity <- NULL
names(data)[ncol(data)] <- 'activity'
## Reorder the columns in data to put the activity before the features
## (the only reason to do this is personal taste)
data <- data[, selected_fields]


## Step 04. Label the columns with descriptive variable names ----
names(data) <- sub('tBodyAcc-mean\\(\\)', 'mean.body.acceleration',
                   names(data))
names(data) <- sub('tBodyAcc-std\\(\\)', 'std.body.acceleration', names(data))
names(data) <- sub('tGravityAcc-mean\\(\\)', 'mean.gravity.acceleration',
                   names(data))
names(data) <- 