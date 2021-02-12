library(stringi)
library(stringr)
library(dplyr)

# - 'README.txt'
# 
# - 'features_info.txt': Shows information about the variables used on the feature vector.
# 
# - 'features.txt': List of all features.
# 
# - 'activity_labels.txt': Links the class labels with their activity name.
# 
# - 'train/X_train.txt': Training set.
# 
# - 'train/y_train.txt': Training labels.
# 
# - 'test/X_test.txt': Test set.
# 
# - 'test/y_test.txt': Test labels.
#
# - "train/subject_train.txt"
#
# - "test/subject_test.txt"

#> setwd("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")

# Read in all of the data
x_train <- read.table('train/X_train.txt')
y_train <- read.table('train/y_train.txt')
x_test <- read.table('test/X_test.txt')
y_test <- read.table('test/y_test.txt')
subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
features <- read.table("features.txt")


# finds all of the names with either mean or std and concatenates them into one list of index values
mind <- grep("mean", features[,2] )
sind <- grep("std", features[,2] )
ind <- c(mind, sind)

# takes the index values of the desired columns for mean and std and stores the name in a list
column_name <- features[ind,2]

#extracts the desired columns from the x train and test sets
desired_x_train <- x_train[,ind]
desired_x_test <- x_test[,ind]

# stores the destriptive column names that were stored in list "column_name"
names(desired_x_test) <- column_name
names(desired_x_train) <- column_name

#stores the activity labels in a list
activity_labels <- c('WALKING', 'WALKING_UPSTAIRS', 'WALKING_DOWNSTAIRS', 'SITTING', 'STANDING', 'LAYING')

# takes the activity labels and changes the coresponding values in the y values 
trainlist <- c()
testlist <- c()
for (i in seq_along(y_train[,1])){
    trainlist <- c(trainlist, str_replace(y_train[i,1],  as.character(y_train[i,1]), activity_labels[y_train[i,1]] ) )
}
for ( i in length(y_test)){
   testlist <- c(testlist, str_replace(y_test[i,1],  as.character(y_test[i,1]), activity_labels[y_test[i,1]] ))
}

y_train1 <- as.data.frame(trainlist)
y_test1 <- as.data.frame(testlist)


# bind the columns that have the same dimensions 
test <- cbind(subject_test, y_test1, desired_x_test)
train <- cbind(subject_train, y_train1, desired_x_train)
names(test)[2] <- "activity_labels"
names(train)[2] <- "activity_labels"

# combine the 2 data sets using rbind and rename it
xy <- rbind(test, train)
names(xy)[1] <- "test_subject"
#counts how many na values are present and prints out the number of na values
count <- 0
for (i in colnames(xy)){
      if (any(is.na(xy[[i]]))){
            count <- count + 1
      }
      
}
print(paste("There are ", count, "na values in this data set"))

# creates a second data set with the averages of each variable for each test ubject and activity
names(xy) <- gsub("-", "_", names(xy))
nxy <- names(xy) <- gsub("\\(\\)", "", names(xy))

average_value_subandact <- aggregate(xy[nxy[3:length(nxy)]], xy[nxy[c(1,2)]], mean)



write.table(average_value_subandact, "averageval.txt", sep = " ", row.names = F)
