# Run_Analysis


#THESE ARE THE NEEDED LIBRARIES TO RUN THE CODE. 
library(stringi)
library(stringr)
library(dplyr)


#THESE ARE THE FILE NAMES INSIDE THE FOLDER TO REFER TO WHEN CALLING THEM FOR READING.
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

#THIS IS A COMMENT TO SET THE DIR TO WHERE THE DATASET FOLDER IS CONTAINED
# setwd("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")


# Reads in all of the data 
x_train <- read.table('train/X_train.txt')
y_train <- read.table('train/y_train.txt')
x_test <- read.table('test/X_test.txt')
y_test <- read.table('test/y_test.txt')
subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
features <- read.table("features.txt")


# finds all of the names with either mean or std and stores them into one list of index values
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


# binds the columns that have the same dimensions 
test <- cbind(subject_test, y_test1, desired_x_test)
train <- cbind(subject_train, y_train1, desired_x_train)
names(test)[2] <- "activity_labels"
names(train)[2] <- "activity_labels"

# combines the 2 data sets using rbind and rename it
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

# creates a second data set with the averages of each variable for each test subject and activity 
names(xy) <- gsub("-", "_", names(xy))
nxy <- names(xy) <- gsub("\\(\\)", "", names(xy))

average_value_subandact <- aggregate(xy[nxy[3:length(nxy)]], xy[nxy[c(1,2)]], mean)



write.table(average_value_subandact, "averageval.txt", sep = " ", row.names = F)



--------------------------------------------------------------------------------------------------------------------------------
--------------------CODE BOOK---------------------------------------------------------------------------------------------------
In the following table V1, V2, V3, and V4 all corespond to each other.
Column V1 contains the index values for the columns in average_value_subandact data set.
Column V2 contains the column names that are in the average_value_subandact tidy data set.  
Column V3 contains the index values that that are the original column names of the x_test and x_train tables. 
Column V4 contains the original column names that were listed in the "features.txt" file that also correspond with the index values in V3


V1       V2                       V3       V4
3 	tBodyAcc_mean_X              	1  	tBodyAcc-mean()-X              
4 	tBodyAcc_mean_Y              	2  	tBodyAcc-mean()-Y              
5 	tBodyAcc_mean_Z              	3  	tBodyAcc-mean()-Z              
6 	tBodyAcc_std_X               	4  	tBodyAcc-std()-X               
7 	tBodyAcc_std_Y               	5  	tBodyAcc-std()-Y               
8 	tBodyAcc_std_Z               	6  	tBodyAcc-std()-Z               
9 	tGravityAcc_mean_X           	41 	tGravityAcc-mean()-X           
10	tGravityAcc_mean_Y           	42 	tGravityAcc-mean()-Y           
11	tGravityAcc_mean_Z           	43 	tGravityAcc-mean()-Z           
12	tGravityAcc_std_X            	44 	tGravityAcc-std()-X            
13	tGravityAcc_std_Y            	45 	tGravityAcc-std()-Y            
14	tGravityAcc_std_Z            	46 	tGravityAcc-std()-Z            
15	tBodyAccJerk_mean_X          	81 	tBodyAccJerk-mean()-X          
16	tBodyAccJerk_mean_Y          	82 	tBodyAccJerk-mean()-Y          
17	tBodyAccJerk_mean_Z          	83 	tBodyAccJerk-mean()-Z          
18	tBodyAccJerk_std_X           	84 	tBodyAccJerk-std()-X           
19	tBodyAccJerk_std_Y           	85 	tBodyAccJerk-std()-Y           
20	tBodyAccJerk_std_Z           	86 	tBodyAccJerk-std()-Z           
21	tBodyGyro_mean_X             	121	tBodyGyro-mean()-X             
22	tBodyGyro_mean_Y             	122	tBodyGyro-mean()-Y             
23	tBodyGyro_mean_Z             	123	tBodyGyro-mean()-Z             
24	tBodyGyro_std_X              	124	tBodyGyro-std()-X              
25	tBodyGyro_std_Y              	125	tBodyGyro-std()-Y              
26	tBodyGyro_std_Z              	126	tBodyGyro-std()-Z              
27	tBodyGyroJerk_mean_X         	161	tBodyGyroJerk-mean()-X         
28	tBodyGyroJerk_mean_Y         	162	tBodyGyroJerk-mean()-Y         
29	tBodyGyroJerk_mean_Z         	163	tBodyGyroJerk-mean()-Z         
30	tBodyGyroJerk_std_X          	164	tBodyGyroJerk-std()-X          
31	tBodyGyroJerk_std_Y          	165	tBodyGyroJerk-std()-Y          
32	tBodyGyroJerk_std_Z          	166	tBodyGyroJerk-std()-Z          
33	tBodyAccMag_mean             	201	tBodyAccMag-mean()             
34	tBodyAccMag_std              	202	tBodyAccMag-std()              
35	tGravityAccMag_mean          	214	tGravityAccMag-mean()          
36	tGravityAccMag_std           	215	tGravityAccMag-std()           
37	tBodyAccJerkMag_mean         	227	tBodyAccJerkMag-mean()         
38	tBodyAccJerkMag_std          	228	tBodyAccJerkMag-std()          
39	tBodyGyroMag_mean            	240	tBodyGyroMag-mean()            
40	tBodyGyroMag_std             	241	tBodyGyroMag-std()             
41	tBodyGyroJerkMag_mean        	253	tBodyGyroJerkMag-mean()        
42	tBodyGyroJerkMag_std         	254	tBodyGyroJerkMag-std()         
43	fBodyAcc_mean_X              	266	fBodyAcc-mean()-X              
44	fBodyAcc_mean_Y              	267	fBodyAcc-mean()-Y              
45	fBodyAcc_mean_Z              	268	fBodyAcc-mean()-Z              
46	fBodyAcc_std_X               	269	fBodyAcc-std()-X               
47	fBodyAcc_std_Y               	270	fBodyAcc-std()-Y               
48	fBodyAcc_std_Z               	271	fBodyAcc-std()-Z               
49	fBodyAcc_meanFreq_X          	294	fBodyAcc-meanFreq()-X          
50	fBodyAcc_meanFreq_Y          	295	fBodyAcc-meanFreq()-Y          
51	fBodyAcc_meanFreq_Z          	296	fBodyAcc-meanFreq()-Z          
52	fBodyAccJerk_mean_X          	345	fBodyAccJerk-mean()-X          
53	fBodyAccJerk_mean_Y          	346	fBodyAccJerk-mean()-Y          
54	fBodyAccJerk_mean_Z          	347	fBodyAccJerk-mean()-Z          
55	fBodyAccJerk_std_X           	348	fBodyAccJerk-std()-X           
56	fBodyAccJerk_std_Y           	349	fBodyAccJerk-std()-Y           
57	fBodyAccJerk_std_Z           	350	fBodyAccJerk-std()-Z           
58	fBodyAccJerk_meanFreq_X      	373	fBodyAccJerk-meanFreq()-X      
59	fBodyAccJerk_meanFreq_Y      	374	fBodyAccJerk-meanFreq()-Y      
60	fBodyAccJerk_meanFreq_Z      	375	fBodyAccJerk-meanFreq()-Z      
61	fBodyGyro_mean_X             	424	fBodyGyro-mean()-X             
62	fBodyGyro_mean_Y             	425	fBodyGyro-mean()-Y             
63	fBodyGyro_mean_Z             	426	fBodyGyro-mean()-Z             
64	fBodyGyro_std_X              	427	fBodyGyro-std()-X              
65	fBodyGyro_std_Y              	428	fBodyGyro-std()-Y              
66	fBodyGyro_std_Z              	429	fBodyGyro-std()-Z              
67	fBodyGyro_meanFreq_X         	452	fBodyGyro-meanFreq()-X         
68	fBodyGyro_meanFreq_Y         	453	fBodyGyro-meanFreq()-Y         
69	fBodyGyro_meanFreq_Z         	454	fBodyGyro-meanFreq()-Z         
70	fBodyAccMag_mean             	503	fBodyAccMag-mean()             
71	fBodyAccMag_std              	504	fBodyAccMag-std()              
72	fBodyAccMag_meanFreq         	513	fBodyAccMag-meanFreq()         
73	fBodyBodyAccJerkMag_mean     	516	fBodyBodyAccJerkMag-mean()     
74	fBodyBodyAccJerkMag_std      	517	fBodyBodyAccJerkMag-std()      
75	fBodyBodyAccJerkMag_meanFreq 	526	fBodyBodyAccJerkMag-meanFreq() 
76	fBodyBodyGyroMag_mean        	529	fBodyBodyGyroMag-mean()        
77	fBodyBodyGyroMag_std         	530	fBodyBodyGyroMag-std()         
78	fBodyBodyGyroMag_meanFreq    	539	fBodyBodyGyroMag-meanFreq()    
79	fBodyBodyGyroJerkMag_mean    	542	fBodyBodyGyroJerkMag-mean()    
80	fBodyBodyGyroJerkMag_std     	543	fBodyBodyGyroJerkMag-std()     
81	fBodyBodyGyroJerkMag_meanFreq	552	fBodyBodyGyroJerkMag-meanFreq()

*V1 starts at 3 because the first two collumns were not from the x_test or x_train 
Column 1 in average_value_subandact:
test_subject - shows the id of the different test subjects.
Column 2 in average_value_subandact:
activity_labels - each activity had a certain number assigned to it. The number was switched with the activity names.
each number assignment:
1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING
