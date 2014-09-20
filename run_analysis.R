# 1.  Merges the training and the test sets to create one data set.

train <- read.table("X_train.txt",header=FALSE)
test <- read.table("X_test.txt",header=FALSE)
data_complete <- rbind(train, test)                               # raw data

# 4.  Appropriately labels the data set with descriptive variable names. 

feature_list <- read.table("features.txt",header=FALSE)[,2]
names(data_complete) <- feature_list

# 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 

hits <- sort(c( grep("mean",names(data_complete)),grep("std",names(data_complete))))
data_filtered <- data_complete[,hits]

# 3.  Uses descriptive activity names to name the activities in the data set

# get activity label- numeric
train_label <- read.table("Y_train.txt",header=FALSE)
test_label <- read.table("Y_test.txt",header=FALSE)
activity_label <- c(train_label[[1]],test_label[[1]])                  

# get activity name - string
act_table <- read.table("activity_labels.txt",header=FALSE)
act_table[,2] <- as.character(act_table[,2])                           

# maping the activity label to activity name
act_lst <- c()
for (a in 1:length(activity_label)) {
  act_lst <- c(act_lst, act_table[activity_label[a],2])
}

# bind activity name into data
data_named <- cbind(activity_name=factor(act_lst), data_filtered)

# assign object ID

objID_train <- read.table("subject_train.txt",header =FALSE)
objID_test <- read.table("subject_test.txt",header=FALSE)
objID <- c(objID_train[[1]],objID_test[[1]])

data_final <- cbind(objID = factor(objID), data_named)

# clean _ and () in column names

col_name <- names(data_final)
col_name <- gsub("\\(\\)","",col_name)
col_name <- gsub("-","_",colname)
names(data_final) <- col_name

# 5. From the data set in step 4, creates a second, independent 
#  tidy data set with the average of each variable for each 
#  activity and each subject.

a <- factor(data_final$objID)
b<- factor(data_final$activity_name)
result <- aggregate(data_final[,3:ncol(data_final)],by=list(a,b),FUN=mean)
names(result) <- c("ObjID","Activity_name",names(result)[3:ncol(result)])
write.table("project_tidy_data.txt",row.names=FALSE,col.names=FALSE,quote=FALSE)



