##################################################################################
#													   #
#					Getting and Cleaning Data				   #
#					       Course Project					   #
#													   #
##################################################################################

#NB : I've decided to change the order of the questions, because it seems more
#logical to me to create the tidy data set, before manipulating the data. 
#so here is "my" order : question 1, question 4, question 3, question 2, question 5

#lets go!

#------------------------------- loading the data into R ------------------------#

#Set the work directory (cf readme)
#load the test data
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt", fill = TRUE, colClasses = "factor")

#load the train data
X_train <-read.table("./train/X_train.txt")
y_train <-read.table("./train/y_train.txt")
subject_train <-read.table("./train/subject_train.txt",colClasses = "factor")

#load the features
features <- read.table("./features.txt", col.names=c("index", "variable_name" ))

#------------------------------------- Question 1 -------------------------------#
#Merge the training data and the test data to create one data set.


###Step 1 : merge training data set

#to check on the dimension of the datas
dim(X_test)
dim(y_test)
dim(subject_test)
#according to the above results, we need to "column bind"
test_data<-cbind(subject_test,X_test,y_test)

###Step 2 : merge the test data set

#to check on the dimension of the datas
dim(X_train)
dim(y_train)
dim(subject_train)
#according to the above results, we need to "column bind"
train_data<-cbind(subject_train,X_train,y_train)

###Step 3 : merge the training and test data 

#to check on the dimension of the datas
dim(test_data)
dim(train_data)
#according to the above results we need to "row bind"


###Here is the merged data set
data<-rbind(test_data, train_data)

#------------------------------------- Question 4 -------------------------------#
###Appropriatly label the data set with descriptive variable names.
vector_labels<-as.character(features$variable_name)
#adding the value of the 2 new columns Subject_ID and activities to the features
vector_labels<-append("Subject_ID", names)
vector_labels<-append(vector_labels, "activities")

#properly label the data set
colnames(data)<-vector_labels

#------------------------------------- Question 3 -------------------------------#
###Use descriptive activity names to name the activities in the data set
#1 WALKING
data$activities[data$activities==1]<-"WALKING"
#2 WALKING_UPSTAIRS
data$activities[data$activities==2]<-"WALKING_UPSTAIRS"
#3 WALKING_DOWNSTAIRS
data$activities[data$activities==3]<-"WALKING_DOWNSTAIRS"
#4 SITTING
data$activities[data$activities==4]<-"SITTING"
#5 STANDING
data$activities[data$activities==5]<-"STANDING"
#6 LAYING
data$activities[data$activities==6]<-"LAYING"

#------------------------------------- Question 2 -------------------------------#
###Extract only the measurements on the mean and the standars deviation for each 
###measurement.

#calculate the mean for each measurements
data_mean<-apply(data[,c(-1,-563)],2,mean)
#calculate the standard deviation measurements
data_sd<-apply(data[,c(-1,-563)], 2, sd)

#create the tidy data set with the results
data_mean_sd<-rbind(data_mean, data_sd)
#display the result
data_mean_sd



#------------------------------------- Question 5 -------------------------------#
###Create a second, independent tidy data set with the average of each variable for 
###each activity and each subject.

test<-aggregate(data[,c(-1,-563)], by=list(data$Subject_ID, data$activities), FUN=mean, na.rm=TRUE)
#rename the 2 first column
colnames(test)[1]<-"Subject_ID"
colnames(test)[2]<-"activities"

###write the data in a file for submission on coursera.
write.table(test, "./tidy_data_answer_Q5.txt")