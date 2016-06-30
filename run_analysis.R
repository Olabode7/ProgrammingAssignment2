## MERGING TWO DATA SETS I.E TRAIN AND TEST TO FORM ONLY ONE SET
# Read the activity rows for train
activitytrain <-read.table("./train/y_train.txt", header=F)
# Read the activity rows for test
activitytest<-read.table("./test/y_test.txt", header=F)

# Add the train and test data together
activity<-rbind(activitytrain, activitytest)

# Check the rows structure
dim(activity) 
## Read the subject rows
# Read the subject rows for train
subjecttrain<-read.table("./train/subject_train.txt", header=F)

# Read the subject rows for test
subjecttest<-read.table("./test/subject_test.txt", header=F)

# Add the test and train data together
subject<-rbind(subjectrain, subjecttest)

# Check the rows structure
dim(subject)

# Create a column name
colnames(subject)<- c("subject")

## Reading of the features label
# Read the featurelabels for the columns of the train and test data
featurelabels<-read.table("features.txt", header=F)

# Check the structure
dim(featurelabels)
# Create column names this
colnames(featurelabels)<-c("featurenr", "featurename")
## READING TRAIN AND TEST DATA
# Read the x-test feature data
featurestrain<-read.table("./train/x_train.txt", header=F)

# Read the x-test feature data
featurestest<-read.table("./test/x_test.txt", header=F)

# Add the train and test data together
features<-rbind(featurestrain, featurestest)

# Check the structure
dim(features) 
## THIS CREATES THE COMBINED DATA SETS
# Create column names for the featurelabels
# Ensure that the column names are unique, otherwise we'll encounter errors later when merging
colnames(features) <- make.names(featurelabels$featurename, unique=TRUE)

# add the activity, subject feature data together
data<-cbind(activity, subject, features)

# Recheck the structure
dim(data)
## EXTRACTION OF MEASUREMENTS(MEAN AND STANDARD DEVIATION)
  # Create a new table containing only columns with subjects, activityid, mean and std
datamean-std <- select(data,matches("subject|activityid|mean|std"))

# Check the structure
dim(datamean-std)

USES OF DESCRIPTIVE ACTIVITIES NAME TO NAME THE VARIABLES
# Read the labels for the activities
activitylabels<-read.table("activity_labels.txt", header=F, sep=" ")

# Check the structure
head(activitylabels)
##   M1                 M2
## 1  1            JUMPING
## 2  2            DANCING
## 3  3            RUNNING
## 4  4            WALKING
## 5  5            FITTING
## 6  6            PLAYINY
## 7  7            WRITTING
## 8  8            DRIVING
# Create column names for activity labels
colnames(activitylabels)<- c("activityid","activity")

# Add the activity label to the dataset using a merge on activityid
data <- merge(x=datameanstd, y=activitylabels, by="activityid")

# Check that activity has been merged correctly
unique(data[,c("activity")])

# Exclude the activityid field
data <- select(data, -activityid)

# Reorder the columns so that the dataset starts with subject and activity
data<-select(data, subject, activity, 2:87)

# Check the structure
dim(data)
## LABELS DATA SETS WITH DESCRIPTIVE NAMES
# Get the column names and make them unique
colnames <-colnames(data)
colnames <- make.names(colnames, unique=TRUE)

#Cleanup the variable names by replacing characters
colnamesclean<-gsub("-", " ", colnames) #Rplace - with a space
colnamesclean<-gsub("\\.", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("\\  ", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("\\  ", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("\\  ", " ", colnamesclean) #Replace . with a space
colnamesclean<-gsub("tBody", "Body", colnamesclean) #Remove the t
colnamesclean<-gsub("tGravity", "Gravity", colnamesclean) #Remove the t
colnamesclean<-gsub("fBody", "Body", colnamesclean) #Remove the f
colnamesclean<-gsub("BodyBody", "Body", colnamesclean) #Remove double Body
colnamesclean<-gsub("^\\s+|\\s+$", "", colnamesclean) #Strip leading and trailing spaces

# Recreate the column names for the dataset
colnames(data) <- colnamesclean

# Check the structure
str(data)
CREATE TIDT DATASET FROM FROM ABOVE DATA
# Create a datafram table (Dplyr)
tidy <- tbl_df(data)

# Create unique column names, otherwise the summary will give errors
colnames(tidy) <- make.names(colnames(tidy) , unique=TRUE)

# Group the data by subject and activity
tidygroup <-group_by(tidy, subject, activity)

# Calculate the mean for all features using a Dplyr function
tidymean <- summarise_each(tidygroup, funs(mean))

# Reapply the clean column names
colnames(tidymean) <- colnamesclean

# Check the first 5 rows and 5 columns
tidymean[1:5, 1:5]
## Get the mean value
# Create tidy dataset from step 5
write.table(tidymean, file="tidy.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=TRUE)
