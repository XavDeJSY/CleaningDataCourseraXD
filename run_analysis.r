# read the test dataset into r 'test.txt'
testdata <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header = FALSE)

# let's see what the variable names look like
head(names(testdata), 10)

# read the train dataset 'train.txt'.
traindata <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header = FALSE)

# let's see what the variable names look like
head(names(traindata), 10)

# Are the variable names the same and in the same order in both the test and training datasets?
identical(names(traindata), names(testdata))
# Ok, the variables are matching

# The read me provided by the researchers tells us to use the file 'features.txt' to get
# a meaningful name for each variable.
# Let's drag this data into r
feat <- read.table("./data/UCI HAR Dataset/features.txt", header = FALSE)
# the Global environment pane tells us that it has the same number of variables as traindata.
# Let's have a look at "feat"
feat[1:5, 1:2]
# The descriptive variable names are in the second column of feat.
# Let's check whether each variable name is unique
as.data.frame(table(feat[,2]))
# Some of them appear more than once we will have to keep that in mind
# For example the variable fBodyAcc-bandsEnergy()-1,16 appears 3 times.

# We are now going to rename all variables in traindata and testdata with
# their descriptive names.
names(traindata) <- feat[,2]
names(testdata) <- feat[,2]
# Let's have a look at the new names
head(names(traindata), 10)
head(names(testdata), 10)


# Let's have a look at traindata
traindata[c(1:5), c(1:5)]
# The subject label and the activity labels are missing from both dataframes.
# However the readme tells us that each row corresponds to all measurments made for 1 subject
# for 1 activity.

# For the test data set the subject number is stored in 'subject_test.txt'.
testsub <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
# For the train data set the subject number is stored in 'subject_train.txt'.
trainsub <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# For the test data set the activity number is stored in 'y_test.txt'.
testactlabels <- read.table("./data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
# For the train data set the activity number is stored in 'y_train.txt'.
trainactlabels <- read.table("./data/UCI HAR Dataset/train/y_train.txt", header = FALSE)

# The environment pane tells us that there are the same number of observations in all
# files relating to test (testdata, testsub and testactlabels).
# The same for the train data, assuming that for each sets of observations
# none of these dataset have been sorted in a different way, we will be able to add the
# subject and activity info to our dataframes.

# The readme tells us that there are 30 subjects, let's see if that makes sense and 
# tie up with the subject and activity dataframes.
range(trainsub)
# The train table contains observations for all subjects

range(testsub)
# The test table only contains observations for some of the subjects.

# The readme tells us that there are 6 different activities.
# Let's have a look at how many activities have been measured in test and train:  
range(trainactlabels)
range(testactlabels)
# both dataframes contain observations for all activities.

# Now it would be nice to have the activity description instead of the activity number
# in both dataframes.
# The code below brings the the file that contains this info.
act <- read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE)
# let's have a look at act
act

# The code below will add the activity name alongside the activity number for each
# of the activity dataframes.
trainactlabels <- merge(trainactlabels, act, by.x = "V1", by.y = "V1", all=TRUE)
testactlabels <- merge(testactlabels, act, by.x = "V1", by.y = "V1", all=TRUE)

# We can also rename the variable V2 as "activity" in the above files
names(trainactlabels)[2] <- "Activity"
names(testactlabels)[2] <- "Activity"

names(trainactlabels)

# We can now use these newly created files to add the activity name and the subject number to 
# both the test and train dataframes.
# n.b: We need to select only the second column in the "activity labels" dataframes

traindata2 <- cbind(trainactlabels$Activity, trainsub, traindata)
testdata2 <- cbind(testactlabels$Activity, testsub, testdata)

# Let's have a look at a sample of both dataframes.

traindata2[c(1:5), c(1:5)]
testdata2[c(1:5), c(1:5)]

# we can rename the first two variables with more meaningful names

names(traindata2)[1] <- "activity"
names(testdata2)[1] <- "activity"
names(traindata2)[2] <- "subject"
names(testdata2)[2] <- "subject"

traindata2[c(1:5), c(1:5)]
testdata2[c(1:5), c(1:5)]

# Now that both datasets test and train contain the Activity and Subject for each observations
# and the same "human friendly" variables we can merge these two datasets together.
# 
combo <- rbind(traindata2, testdata2)

# The environment pane tells us that the merging seems to have worked properly. 
# lets have a look at combo
combo[1:5, 1:5]

# We will now create a smaller combo dataset of only
# the measurements on the mean and standard deviation 

# First we need to pinpoint the variables which relate to mean or Std dev measurment.
# The function grep() takes a pattern as first argument and a character vetor as second argument.
# It returns the index or label of each element of the character vector 
# where the pattern is found   

# We apply the grep function to the character vector names(combo)
# to select the variables where the exact character string mean() and std() can be found.
# the metacharacter () signify "any character" and therefore needs to be
# escaped by \\ in order to only select variable names where the exact character string "mean()" can be
# found, as opposed to all character strings containing the word "mean" only.

library(tidyr)
library(dplyr)

varlist <- grep("mean\\()|std\\()", names(combo), value = TRUE)
varlist

# the code below produces a charachter verctor of all the variables that we will need:
# The first 2 variables because they contain the activity and individual and those
# in varlist
finalvarlist <- c(names(combo)[c(1,2)], varlist)

# now we need to create a subset of combo for the variables in finalvarlist
combo2 <- combo[, finalvarlist]

head(names(combo2))

# The variable names in that new dataframe contain metacharacters that will "break" the
# execution of functions on these variables.
# The code below strips those variables names of the metacharacters

names(combo2) <- gsub("-", "", names(combo2))
names(combo2) <- gsub("\\()", "", names(combo2))

# Let's have a look at the new names
head(names(combo2), 10)

#	From the data set in step 4, creates a second,
# independent tidy data set with the average of each variable for each activity and each subject.

# We are now going to group the data activity and subject in preparation for summarising it. 
ind <- group_by(combo2, activity, subject)

ind[1:5, 1:5]

# the code below creates the required tidy data set where each variable is averaged.
tidydata <- summarise_each(ind ,funs(mean))

head(tidydata)

# we now need to create a text file from the above table
write.table(tidydata, file = "tidydata.txt", sep="\t")
write.csv(tidydata, file = "tidydata.csv")

