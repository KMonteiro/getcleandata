library(data.table)
library(dplyr)


#Load the column names into features data frame
features <- read.table("./features.txt")

#Load the activity labels into activity_labels data frame
activity_labels <- read.table("./activity_labels.txt")

#Load the test data

Xtest <- read.table("./test/X_test.txt")
Ytest <- read.table("./test/Y_test.txt")
subjecttest <-  read.table("./test/subject_test.txt")

#Load the train data

Xtrain <- read.table("./train/X_train.txt")
Ytrain <- read.table("./train/Y_train.txt")
subjecttrain <-  read.table("./train/subject_train.txt")

#Combine the data

Xcombined <- rbind(Xtest,Xtrain)
Ycombined <- rbind(Ytest,Ytrain)
subjectcombined <- rbind(subjecttest,subjecttrain)

#Add the column names from the features file
colnames(Xcombined) <- t(features[2])
colnames(Ycombined) <- "Activity"
colnames(subjectcombined) <- "Subject"

#Combined the whole shebang into one bing file

completeData <- cbind(Xcombined,Ycombined,subjectcombined)

# Find the Mean and STD columns by finding characters in the labels
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# Put the Mean and STD columns into storage
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

# Using the stored column set, pull the needed columns out of the completed data
extractedData <- completeData[,requiredColumns]

# Apply activity names -- to match the numbers in the original with the text labels, 
# first change the data to character, then loop through to substitue

extractedData$Activity <- as.character(extractedData$Activity)

for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activity_labels[i,2])
}

#Then return the vairable to factor

extractedData$Activity <- as.factor(extractedData$Activity)


# Create a new dataset with the average of each variable for each activity and subject

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
