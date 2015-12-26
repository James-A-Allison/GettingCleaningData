## In the section below we download and unzip the source data

sourceURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = sourceURL, "project.zip")
unzip("project.zip")

#this is our working data directory for most of the operations
directory <- "UCI HAR Dataset"

#In the section below we get the list of features and tidy it up to make it more readable by humans
features <- read.csv(paste(directory, "features.txt", sep = "/"), header = FALSE, sep = " ")
colnames(features) <- c("ft_id", "features")

features$features <- gsub("^t", "time", features$features)
features$features <- gsub("^f", "frequency", features$features)
features$features <- gsub("Acc", "Acceleration", features$features)
features$features <- gsub("Mag", "Magnitude", features$features)
features$features <- gsub("\\()", "", features$features)
features$features <- tolower(features$features)

#these are the features we want to output
desired_features <- as.character(features[c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241,253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543),2])

#these are the exercises people perfrom
exercise <- read.csv(paste(directory, "activity_labels.txt", sep = "/"), header = FALSE, sep = " ")
colnames(exercise) <- c("ex_id", "exercise")

#these are the people completing the tests
subject_test <- read.csv(paste(directory, "test", "subject_test.txt", sep = "/"), header = F, sep = " ")
colnames(subject_test) <- c("person")

#these are the tests people completing
y_test <-read.csv(paste(directory, "test", "y_test.txt", sep = "/"), sep = " ", header = F)
colnames(y_test) <- c("ex_id")
y_test <- merge(y_test, exercise)

#this is the test data, filtered for means and standard deviations we wish to do downstream analysis on
x_test <-read.fwf(paste(directory, "test", "x_test.txt", sep = "/"), header = F, widths = rep.int((c(-2,14)), times = 561))
colnames(x_test) <- features$features
x_test <- subset(x_test, select = desired_features)

#these are the people completing training
subject_train <- read.csv(paste(directory, "train", "subject_train.txt", sep = "/"), header = F)
colnames(subject_train) <- c("person")

#this is the training people are completing
y_train <- read.csv(paste(directory, "train", "y_train.txt", sep = "/"), header = F)
colnames(y_train) <- c("ex_id")
y_train <- merge(y_train, exercise)

#this is the training data, again filtered for the measures we wish to do downstream analysis on
x_train <-read.fwf(paste(directory, "train", "x_train.txt", sep = "/"), header = F, widths = rep.int((c(-2,14)), times = 561))
colnames(x_train) <- features$features
x_train <- subset(x_train, select = desired_features)


#creating some empyty vectors to add data into
test <- c()
train <- c()
large_table <- c()

#combining all the test and training data
test <- cbind(subject_test, y_test, x_test)
train <- cbind(subject_train, y_train, x_train)
large_table <- rbind(test, train)

#in this section we loop through all the measures we wish to output, creating a mean, and building up to the tidy_data output
loop_data <- data.frame()
tidy_data <- data.frame()

for (i in 4:49) {
  loop_data <- aggregate(large_table[,i], list(large_table$person, large_table$exercise), FUN = mean)
  colnames(loop_data) <- c("person", "exercise", colnames(large_table)[i])
  
  if (i == 4) {tidy_data <- loop_data}
  else {tidy_data <- merge(tidy_data, loop_data)}
}

write.table(tidy_data, "tidy_data.txt", row.names = F)
