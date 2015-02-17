run_analysis <- function() {
        
        library(dplyr)
        
        # Part 1: Merge the training en test set to one dataset
        # Read training data and combine them to one dataset
        subject.train <- read.csv("./train/subject_train.txt",
                                  sep= "\t", header= FALSE)
        widths <- rep(16,561)
        x.train <- read.fwf("./train/X_train.txt", 
                            widths = widths, buffersize = 100)
        y.train <- read.csv("./train/Y_train.txt", sep= "\t", header= FALSE)
        data.train <- data.frame(x.train, subject.train, y.train)

        
        # Read test data and combine them to one dataframe
        subject.test <- read.csv("./test/subject_test.txt", sep="\t", 
                                 header = FALSE)
        x.test <- read.fwf("./test/X_test.txt", widths = widths, 
                           buffersize = 100)
        y.test <- read.csv("./test/Y_test.txt", sep="\t", header = FALSE)
        data.test <- data.frame(x.test, subject.test, y.test)
        
        # combine the test and train data in one dataframe
        data <- rbind(data.train,data.test)

        # Part 4: set the columnnames to discriptive names. 
        # We do it here because it is more straight foreward. For example
        # now we can use the columnnames in part 3 to merge it and in part 2
        # the extract already has the correct columnnames.

        # read  features and subject coerse factors to character
        features <- read.csv("features.txt", sep= " ", header= FALSE)
        features <- sapply(features[,2], as.character)
        colnames(data) <- c(sapply(features, as.character),
                            "subject", "activitynr")
        
        # Part 2: Extracts mean and sd for each measurement
        # find columnid's mean and std measurements
        columnid <-grep("mean|std", features)
        # extract mean std measurements 
        extract <- data[,columnid]
        # and add subject, activity, test/train
        extract$subject <-data$subject
        extract$activitynr <- data$activitynr
        
        # Part 3: set descriptive activity names to name the activities
        activity.labels <- read.csv("activity_labels.txt", 
                                    sep= " ", header= FALSE)
        # set columnnames so the data can be merged on activitynr
        colnames(activity.labels) <- c("activitynr", "activity")
        merged <- merge(extract,activity.labels)
        # remove column with the activitynr 
        merged <- merged[,!(names(merged) %in% "activitynr")]

        # Part 5: creates a independent tidy data set with the average
        # of each variable for each activity and each subject
        
        grouped <- group_by(merged, subject, activity)
        tidy <- grouped %>% summarise_each(funs(mean))
        
        # return the result
        tidy
}