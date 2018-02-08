# This code will run the analysis and saves it under the file "tiny.txt"

# Reads the base sets (files with begining by X) in an optimal way
read_BaseSet <- function(filePath, filteredFeatures, features) {
        cols_widths <- rep(-16, length(features))
        cols_widths[filteredFeatures] <- 16
        rawSet <- read.fwf(
                file=filePath,
                widths=cols_widths,
                col.names=features[filteredFeatures])
}

# Reads an additional file (other than the base sets). This will be used for subjects and labels.
read_AdditionalFile <- function(dataDirectory, filePath) {
        filePathTest <- paste(dataDirectory, "/test/", filePath, "_test.txt", sep="")
        filePathTrain <- paste(dataDirectory, "/train/", filePath, "_train.txt", sep="")
        data <- c(read.table(filePathTest)[,"V1"], read.table(filePathTrain)[,"V1"])
        data
}

# Correct a feature name - makes it nicer for dataframe columns (removes parentheses)
# because otherwise they are transformed to dots.

correctFeatureName <- function(featureName) {
        featureName <- gsub("\\(", "", featureName)
        featureName <- gsub("\\)", "", featureName)
        featureName
}

# Read sets and returns a complete sets
# * dataDirectory: directory of data
readSets <- function(dataDirectory) {
        # Adding main data files (X_train and X_test)
        featuresFilePath <- paste(dataDirectory, "/features.txt", sep="")
        features <- read.table(featuresFilePath)[,"V2"]
        filteredFeatures <- sort(union(grep("mean\\(\\)", features), grep("std\\(\\)", features)))
        features <- correctFeatureName(features)
        set <- read_BaseSet(paste(dataDirectory, "/test/X_test.txt", sep=""), filteredFeatures, features)
        set <- rbind(set, read_BaseSet(paste(dataDirectory, "/train/X_train.txt", sep=""), filteredFeatures, features))
        
        # Adding subjects
        set$subject <- read_AdditionalFile("UCI HAR Dataset", "subject")
        
        # Adding activities
        activitiesFilePath <- paste(dataDirectory, "/activity_labels.txt", sep="")
        activities <- read.table(activitiesFilePath)[,"V2"]
        set$activity <- activities[read_AdditionalFile("UCI HAR Dataset", "y")]
        
        set
}

# From sets, creates the tidy dataset (a summary)
createSummaryDataset <- function(dataDirectory) {
        sets <- readSets(dataDirectory)
        sets_x <- sets[,seq(1, length(names(sets)) - 2)]
        summary_by <- by(sets_x,paste(sets$subject, sets$activity, sep="_"), FUN=colMeans)
        summary <- do.call(rbind, summary_by)
        summary
}

dataDirectory <- "UCI HAR Dataset"
if (!file.exists(dataDirectory)) {
        url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        tmp_file <- "./temp.zip"
        download.file(url,tmp_file, method="curl")
        unzip(tmp_file, exdir="./")
        unlink(tmp_file)
}

summary <- createSummaryDataset(dataDirectory)
write.table(summary, "tidy.txt")
