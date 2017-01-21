run_analysis <- function (directory, outputfile) {

    ## This function assumes the directory passed in will be used as the working directory,
    ## and is the parent of the top level directory created by the unzip process from the data set archive
    ## The optional passed in outputfile will be used for the output filename, 
    ## set to "calc_means" if not provided, and placed in the working directory

    ## load libraries
    
    library(reshape2)
    
    ## set the working directory
    
    try( if (!dir.exists(paths = directory)) stop ("Directory not found"))
    setwd(directory)

    ## set the output filename
    
    ifelse ( missing(outputfile), outfile <- "calc_means", outfile <- outputfile ) 
    
    ## subject_xxx is the person performing the activity
    ## y_xxx is the activity ID for the observation row
    ## X_xxx is the set of observations
    
    ## create feature names vector

    feature_names <- rbind("subject", "activity_id", 
                           read.table("./UCI HAR Dataset/features.txt", 
                                      stringsAsFactors = FALSE))
    feature_names <- feature_names[,2]

    ## read in the data
    

    train_set <- cbind(read.table("./UCI HAR Dataset/train/subject_train.txt"),
                       read.table("./UCI HAR Dataset/train/y_train.txt"),
                       read.table("./UCI HAR Dataset/train/X_train.txt"))

    test_set <- cbind(read.table("./UCI HAR Dataset/test/subject_test.txt"),
                       read.table("./UCI HAR Dataset/test/y_test.txt"),
                       read.table("./UCI HAR Dataset/test/X_test.txt"))

    all_obs <- rbind(train_set, test_set)

    ## apply feature names
    
    names(all_obs) <- feature_names
    
    ## subset the mean and standard deviation columns from the data set
    
    mean_std <- subset(all_obs, select = grepl("subject|activity|mean|std", 
                                               names(all_obs), 
                                               ignore.case = TRUE))

    ## replace activity IDs with descriptive labels
    
    activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                                  col.names = c("activity_id", "activity"),
                                  stringsAsFactors = FALSE)

    mean_std <- merge(activity_labels, mean_std, by = "activity_id")[,-1]

    ## create summary dataset of feature averages for each activity + subject combination
    
    mean_std_summ <- dcast(
                        melt(mean_std, id.vars = c("activity", "subject")),
                            activity + subject ~ variable, fun.aggregate = mean)
    
    write.table(mean_std_summ, file = outfile)

}