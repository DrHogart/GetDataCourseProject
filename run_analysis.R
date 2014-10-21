# First of all we define a couple of functions that will be used for 
# manipulatiion of the data.
# The first function allows to find all variable names contained 'mean/std'
# substrings. Also, by this function we add the names for the Subject
# and Activity columns. Output is the vector of the column numbers
# named by corresponding column names

select_var <- function(df) {
    cols <- c(1, 2)
    vars <- c('Subject', 'Activity')
    is <- nrow(df)
    for (i in 1:is) {
        if (grepl('mean', df[i,2])) {
            cols <- c(cols, i+2)
            vars <- c(vars, as.character(df[i,2]))
        }
 #       if (grepl('Mean', df[i,2])) {
 #           cols <- c(cols, i+2)
 #           vars <- c(vars, as.character(df[i,2]))
 #       }
        if (grepl('std', df[i,2])) {
            cols <- c(cols, i+2)
            vars <- c(vars, as.character(df[i,2]))
  #      }
  #      if (grepl('Std', df[i,2])) {
  #          cols <- c(cols, i+2)
  #          vars <- c(vars, as.character(df[i,2]))
  #     }
    }
    names(cols) <- vars
    return(cols)   
}

# The second function renames the activity numbers to the activity labels

activity_rn <- function(vector, labs) {
    out <- c()
    for (i in vector) {
        out <- c(out, as.character(labs[i,2]))
    }
    return(out)
}

# The last function renames the variables by expanding the abbreviations
# (according to features_info.txt file in the analysed dataset)

variables_rn <- function(names){
    out <- names
    out <- gsub('Acc', 'Accelerometer', out)
    out <- gsub('Gyro', 'Gyroscope', out)
    out <- gsub('tBody', 'timeBody', out)
    out <- gsub('tGravity', 'timeGravity', out)
    out <- gsub('fBody', 'frequencyBody', out)
    out <- gsub('Mag', 'Magnitude', out)
    out <- gsub('-mean\\(\\)(-)?','Mean',out)
    out <- gsub('-meanFreq\\(\\)(-)?','MeanFreq',out)
    out <- gsub('-std\\(\\)(-)?','Std',out)
    out <- gsub('BodyBody','Body',out)  # remove the artefact in the some names
    return(out)
}

# The analysis begin with downloading of the data arhive and extracting of
# files

file_url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download.file(url = file_url, destfile = 'UCI_HAR_Dataset.zip', method = 'wget')
unzip('UCI_HAR_Dataset.zip')

# Loading the test and train datasets as data.table objects. Since the
# separators of the column data are one or two spaces, we need to replace
# them to more uniform way before loading datasets.

library('data.table')
test <- fread(paste("sed 's/^[[:blank:]]*//;s/[[:blank:]]\\{1,\\}/,/g'", 'UCI\\ HAR\\ Dataset/test/X_test.txt'))
train <- fread(paste("sed 's/^[[:blank:]]*//;s/[[:blank:]]\\{1,\\}/,/g'", 'UCI\\ HAR\\ Dataset/train/X_train.txt'))

# Then we load the datasets with description of activity types and the
# subject codes for both test and train data and then add this information
# to the corresponding datasets.

test_lab <- read.table('UCI HAR Dataset/test/y_test.txt')
test_subj <- read.table('UCI HAR Dataset/test/subject_test.txt')
test <- cbind(test_subj, test_lab, test)
train_lab <- read.table('UCI HAR Dataset/train/y_train.txt')
train_subj <- read.table('UCI HAR Dataset/train/subject_train.txt')
train <- cbind(train_subj, train_lab, train)

# Also we need to load the description of all measured features and
# the text description of the activity labels

all_features <- read.table('UCI\ HAR\ Dataset/features.txt')
activity_labs <- read.table('UCI\ HAR\ Dataset/activity_labels.txt')

# Merging test and train data to the combined dataset and selecton of
# variables in the combined dataset that contains 'mean/Mean'
# and 'std/Std' substrings.

all <- rbind(test, train)
variables <- select_var(all_features)
all <- all[, variables]
colnames(all) <- names(variables)

# The variable names in the dataset have a lot of abbrevations, so it is
# neccessery to rename them to be much more self-descritptive

colnames(all) <- variables_rn(colnames(all))

# The activity types are coded as numbers, so it is neccessary to rename
# the number-coded labels of activities to the corresponding text 
# descriptive labels

all$Activity <- activity_rn(all$Activity, activity_labs)

# Finally, the averages of variables observations for each Subject
# and type of Activity are estimated. The result tidy dataset is saved
# in the output file

result <- aggregate(. ~ Subject + Activity, data = all, mean)
write.table(result, file = 'UCI_dataset.processed.txt', row.names = FALSE)

