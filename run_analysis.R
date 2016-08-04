##################
# run_analysis.R # 
##################

rm(list = ls(all = TRUE))

# install.packages("plyr")
library(plyr, lib.loc = "C:/Users/TRAICUE/Documents/R/win-library/3.3")

# Read & load data from files
UCI_HAR_Dataset <- "C:/Coursera-R/Course_3/Final Assignment"
feature_file <- paste(UCI_HAR_Dataset, "/features.txt", sep = "")
activity_labels_file <- paste(UCI_HAR_Dataset, "/activity_labels.txt", sep = "")
x_train_file <- paste(UCI_HAR_Dataset, "/train/X_train.txt", sep = "")
y_train_file <- paste(UCI_HAR_Dataset, "/train/y_train.txt", sep = "")
subject_train_file <- paste(UCI_HAR_Dataset, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(UCI_HAR_Dataset, "/test/X_test.txt", sep = "")
y_test_file  <- paste(UCI_HAR_Dataset, "/test/y_test.txt", sep = "")
subject_test_file <- paste(UCI_HAR_Dataset, "/test/subject_test.txt", sep = "")

features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

# Step 1. Merges the training and the test sets to create one data set.
#######################################################################

training_data <- cbind(cbind(x_train, subject_train), y_train)
test_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_data, test_data)

sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels

# Step 2. Extracts only the measurements on the mean and standard deviation for each measurements.
##################################################################################################

sensor_data_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

# Step 3. Uses descriptive activity names to name the activities in the data set.
#################################################################################

sensor_data_mean_std <- join(sensor_data_mean_std, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean_std <- sensor_data_mean_std[,-1]

# Step 4. Appropriately labels the data set with descriptive names.
###################################################################

# Define descriptive column names
names(sensor_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)
names(sensor_data_mean_std) <- gsub('mean',"Mean",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('std',"StandardDeviation",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_std))

# Step 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
########################################################################################################################################################

tidy_dataset = ddply(sensor_data_mean_std, c("Subject","Activity"), numcolwise(mean))
write.table(tidy_dataset, file = "tidy_data.txt")