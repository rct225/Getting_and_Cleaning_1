## Getting and Cleaning Project 1
Rob Tuck

### Description
Information about variables, data and transforms done for course project

### Source Data
A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Data processing

1. Merge the training and test sets.  Read the following files into data tables:
- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt

Add column names to ease future manipulations

2. Extract the mean and standard deviation for each measurement.

Grep for the columns with "std" and "mean" in the column names and store data.
Additionally, grep for the activity and subject id columns and store.

Bind the stored columns into a new data set.  Merge in the activity labels to make activities more human readable.

3. Add desciptive names or clean up names for case

- std to StdDev
- mean to Mean
- (t) to time
- (f) to freq
- [Gg]ravity to Gravity
- [Bb]ody to Body
- [Gg]yro to Gyro
- AccMag to AccMagnitude
- [Bb]odyAccjerkmag to BodyAccJerkMagnitude
- JerkMag to JerkMagnitude
- GyroMag to GyroMagnitude

4. Create another tidy data set with the average of each measurement for each activity and subject
