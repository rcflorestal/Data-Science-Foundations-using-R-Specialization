## Week 4 Peer graded Assignment
This script is part of the Getting and Cleaning Data Module of the Data Science Specialization, offered by Johns Hopkins University through  Coursera platform. 

### About the data frame
This data frame was cleaning as tidy from the originals data sets which used to creates the [_Human 
Activity Recognition Using Smartphones Data Set_](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones), Version 1.0.
The present data frame was cleaning from the data sets named: subject_test, X_test, 
y_test, subject_train, X_train and y_train. All data sets are in .txt file 
format.
According to the authors[1] of the original data set, the experiment have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist.

### [run_analysis.R](https://github.com/rcflorestal/Data-Science-Specialization/blob/main/Getting-and-Cleaning-Data/Week-4-Peer-graded_Assignment/run_analysis.R) performs the data preparation to:
    1. Merges the training and the test sets to create one data set.
    2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    3. Uses descriptive activity names to name the activities in the data set
    4. Appropriately labels the data set with descriptive variable names. 
    5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
### Tidy Data Set
[Click here to see the Tidy Data Set](https://github.com/rcflorestal/Data-Science-Specialization/blob/main/Getting-and-Cleaning-Data/Week-4-Peer-graded_Assignment/tidyDF.csv)
 #### Source   
    [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. 
        A Public Domain Dataset for Human Activity Recognition Using Smartphones. 21th 
        European Symposium on Artificial Neural Networks, Computational Intelligence and 
        Machine Learning, ESANN 2013. Bruges, Belgium 24-26 April 2013. 
