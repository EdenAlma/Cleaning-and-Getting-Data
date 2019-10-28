README.MD
Eden Almakias
2019-10-27

run_analysis.R is a single script which does all the work required in this assignment.
In order for it to work the working directory in which it is executed must contain the 
unzipped data from the "getdata_projectfiles_UCI HAR Dataset.zip" file.

Additionaly, the dplyr package must be installed to make use of the group_by and summarise_all functions.

The run_analysis.R script does the following transformations on the raw data:

step 1: read data into dataframes from the raw file listed below:
/test/X_test.txt
/train/X_train.txt   
/test/y_test.txt
/train/y_train.txt
/test/subject_test.txt
/train/subject_train.txt
/activity_labels.txt
/features.txt

*The two "X_" files are processed differently since they contain inconsistent spacing between their 
columns meaking it necessary to extract an entire line of features, split it by a single space and 
then remove any empty values from the line (all done by the processXFiles function)

step 2:
The train and test activity vectors are processed using the assignActivtyLabel function (and sapply)
to convert the two integer vector to string vectors containing the labels for the activities. 
This is done by using the activity_labels.txt data/data frame. 
Additionaly, these columns are named "activity". The subject vector column are named "subject".
The train and test vectors are combined (rbind) for subject and activity vectors

step 3:
Using the data contained in /features.txt, columns which contain "std()" or "mean()"  
are extracted (using grepl) from the merged dataframe (x_test + x_train) called totalData.
The columns are then assigned the appropriate labels.

step 4:
Subject and activity columns are bound to the main datafram using the rbind function

step 5:
The dplyr package function group_by is used to group the data in the main dataframe using 
the varaibles subject and activity, this grouped tbl is then processed using the summarise_all function,
applying the mean function to the groups and producing the final product which is the sumData dataframe

step 6: 
The sumData dataframe is written into a file named summarizedData.txt inside the workind directory


 