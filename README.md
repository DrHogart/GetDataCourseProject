Readme for the Course Project
====================
### by Sergei Ryazansky


In this ReadMe file I would like to describe my inplementation of the Course Project for *Getting and Cleaning Data* class of the Coursera.

The aim of this Course Project is to convert messy and separated to different files datasets into one tidy dataset which can be appropriate for the future analysis. Specifically speaking, the input raw datasets are the results of measurments of the six types of activities (such as 'walking', 'laying' etc) for 30 persons performed in different time points by the Samsung Galaxy S II smartphone. All information concearning these data are separated into *test* and *train* datasets. The output file should be the single dataset that contain the estimation of time-averages for the selected set of the variables (contained mean and standart deviation of measurments) for the every each subject and type of acitivity.

To implement the Course Project, the R script was written (**run_analysis.R**). This script download the data arhive, extract datasets, merge the *test* and *train* data, select all neccessary variables and aggregate the data to estimate the averages for each subject and each type of activity. For both of *test* and *train* parts of data arhive the measurments of variables for each observation, the list of variables, the list of subjects, and the list of activity types are separated into four different files (totaly, 8 data files). So, the script collects all these data individually and then combine them to the single dataset. There were totally 561 different variables for each observation in the raw dataset while only 86 of them were selected as ones contained means and standard deviations of measurments. Finally, the script writes the output tidy dataset to the file. Detail step-by-step description of script is presented in the script iself. The final dataset contains the averages for 180 observations (30 subjects x 6 types of activity) and 81 variables (Subject, Activity type, and 79 measurment variables). Future explanations of the study design as well as variable codes are presented in the supplementary **codebook.md** file.
