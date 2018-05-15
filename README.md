# Kaggle TalkingData AdTracking Fraud Detection

# Project Overview:

• Analyze data and implement feature visualization to render correlation and patterns using R's ggplot, corrplot. 

• Employ feature engineering to formulate important features and execute data resampling techniques leveraging R’s rose package and smote to counter imbalance data. 

• Build and evaluate classification models such as Naïve Bayes, KNN, C4.5, random forest and neural network to classify fraudulent ads, with 98% precision in R.

# Instructions:

1. train_sample.csv is our input data for this model
2. Exploratory Data Analysis.R contains the EDA on train_sample data, in which we have evaluated important features and examined correlation etc. Attached the output file called Exploratory Data Analysis_Kaggle_Kernel_Output.pdf to view the EDA.
3. FeatureDriver.R reads this file and then invokes methods from FeatureGeneration.R to generate new features after grouping the data per app.
4. The output file thus created is named FeatureEngineeredData.csv (this is unsampled data)
5. Use this file as input to run the sampling codes: Smote_Unsampled.R and Rose_Sampled.R are two separate processes which further sample the data into Smote_Sampled.csv and Rose_Sampled_Data.csv. 
6. The Rose_n_SmoteModeling.R file has two processes who respectively apply the various prediction algorithms on the sampled data generated in the above step.
7. DataModeling.R contains processes which apply the prediction algorithms on the unsampled data created in step 3

