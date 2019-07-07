#!/usr/bin/python
"""
Copyright 2019 Accenture and/or its affiliates.  All Rights Reserved.  
You may not use, copy, modify, and/or distribute this code and/or its documentation without permission from Accenture.
Please contact the Advanced Analytics-Operations Analytics team and/or Frode Huse Gjendem (lead) with any questions.

\brief This is the starter script for the Accenture's Health Datathon 2019 competition.

\version 1.0

\date $Date: 2019/05

"""
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import LabelEncoder
from sksurv.preprocessing import OneHotEncoder
from sksurv.linear_model import CoxPHSurvivalAnalysis
import error_function # Inside this module you will find the function to calculate Brier error


######################################
# 1. Set configuration & Data Import #
######################################

# Enter your input data and output data paths below.
PATH = os.getcwd()
OUTPATH = os.getcwd()

# Set the input data folder as working directory.
os.chdir(PATH)


# Read the train & test files. 
train = pd.read_csv("train.csv",  index_col='ID')
test = pd.read_csv("test.csv", index_col='ID')


##########################
# 2. Data transformation #
##########################
 
# Add your feature engineering here. 

# Feature selection
x_train = train[['age', 'cutaneous_biopsy_breslow']]
y_train = train[['specific_death', 'months_survival']]


#######################################
# 3. Prepare the data & fit the model #
#######################################

'''
@paramters
    x_train: Dataframe on which to train the model
        Format: Index = ID, Rows = patients, cols = ['Any variables you think may be predictive']
    y_train: Dependent variables on which to train the dataset
        Format: Index = ID, Rows = patients, cols = ['specific_death', 'months_survival']
    test_df: Dataframe on which to test the model
        Format: Index = ID, Rows = patients, cols = ['Any variables you think may be predictive']
@ return
    estimator: survival model
    x_test: x_test dataframe. Format: Index = ID, Rows = patients, cols = created by above
    x_train: x_train dataframe Format: Index = ID, Rows = patients, cols = created by above
    y_train: y_train dataframe Format: Index = ID, Rows = patients, cols = ['specific_death', 'months_survival']
'''
def fit_and_prepare(x_train, y_train, test_df):
    
    # 3.1. Prepare Y-----
    y_train.specific_death = y_train.specific_death.astype(bool)
    
    # Transform it into a structured array
    y_train = y_train.to_records(index = False)
    
    # 3.2. Prepare X-----
    # obtain the x variables that are categorical
    categorical_feature_mask = x_train.dtypes==object

    # Filter categorical columns using mask and turn it into a list
    categorical_cols = x_train.columns[categorical_feature_mask].tolist()

    # Ensure categorical columns are category type
    for col in categorical_cols:
        x_train[col] = x_train[col].astype('category')
        test_df[col] = test_df[col].astype('category')
    
    # 3.3. Fit model-----
    # initiate
    encoder = OneHotEncoder()
    estimator = CoxPHSurvivalAnalysis()
    
    # fit model
    estimator.fit(encoder.fit_transform(x_train), y_train)
    
    # transform the test variables to match the train
    x_test = encoder.transform(test_df)
    
    return (estimator, x_test, x_train, y_train)

# 3.1 Call the function
estimator, x_test, x_train, y_train = fit_and_prepare(x_train, y_train, test)


#############################
# 4. Perform the prediction #
#############################

# 4.1 Get the predictions (probability of being alive) for each patient in test
'''
@paramters
    x_test: Created from above function fit_and_prepare
        Format: Index = ID, Rows = patients, cols = created by above
    estimator: Created from above function fit_and_prepare; this is the model you just trained
'''
def get_probabilities(x_test, estimator):
    
    pred_surv = estimator.predict_survival_function(x_test)

    # Get the "X's" or time of each data point
    times = pred_surv[0].x

    # Create an empty pandas dataframes with these times as the columns
    pred_df = pd.DataFrame(columns = times)

    # Convert each row to a pandas series row (transpose) with the index as these x times and append it to the df
    for i in range(0, len(x_test)):
        pred_df = pred_df.append(pd.DataFrame(pred_surv[i].y).set_index(times).T) 

    pred_df = pred_df.set_index(x_test.index)

    return pred_df

# 4.2 store the predictions
predictions = get_probabilities(x_test, estimator)

# 4.3 Compute estimate of the survival curves
pred_curves = estimator.predict_survival_function(x_test)

# Plot survival curves for certain observations (here we take first 3 observations)
for curve in pred_curves[0:3]:
    plt.step(curve.x, curve.y, where="post")
  
    
##########################
# 5. Calculate the error #
##########################
    
# To be filled
# Using the different module error_functions obtain the error matrix for y_test
weights = error_function.calc_weights(__) 

error = error_function.brier_score_loss_weighted(pred = __,
                                                 actual = __, 
                                                 weights = __, 
                                                 years_cutoff = __)


print("Overall error: ", error)


###########################
# 6 Create the submission #
###########################

# Please, remember that rows NEED TO BE indexed by patient IDs and columns MUST be ordered from T0 to T120

# First subset to 10 years
predictions_10yr = predictions.iloc[:,:121]

#Rename columns to Time periods
columns = predictions_10yr.columns.values
new_columns = ['T' + str(s) for s in columns]
predictions_10yr.columns = new_columns

# Write the final CSV file
# Please, remember than in order to make the submission you need to create a .zip file ONLY with the csv
pd.DataFrame(predictions_10yr).to_csv(OUTPATH + '/sample-submission-cox.csv')




