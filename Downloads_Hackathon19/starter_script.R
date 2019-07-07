#!/usr/bin/R
# Copyright (c)2019 Accenture and/or its affiliates.  All Rights Reserved.  
# You may not use, copy, modify, and/or distribute this code and/or its documentation 
# without permission from Accenture.Please contact the Advanced Analytics-Operations 
# Analytics team and/or Frode Huse Gjendem (lead) with any questions.

# This is the starter script for the Accenture Health Datathon 2019 Competition.

# version 1.0
# date: 2019/05

rm(list = ls()) # Clear workspace.


# -----1. Set configuration & Data Import.----- 

Sys.setlocale("LC_TIME", "English")

# Enter your input data and output data paths below.
PATH = getwd() # Otherwise use your own path
OUTPATH = getwd()
# Set the input data folder as default path.
setwd(PATH)

# ----- Attach packages -----

usePackage <- function(p) {
  if ( !is.element(p, installed.packages()[,1]) ) {
    install.packages(p, dep = TRUE)}
  require(p, character.only = TRUE)}

packages <- c("dplyr", "stringr", "data.table", "survival", "zoo")

for (p in packages) { usePackage(p) }

# ----- BRIER ERROR function -----

# source("error_function.R")
source("./r/03_starter/error_function.R")

# ----- Read data -----

# train <- fread("train.csv")
# test <- fread("test.csv")

train <- fread("./data/final/train_specific.csv")
test <- fread("./data/final/test_specific.csv")

# -----2. Data Transformation.----- 

# Add your feature engineering here. 



# -----3. Run a Cox model.----- 

# Create Survival Object
surv_object <- Surv(time = train$months_survival, event = train$specific_death)

# Fit Cox Model
fitform <- surv_object ~ age + cutaneous_biopsy_breslow
cox_model <- coxph(fitform, data = train)
survfit(cox_model)


# -----4. Perform the prediction.----- 

# Compute estimate of the survival curves
cox_prediction <- survfit(cox_model, newdata = test)

# Plot survival curves for certain observations (here we take first 3 observations)
plot(cox_prediction[1:3])

# Gather Predictions
test_predictions <- cox_prediction$surv %>% as.data.table()

# ----- Format Predictions -----

setnames(test_predictions, names(test_predictions), test$ID)

# Months Vectors
train_months_survival <- train[, months_survival] %>% unique() %>% sort(decreasing = FALSE)
months_survival <- 0:(12*10)

# Re-arrange prediction matrix to fill in missing months.
# Please, remember that rows NEED TO BE indexed by patient IDs and columns MUST be ordered from T0 to T120
IDs <- test[, ID]
submission <- data.table(months_survival = months_survival) %>% 
  merge(test_predictions[, months_survival := train_months_survival], by = 'months_survival', all.x = TRUE) %>% 
  .[, c(IDs) := lapply(.SD, function(x) na.locf(x)), .SDcols = c(IDs)] %>% 
  .[, months_survival := NULL] %>% t()

# Update colnames
colnames(submission) <- str_c("T", 0:120)


# -----5. Calculate the error.----- 

# To be filled
actual <- mapply(populate_actual, months = __ , status = __ ,
                max_period = __ ) %>% t()

weights <- calc_weights(data = __)

error <- brier_score_loss_weighted(pred = __, actual = __, weights = __, years_cutoff = 10)

print(str_c("Overall error: ", error))


# -----6. Save the submission.----- 

# Write the final CSV file
# Please, remember than in order to make the submission you need to create a .zip file ONLY with the csv
fwrite(submission, file=str_c(OUTPATH,'/sample-submission-cox.csv'), row.names = F)

# Clear memory.
rm(list = ls()) 
