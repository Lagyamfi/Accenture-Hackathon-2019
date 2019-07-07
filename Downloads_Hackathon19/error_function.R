#!/usr/bin/R
# Copyright (c)2019 Accenture and/or its affiliates.  All Rights Reserved.  
# You may not use, copy, modify, and/or distribute this code and/or its documentation 
# without permission from Accenture.Please contact the Advanced Analytics-Operations 
# Analytics team and/or Frode Huse Gjendem (lead) with any questions.

# brief: This is the error function for the Accenture Health Datathon 2019 Competition.

# version 1.0
# date: 2019/05

# ===== Functions in R ===== 
options(digits = 17)

## 1. Transform the vector of actual predictions into a matrix ====

#' Function to create actuals vector
#' @param months is the months of survival of an observation
#' @param status is the status of the observation
#' @param max_period is the maximum survival months in the dataset
#' @return the vector of actual status over time
populate_actual <- function(months, status, max_period) 
  return(c(rep(1, months), rep(1 - status, max_period - months + 1)))

##2. Calculate the weights ====

#' Function to select certain/uncertain weights
#' @param months is the months of survival of an observation
#' @param status is the status of the observation
#' @param max_period is the maximum survival months in the dataset
#' @return the vector of weights
get_weights <- function(months, status, max_period) {
  
  if(status == 1) {
    x <- c(rep(1, max_period + 1))
  } else {
    x <- c(rep(1, months), prob_vec[(months + 1):length(prob_vec)])
  } 
  
  return(x)
  
}

#' Function to calculate the weight matrix
#' @param data is the data table on which we want to compute the weights. Data.table() format is required
#' @return the weights matrix with the weights for each patient at each time t
calc_weights <- function(data){
  
  data <- setDT(data)
  if(any(!c("specific_death", "months_survival") %in% names(data))){message("specific_death and months_survival are required in data")}
  
  max_period = max(data$months_survival)
  all_months_survival <- (0:max_period)
  aux <- data.table(months_survival = all_months_survival) %>% 
    merge(data[,.(specific_death, months_survival)], by = 'months_survival', all.x = TRUE) %>%
    .[, .(patientN = length(specific_death[!is.na(specific_death)]), specificN = sum(specific_death, na.rm = TRUE)), by = .(months_survival)]
  
  aux[order(-months_survival), prob_num := shift(cumsum(patientN), type = "lag", n = 1, fill = 0)]
  aux[order(months_survival), prob_den := shift(cumsum(specificN), type = "lag", n = 1, fill = 0)]
  
  aux[, prob := (prob_num/(prob_num + prob_den))]
  
  # Set probabilities vector as global var to have it for get_weights() where months and status need to be simplified by point element but prob_vec not
  prob_vec <<- aux[order(months_survival), prob]

  # Populate weights
  pop_weights <- mapply(get_weights, months = data[, months_survival], status = data[, specific_death], 
                        max_period = max_period) %>% t()
  
  weights <- apply(pop_weights, 1, function(x) cumprod(x)) %>% t()

  return(weights)
  
}

  
##3. Calculate the average error across time and patients ====

#' Function to compute the weighted Brier score
#' @param pred is the prediction matrix
#' @param actual is the actual value matrix
#' @param weights is the matrix of weights associated to the predictions
#' @param years_cutoff is the cut-off time in years for which the error is computed
#' @return the error for each prediction
brier_score_loss_weighted <- function(pred, actual, weights, years_cutoff = 10){
  w_errors <- (pred - actual)*(pred - actual)*weights
  mean_w_error <- apply(w_errors, 2, function(x) mean(x))
  error <- mean(mean_w_error[1:((years_cutoff*12)+1)])
  return(error)
}

