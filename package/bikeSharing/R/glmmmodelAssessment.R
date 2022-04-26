#' Assess glmm model fit
#'
#' This function assesses the fit of a trained negative binomial mixed model
#' model on a given dataset
#'
#' @param model_glmm trained negative binomial mixed model for assessment (output of MCEM_algorithm() function)
#' @param data pre-processed bike sharing data frame
#'             with columns  Hour_chunks, Is_weekend, Is_holiday, 
#'             Season, Min_temp, Max_temp, Min_humidity, Max_humidity,
#'             Wind_speed, Rain_or_snow, Date
#' @param scale_to_seoul_mean "yes" or "no". If "yes",
#'             scales the bike count mean of the input
#'             data to the Seoul data bike count mean
#'
#' @return a data frame summarizing the RMSE, MAE, and R2 value of the fitted glmm model to the data
#'         
#'
#' @import 
#'
#' @export

glmm_model_fit <- function(model_glmm, data, scale_seoul_mean = "no"){
  
  ## Keep only same days as in seoul set
  data = data[data$Date %in% seoul$Date,]
  
  ## Name day ranef from seoul
  names(model_glmm$day_ranef) = unique(seoul$Date)
  
  ## Predict log Mean for each day Values
  model_matrix = model.matrix( Bike_count ~ Hour_chunks + Is_weekend + Is_holiday + Season + Min_temp + 
                                 Max_temp + Min_humidity + Max_humidity + Wind_speed + Rain_or_snow, data = data)
  fixedef = c(model_matrix %*% model_glmm$beta)
  ranef = model_glmm$day_ranef[match(data$Date,names(model_glmm$day_ranef))]
  logmeans = fixedef + ranef
  
  ## Transform to get prediction
  pred = exp(logmeans)
  
  
  ## Scale to seoul bike count mean
  if(scale_seoul_mean == "yes"){
    ## Scaling Factor
    scale = mean(data$Bike_count)/
      mean(seoul$Bike_count)
    pred = pred * scale
  }
  
  ## Obtained RMSE
  RMSE = sqrt(mean((data$Bike_count - pred)^2))
  
  ## Obtain MAE
  MAE = mean(abs(data$Bike_count - pred)) 
  
  ## Obtain R^2
  R2 = cor(data$Bike_count, pred, use = "pairwise.complete.obs")^2 ## R squared
  
  
  ## Combine into data frame
  model_fit = data.frame(RMSE = RMSE, MAE = MAE, R2 = R2)
  
  return(model_fit)
}
