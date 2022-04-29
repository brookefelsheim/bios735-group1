#' Assess glmm model fit
#'
#' This function assesses the fit of a trained negative binomial mixed model
#' model on a given dataset
#'
#' @param model_glmm trained negative binomial mixed model for assessment
#'                   (output of MCEM_algorithm() function)
#' @param data pre-processed bike sharing data frame
#'             with columns Hour_chunks, Is_weekend, Is_holiday,
#'             Season, Min_temp, Max_temp, Min_humidity, Max_humidity,
#'             Wind_speed, Rain_or_snow, Date, Bike_count. This is the
#'             dataset the model is being assessed on.
#' @param scale_to_reference_mean "yes" or "no". If "yes",
#'             scales the bike count mean of the input
#'             data to the reference data bike count mean
#' @param reference pre-processed bike sharing data frame with columns
#'                  Hour_chunks, Is_weekend, Is_holiday, Season,
#'                  Min_temp, Max_temp, Min_humidity, Max_humidity,
#'                  Wind_speed, Rain_or_snow, Date, Bike_count. This is
#'                  the dataset that the model is being referenced to
#'                  (should be the set the model was trained on).
#'
#' @return a data frame summarizing the RMSE, MAE, and R2 value of the fitted
#'         glmm model to the data
#'
#' @importFrom stats model.matrix cor
#'
#' @export
glmm_model_fit <- function(model_glmm, data,
                           scale_to_reference_mean = "no",
                           reference) {

  checkBikeData(data)
  checkBikeData(reference)

  checkModelGLMM(model_glmm)

  if (!(scale_to_reference_mean %in% c("yes", "no"))) {
    stop("scale_to_reference_mean must be 'yes' or 'no'")
  }

  ## Keep only same days as in reference set
  data = data[data$Date %in% reference$Date,]

  ## Name day ranef from reference
  names(model_glmm$day_ranef) = unique(reference$Date)

  ## Predict log Mean for each day Values
  model_matrix = model.matrix( Bike_count ~ Hour_chunks + Is_weekend +
                                 Is_holiday + Season + Min_temp + Max_temp +
                                 Min_humidity + Max_humidity + Wind_speed +
                                 Rain_or_snow, data = data)
  fixedef = c(model_matrix %*% model_glmm$beta)
  ranef = model_glmm$day_ranef[match(data$Date,names(model_glmm$day_ranef))]
  logmeans = fixedef + ranef

  ## Transform to get prediction
  pred = exp(logmeans)


  ## Scale to reference bike count mean
  if(scale_to_reference_mean == "yes"){
    ## Scaling Factor
    scale = mean(data$Bike_count)/
      mean(reference$Bike_count)
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
