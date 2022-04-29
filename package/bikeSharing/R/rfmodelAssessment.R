#' Assess random forest model fit
#'
#' This function assesses the fit of a trained random forest
#' model on a given dataset
#'
#' @param model trained random forest model for assessment
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
#'         rf model to the data
#'
#' @importFrom stats cor predict
#'
#' @export
rf_model_fit <- function(model, data, scale_to_reference_mean = "no",
                         reference) {

  checkBikeData(data)
  checkBikeData(reference)

  checkModelRF(model)

  if (!(scale_to_reference_mean %in% c("yes", "no"))) {
    stop("scale_to_reference_mean must be 'yes' or 'no'")
  }

  x = subset(data,
  select = c("Hour_chunks", "Is_weekend", "Is_holiday", "Season", "Min_temp",
             "Max_temp", "Min_humidity", "Max_humidity", "Wind_speed",
             "Rain_or_snow", "Date"))

  ## Predict Values
  pred = predict(model, x)

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

