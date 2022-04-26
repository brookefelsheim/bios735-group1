#' Assess random forest model fit
#'
#' This function assesses the fit of a trained random forest
#' model on a given dataset
#'
#'
#' @param model trained random forest mmodel for assessment
#' @param data pre-processed bike sharing data frame
#'             with columns  Hour_chunks, Is_weekend, Is_holiday, 
#'             Season, Min_temp, Max_temp, Min_humidity, Max_humidity,
#'             Wind_speed, Rain_or_snow, Date
#' @param scale_to_seoul_mean "yes" or "no". If "yes",
#'             scales the bike count mean of the input
#'             data to the Seoul data bike count mean
#'
#' @return a data frame summarizing the RMSE, MAE, and R2 value of the fitted rf model to the data
#'
#' @import caret
#'
#' @export
rf_model_fit <- function(model, data, scale_to_seoul_mean = "no") {

  x = subset(data,
             select = c("Hour_chunks", "Is_weekend", "Is_holiday", 
                        "Season", "Min_temp", "Max_temp", "Min_humidity", "Max_humidity",
                        "Wind_speed", "Rain_or_snow", "Date"))
  ## Predict Values
  pred = predict(model, x)

  ## Scale to seoul bike count mean
  if(scale_to_seoul_mean == "yes"){
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

