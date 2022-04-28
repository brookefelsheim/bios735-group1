#' Check correct format of bike sharing data
#'
#' This is a helper function to ensure that the bike
#' sharing dataset used as input to a function is
#' properly formatted. Throws warnings if formatting
#' is incorrect.
#'
#' @param data pre-processed bike sharing data frame
#'             with columns Hour_chunks, Is_weekend,
#'             Is_holiday, Season, Min_temp, Max_temp,
#'             Min_humidity, Max_humidity, Wind_speed,
#'             Rain_or_snow, Date, Bike_count
#'
checkBikeData <- function(data) {

  if (!is.data.frame(data)) {
    stop("Input data is not a data frame")
  }
  if (!all(
    c("Bike_count", "Hour_chunks", "Rain_or_snow", "Date",
      "Is_weekend", "Is_holiday", "Season", "Min_temp", "Max_temp",
      "Min_humidity", "Max_humidity", "Wind_speed") %in%
           colnames(data))) {
    stop("Input data does not contain all necessary columns:
         Bike_count, Hour_chunks, Max_temp, Rain_or_snow, and Date")
  }
  if (!is.numeric(data$Bike_count)) {
    stop("Bike_count column must be numeric")
  }
  if(length(levels(data$Hour_chunks)) != 3) {
    stop("Hour_chunks column must contain exactly 3 levels")
  }
  if(!all(levels(data$Hour_chunks) == c("[0,8)", "[8,16)", "[16,24)"))) {
    stop('Hour_chunks levels must be "[0,8)", "[8,16)", "[16,24)"')
  }
  if (!is.numeric(data$Max_temp)) {
    stop("Max_temp column must be numeric")
  }
  if (!is.numeric(data$Min_temp)) {
    stop("Min_temp column must be numeric")
  }
  if (!is.numeric(data$Max_humidity)) {
    stop("Max_humidity column must be numeric")
  }
  if (!is.numeric(data$Min_humidity)) {
    stop("Min_humidity column must be numeric")
  }
  if (!is.numeric(data$Wind_speed)) {
    stop("Wind_speed column must be numeric")
  }
  if (!is.factor(data$Rain_or_snow)) {
    stop("Rain_or_snow column must be a factor")
  }
  if(!all(unique(data$Rain_or_snow) %in% c(0, 1))) {
    stop("Rain_or_snow column must contain only values of 0 and 1")
  }
  if (!is.factor(data$Is_weekend)) {
    stop("Is_weekend column must be a factor")
  }
  if(!all(unique(data$Is_weekend) %in% c(0, 1))) {
    stop("Is_weekend column must contain only values of 0 and 1")
  }
  if (!is.factor(data$Is_holiday)) {
    stop("Is_holiday column must be a factor")
  }
  if(!all(unique(data$Is_holiday) %in% c(0, 1))) {
    stop("Is_holiday column must contain only values of 0 and 1")
  }
  if(!is.factor(data$Season)) {
    stop("Season column must be a factor")
  }
  if(!all(unique(data$Season) %in%
          c("Winter", "Spring", "Summer", "Autumn"))) {
    stop("Season column must contain only values of
         'Winter', 'Spring', 'Summer', and 'Autumn'")
  }
  if(!is.character(data$Date)) {
    stop("Date column must be of class character (format should be mm-dd)")
  }

}
