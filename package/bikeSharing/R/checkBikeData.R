#' Check correct format of bike sharing data
#'
#' This is a helper function to ensure that the bike
#' sharing dataset used as input to a function is
#' properly formatted. Throws warnings if formatting
#' is incorrect.
#'
#' @param data pre-processed bike sharing data frame
#'             with columns Bike_count, Hour_chunks,
#'             Max_temp, Rain_or_snow, and Date
#'
checkBikeData <- function(data) {

  if (!is.data.frame(data)) {
    stop("Input data is not a data frame")
  }
  if (!all(
    c("Bike_count", "Hour_chunks", "Max_temp", "Rain_or_snow", "Date") %in%
           colnames(seoul))) {
    stop("Input data does not contain all necessary columns:
         Bike_count, Hour_chunks, Max_temp, Rain_or_snow, and Date")
  }
  if (!is.numeric(data$Bike_count)) {
    stop("Bike_count column must be numeric")
  }
  if(length(levels(seoul$Hour_chunks)) != 3) {
    stop("Hour_chunks column must contain exactly 3 levels")
  }
  if(!all(levels(seoul$Hour_chunks) == c("[0,8)", "[8,16)", "[16,24)"))) {
    stop('Hour_chunks levels must be "[0,8)", "[8,16)", "[16,24)"')
  }
  if (!is.numeric(data$Max_temp)) {
    stop("Max_temp column must be numeric")
  }
  if (!is.numeric(data$Rain_or_snow)) {
    stop("Rain_or_snow column must be numeric")
  }
  if(!all(unique(data$Rain_or_snow) %in% c(0, 1))) {
    stop("Rain_or_snow column must contain only values of 0 and 1")
  }
  if(!is.character(data$Date)) {
    stop("Date column must be of class character (format should be mm-dd)")
  }

}
