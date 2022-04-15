#' Pre-processed London bike sharing data
#'
#' The London bike sharing data with data that has been standardized and
#' grouped in 8-hour time chunks. Pre-processing script can be found in
#' the raw_data/ directory.
#'
#' @format A data frame with 730 rows and 12 variables:
#' \describe{
#'   \item{Date}{Date of observation, yyyy-mm-dd, class: Date}
#'   \item{Hour_chunks}{8-hour time chunk, class: factor, 3 levels}
#'   \item{Min_temp}{Daily minimum temperature in Celcius, class: num}
#'   \item{Max_temp}{Daily maximum temperature in Celcius, class: num}
#'   \item{Min_humidity}{Daily minimum humidity percentage, class: num}
#'   \item{Max_humidity}{Daily maximum humidity percentage, class: num}
#'   \item{Is_weekend}{Weekend (1) or not weekend (0), class: num}
#'   \item{Season}{Autumn, Spring, Summer, or Winter, class: chr}
#'   \item{Is_holiday}{Holiday (1) or not holiday (0), class: num}
#'   \item{Wind_speed}{Average wind speed in m/s during time chunk, class: num}
#'   \item{Rain_or_snow}{Occrrence of any rain or snow (1) during time chunk or not (0), class: num}
#'   \item{Bike_count}{Number of bikes rented during time chunk, class: int}
#' }
"london"
