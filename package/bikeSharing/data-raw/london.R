#' This script cleans and tidies the London bike sharing data set so the
#' column names, and data types are standardized with the Seoul and DC
#' bike sharing data sets. It also makes the London bike sharing data set
#' available for use from within the package infrastructure.
#'
#' @source \url {https://www.kaggle.com/datasets/hmavrodiev/london-bike-sharing-dataset}

library(dplyr)

london <- read.csv("data-raw/LondonBikeData.csv", check.names = F)

# Convert season from numeric to factor
london <- london %>%
  mutate(Season = factor(ifelse(season == 0, "Spring",
                         ifelse(season == 1, "Summer",
                                ifelse(season == 2, "Autumn", "Winter"))),
                         levels = c("Spring", "Summer", "Autumn", "Winter")))

# Extract date from timestamp
london <- london %>%
  mutate(Date = as.Date(timestamp))

# Extract hour from timestamp
london <- london %>%
  mutate(Hour = as.numeric(sub(":.*", "", sub(".* ", "", timestamp))))

# Add day of the year column
london <- london %>%
  mutate(Day = as.numeric(strftime(Date, format = "%j")))

# Add year column
london <- london %>%
  mutate(Year = ifelse(Date > "2016-01-03", "Year 2", "Year 1"))

# Create rain or snow column
london <- london %>%
  mutate(Rain_or_snow = ifelse(weather_code %in% c(7, 10, 26), 1, 0))

# Convert wind speed from km/h to m/s
london <- london %>%
  mutate(Wind_speed = wind_speed / 3.6)

# Add maximum and minimum daily temperature
temp <- london %>%
  group_by(Date) %>%
  summarise(Min_temp = min(t1), Max_temp = max(t1))
london <- inner_join(london, temp, by = "Date")

# Add maximum and minimum daily humidity
hum <- london %>%
  group_by(Date) %>%
  summarise(Min_humidity = min(hum), Max_humidity = max(hum))
london <- inner_join(london, hum, by = "Date")

# Standardize column names
london <- london %>%
  rename(Bike_count = cnt,
         Is_weekend = is_weekend,
         Is_holiday = is_holiday,
  )

# Make the Date Only Include Month and Day, not Year
london <- london %>% mutate(Date = format(Date, format="%m-%d"))

# Remove leap day
london <- london %>%
  filter(Date != "02-29")

# Turn hourly data into 8-hour time chunks, keeping only the columns
# that will be used in the predictive models. Hourly bike counts will
# be taken a sum of each hour over the 8-hour chunk, wind speed will
# be taken as an average over the 8-hour chunk, and rain or snow
# observations will be true (1) if there was rain or snow at any point
# over the 8-hour chunk and false (0) otherwise.
london$Hour_chunks <- cut(london$Hour, c(0,8,16,24), right = FALSE)
london <- london %>%
  group_by(Date, Hour_chunks, Day, Is_weekend, Is_holiday, Season,
           Min_temp, Max_temp, Min_humidity, Max_humidity, Year) %>%
  summarise(
    Wind_speed = mean(Wind_speed),
    Rain_or_snow = if( sum(Rain_or_snow > 0) > 0 ) {1} else {0},
    Bike_count = sum(Bike_count)
  )

london <- data.frame(london)

# Binary data as factors
london <- london %>%
  mutate(Is_weekend = factor(Is_weekend, levels = c("0", "1"))) %>%
  mutate(Is_holiday = factor(Is_holiday, levels = c("0", "1"))) %>%
  mutate(Rain_or_snow = factor(Rain_or_snow, levels = c("0", "1")))

usethis::use_data(london, overwrite = TRUE)
