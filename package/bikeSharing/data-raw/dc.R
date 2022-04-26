#' This script cleans and tidies the DC bike sharing data set so the
#' column names, and data types are standardized with the Seoul and London
#' bike sharing data sets. It also makes the DC bike sharing data set
#' available for use from within the package infrastructure.
#'
#' @source \url {https://www.kaggle.com/datasets/marklvl/bike-sharing-dataset}

library(dplyr)

dc <- read.csv("data-raw/DCBikeData.csv", check.names = F)

# Convert season from numeric to character
dc <- dc %>%
  mutate(Season = ifelse(season == 1, "Spring",
                         ifelse(season == 2, "Summer",
                                ifelse(season == 3, "Autumn", "Winter"))))

# Format date as a Date object
dc <- dc %>%
  mutate(Date = as.Date(dteday, format = "%Y-%m-%d"))

# Add day of the year column
dc <- dc %>%
  mutate(Day = as.numeric(strftime(Date, format = "%j")))

# Add year column
dc <- dc %>%
  mutate(Year = ifelse(Date > "2011-12-31", "Year 2", "Year 1"))

# Create rain or snow column
dc <- dc %>%
  mutate(Rain_or_snow = ifelse(weathersit %in% c(3, 4), 1, 0))

# Create weekend column
dc <- dc %>%
  mutate(Is_weekend = ifelse(weekday %in% c(0,6), 1, 0))

# Convert humidity to percentage
dc <- dc %>%
  mutate(hum = hum * 100)

# Convert windspeed - according to the kaggle page for this dataset, the
# windspeed units are normalized (divided by 67). We want to convert them
# to m/s to be comparable with the other data sets. After multiplying by
# 67, it appears that the data is likely in km/h, so this will be converted
# to m/s but it is possible this is not completely accurate.
dc <- dc %>%
  mutate(Wind_speed = (windspeed * 67) / 3.6)

# Convert normalized temperature values to C - according to the kaggle page
# for this dataset, the normalized temperature was derived via the following
# equation: (t-tmin)/(tmax-tmin), tmin=-8, t_max=+39
dc <- dc %>%
  mutate(temp = temp * (39 + 8) - 8)

# Add maximum and minimum daily temperature
temp <- dc %>%
  group_by(Date) %>%
  summarise(Min_temp = min(temp), Max_temp = max(temp))
dc <- inner_join(dc, temp, by = "Date")

# Add maximum and minimum daily humidity
hum <- dc %>%
  group_by(Date) %>%
  summarise(Min_humidity = min(hum), Max_humidity = max(hum))
dc <- inner_join(dc, hum, by = "Date")

# Standardize column names
dc <- dc %>%
  rename(Bike_count = cnt,
         Is_holiday = holiday,
         Hour = hr)

# Make the Date Only Include Month and Day, not Year
dc <- dc %>% mutate(Date = format(Date, format="%m-%d"))


# Turn hourly data into 8-hour time chunks, keeping only the columns
# that will be used in the predictive models. Hourly bike counts will
# be taken a sum of each hour over the 8-hour chunk, wind speed will
# be taken as an average over the 8-hour chunk, and rain or snow
# observations will be true (1) if there was rain or snow at any point
# over the 8-hour chunk and false (0) otherwise.
dc$Hour_chunks <- cut(dc$Hour, c(0,8,16,24), right = FALSE)
dc <- dc %>%
  group_by(Date, Hour_chunks, Day, Is_weekend, Is_holiday, Season,
           Min_temp, Max_temp, Min_humidity, Max_humidity, Year) %>%
  summarise(
    Wind_speed = mean(Wind_speed),
    Rain_or_snow = if( sum(Rain_or_snow > 0) > 0 ) {1} else {0},
    Bike_count = sum(Bike_count)
  )

dc <- data.frame(dc)

usethis::use_data(dc, overwrite = TRUE)
