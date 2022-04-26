#' This script cleans and tidies the Seoul bike sharing data set so the
#' column names, and data types are standardized with the London and DC
#' bike sharing data sets. It also makes the Seoul bike sharing data set
#' available for use from within the package infrastructure.
#'
#' @source \url {https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand}

library(dplyr)

seoul <- read.csv("data-raw/SeoulBikeData.csv", check.names = F)

# Remove non-functional observations
seoul <- seoul %>%
  filter(`Functioning Day` == "Yes")

# Combine rain and snow into binary variables
seoul <- seoul %>%
  mutate(Rain_or_snow = ifelse(`Rainfall(mm)` > 0 |
                                 `Snowfall (cm)` > 0, 1, 0))

# Convert season from character to factor
seoul <- seoul %>%
  mutate(Season = factor(Seasons,
                         levels = c("Spring", "Summer", "Autumn", "Winter")))

# Convert the date into an R date object
seoul <- seoul %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

# Add day of the year column
seoul <- seoul %>%
  mutate(Day = as.numeric(strftime(Date, format = "%j")))

# Create a weekend column
seoul <- seoul %>%
  mutate(Weekend = ifelse(weekdays(Date) == "Saturday" |
                            weekdays(Date) == "Sunday", 1, 0))

# Format the holiday column to be binary
seoul <- seoul %>%
  mutate(Holiday = ifelse(Holiday == "Holiday", 1, 0))

# Add maximum and minimum daily temperature
temp <- seoul %>%
  group_by(Date) %>%
  summarise(Min_temp = min(`Temperature(C)`),
            Max_temp = max(`Temperature(C)`))
seoul <- inner_join(seoul, temp, by = "Date")

# Add maximum and minimum daily humidity
hum <- seoul %>%
  group_by(Date) %>%
  summarise(Min_humidity = min(`Humidity(%)`), Max_humidity = max(`Humidity(%)`))
seoul <- inner_join(seoul, hum, by = "Date")

# Standardize column names
seoul <- seoul %>%
  rename(Bike_count = `Rented Bike Count`,
         Wind_speed = `Wind speed (m/s)`,
         Is_weekend = Weekend,
         Is_holiday = Holiday)

# Make the Date Only Include Month and Day, not Year
seoul <- seoul %>% mutate(Date = format(Date, format="%m-%d"))


# Turn hourly data into 8-hour time chunks, keeping only the columns
# that will be used in the predictive models. Hourly bike counts will
# be taken a sum of each hour over the 8-hour chunk, wind speed will
# be taken as an average over the 8-hour chunk, and rain or snow
# observations will be true (1) if there was rain or snow at any point
# over the 8-hour chunk and false (0) otherwise.
seoul$Hour_chunks <- cut(seoul$Hour, c(0,8,16,24), right = FALSE)
seoul <- seoul %>%
  group_by(Date, Hour_chunks, Day, Is_weekend, Is_holiday, Season,
           Min_temp, Max_temp, Min_humidity, Max_humidity) %>%
  summarise(
    Wind_speed = mean(Wind_speed),
    Rain_or_snow = if( sum(Rain_or_snow > 0) > 0 ) {1} else {0},
    Bike_count = sum(Bike_count)
  )

seoul <- data.frame(seoul)

usethis::use_data(seoul, overwrite = TRUE)
