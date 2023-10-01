## IMPORT REQUIRED MODULES ##
#install.packages("rlang")
#install.packages("tidymodels")
#install.packages("dplyr")
#install.packages("tidyverse")

library(rlang)
library(tidymodels)
library(dplyr)
library(tidyverse)

## 1: DOWNLOAD NOAA WEATHER DATASET ##
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz" 
download.file(url, destfile = "noaa-weather-sample-data.tar.gz")

# UNZIP THE FILE SO WE CAN GET THE CSV ONLY
untar("noaa-weather-sample-data.tar.gz", tar = "internal")



## 2: EXTRACT & READ INTO PROJECT  ##
weather_sample <- read.csv("noaa-weather-sample-data/jfk_weather_sample.csv")
head(weather_sample)
glimpse(weather_sample)



## 3: SELECT SUBSET OF COLUMNS  ##
# SELECT five columns & store the modified dataframe as hourly_weather_sample
hourly_weather_sample <- weather_sample %>%
  select(c(HOURLYRelativeHumidity, HOURLYDRYBULBTEMPF, HOURLYPrecip, HOURLYWindSpeed, 
           HOURLYStationPressure))

# Show first 10 rows of hourly_weather_sample
head(hourly_weather_sample, 10)



## 4: CLEAN UP COLUMNS  ##
# Inspect the unique values in hourly_weather_sample
unique(hourly_weather_sample$HOURLYPrecip)

# Replace in dataframe 'T' values with '0.0'
hourly_weather_sample$HOURLYPrecip[hourly_weather_sample$HOURLYPrecip == "T"] <- 0.0

# Remove in dataframe "s" values 
hourly_weather_sample$HOURLYPrecip <- str_remove(hourly_weather_sample$HOURLYPrecip,
                                                 pattern = "s$")

weather_sample$HOURLYPrecip <- hourly_weather_sample$HOURLYPrecip

# Check dataset to ensure the 'T' and 's' values been removed
unique(weather_sample$HOURLYPrecip)



## 5: CONVERT COLUMNS TO NUMERICAL TYPES  ##
# Use "glimpse()" function to check data type
glimpse(weather_sample)

# Convert "CHR" TO "DB1"
weather_sample$HOURLYPrecip <- (as.numeric(weather_sample$HOURLYPrecip))

# Check datatype of all fields
glimpse(weather_sample$HOURLYPrecip)



## 6: RENAME COLUMNS ##
weather_sample_2 <- weather_sample %>%
  rename(relative_humidity = HOURLYRelativeHumidity,
         dry_bulb_temp_f = HOURLYDRYBULBTEMPF,
         precip = HOURLYPrecip,
         wind_speed = HOURLYWindSpeed,
         station_pressure = HOURLYStationPressure)

glimpse(weather_sample_2)

## 7: EXPLORATORY DATA ANALYSIS ##
set.seed(1234)
weather_sample_2_split <- initial_split(weather_sample_2, prop = 0.8)
train_data <- training(weather_sample_2_split)
test_data <- testing(weather_sample_2_split)

ggplot(train_data, aes(x = relative_humidity)) +
  geom_histogram(color = "Maroon", fill = "lightblue")

ggplot(train_data, aes(x = dry_bulb_temp_f)) + 
  geom_histogram(color = "black", fill = "lightblue")

ggplot(train_data, aes(x = precip)) + 
  geom_histogram(color = "red", fill = "lightblue")

ggplot(train_data, aes(x = wind_speed)) +
  geom_histogram(color = "purple", fill = "lightblue")

ggplot(train_data, aes(x = station_pressure)) +
  geom_histogram(color = "orange", fill = "lightblue")



## 8: LINEAR REGRESSION ##
lm_relative_humidity <- lm(precip ~ relative_humidity, data = train_data)
ggplot(train_data, aes(x = relative_humidity, y = precip)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

lm_temp_f <- lm(precip ~ dry_bulb_temp_f, data = train_data)
ggplot(train_data, aes(x = dry_bulb_temp_f, y = precip)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

lm_wind_speed <- lm(precip ~ wind_speed, data = train_data)
ggplot(train_data, aes(x = wind_speed, y = precip)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

lm_station_pressure <- lm(precip ~ station_pressure, data = train_data)
ggplot(train_data, aes(x = station_pressure, y = precip)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")



## 9: IMPROVE THE MODEL ##
polynomial_relative_humidity <- lm(precip ~ poly(relative_humidity, 10, raw = TRUE), data = train_data)
ggplot(data = train_data, aes(relative_humidity, precip)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 10), color = "magenta")

summary(polynomial_relative_humidity)

lm_multiple <- lm(precip ~ relative_humidity + dry_bulb_temp_f + wind_speed + station_pressure, 
             data = train_data)
ggplot(train_data, aes(x = relative_humidity + dry_bulb_temp_f + wind_speed + station_pressure,
                       y = precip)) + geom_point() + stat_smooth(method = "lm", col = "magenta")

summary(lm_multiple)



## 10: FIND BEST MODEL ## 
model_names <- c(polynomial_relative_humidity, lm_multiple)
train_error <- c("0.05536", "0.04632")
#test_error <- c(?, ?) # Unable to work through MSE calculations
#comparison_df <- hourly_weather_sample(model_names, train_error)   # couldn't make out what's wrong!!
