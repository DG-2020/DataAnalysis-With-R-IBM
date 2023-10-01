# Simple Data Frame Operations
sub_airline

# Add another row before the first row
sub_airline %>% 
  add_row('ArrDelay' = 0, .before = 1)

# Missing Values
head(sub_airline, 12)

# Counting missing value in a specific column ~ using IS.NA()
sub_airline %>%
  summarise(count = sum(is.na(CarrierDelay)))

# Using purrr::map to count missing values in all columns
sub_airline %>% 
  map(~sum(is.na(.)))

# DROP Rows: use data [-c(n1, n2, ...), ] to drop specific rows
sub_airline[-c(2, 4, 6), ]

# DROP Columns: use data [ , -c(n1, n2, ...)] to drop specific columns
sub_airline[ , -c(2, 4, 6)]

# Drop the missing values
carrier_delays <- sub_airline %>% 
  drop_na(CarrierDelay)

carrier_delays

dim(sub_airline)
dim(carrier_delays)

# REPLACE missing values (in five columns)
replace_nas <- sub_airline %>%
  replace_na(list(CarrierDelay = 0,
                  WeatherDelay = 0,
                  NASDelay = 0,
                  SecurityDelay = 0,
                  LateAircraftDelay = 0
                  ))
dim(sub_airline)
dim(replace_nas)

# REFORMAT entire column: Separate flightdate in the airline dataset 
data_airline <- sub_airline %>% 
  separate(FlightDate, sep = '-', into = c('Year', 'Month', 'Day'))

head(data_airline)

# To identify datatypes
sapply(sub_airline, typeof)

# To convert data types  
#mutate_all(type.convert) %>%
  #mutate_if(is.character, as.numeric)

# Convert data type to INTEGER in columns "Year", "Month" and "Day"
data_airline %>%
  select(Year, Month, Day) %>%
  mutate_all(type.convert) %>%
  mutate_if(is.character, as.numeric)

# Data Normalization
range(sub_airline$ArrDelay)

# Simple Feature Scaling In R
head(sub_airline$ArrDelay)

simple_scale <- sub_airline$ArrDelay/max(sub_airline$ArrDelay)
head(simple_scale)

# Min-Max in R
head(sub_airline$ArrDelay)

minmax_scale <- (sub_airline$ArrDelay - min(sub_airline$ArrDelay)) / 
  (max(sub_airline$ArrDelay) - min(sub_airline$ArrDelay))
head(minmax_scale)

# Z-Score in R
head(sub_airline$ArrDelay)

z_scale <- (sub_airline$ArrDelay - mean(sub_airline$ArrDelay)) /
              sd(sub_airline$ArrDelay)
head(z_scale)

# Visualizing Data - Histogram
ggplot(data = sub_airline, mapping = aes(x = ArrDelay)) + 
  geom_histogram(bins = 100, color = "white", fill = "red") + 
  coord_cartesian(xlim = c(-73, 682))

# Binning in R tidyverse
binning <- sub_airline %>%
  mutate(Quantile_Rank = ntile(sub_airline$ArrDelay, 4))
head(binning)

# To convert categorical variables to dummy variables
sub_airline %>%
  mutate(dummy = 1) %>% # column with single value
  spread(key = Reporting_Airline, # column to spread
         value = dummy, fill = 0) %>%
  slice(1:5)

# Change Dummy Values
sub_airline %>%
  spread(key = Reporting_Airline,
         value = ArrDelay)
