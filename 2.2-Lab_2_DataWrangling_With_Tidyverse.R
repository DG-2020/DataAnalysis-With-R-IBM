### LOAD LIBRARIES AND DATA ###
library(tidyverse)

# URL where data is located
url <-
  "https;//dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# Download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# UNTAR the file so we can get the csv only (if we run this on a local machine, then remove tar = "internal")
untar("lax_to_jfk.tar.gz", tar = "internal")

# READ_CSV only
sub_airline <- read_csv(
  "lax_to_jfk/lax_to_jfk.csv",
  col_types = cols('DivDistance' = col_number(),
                   'DivArrDelay' = col_number())
)

head(sub_airline)
dim(sub_airline)

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

### 1. MISSING VALUES AND FORMATING ###
head(sub_airline)

## 1.1: Identifying Missing Values ##
is.na(c(1, NA))
is.na(paste(c(1, NA)))

anyNA(c(1, NA))

# Counting Missing Values for a Particular Column
sub_airline %>%
  summarise(count = sum(is.na(CarrierDelay)))

# Counting Missing Values for All Column: using purrr::map()
map(sub_airline, ~ sum(is.na(.)))

dim(sub_airline)


## 1.2: Handle Missing Data

# Drop the whole column: All columns are empty or NA
drop_na_cols <- sub_airline %>%
  select(-DivDistance,-DivArrDelay)

dim(drop_na_cols)
head(drop_na_cols)

# Drop the whole row: same amount if missing value from summary. by droping missing value from one column will
# also solve the missing value issues in the others
drop_na_rows <- drop_na_cols %>%
  drop_na(CarrierDelay)
dim(drop_na_rows)
head(drop_na_rows)

# Convert NA to 0
# Replace missing value in 5 columns
replace_na <- drop_na_rows %>%
  replace_na(
    list(
      CarrierDelay = 0,
      WeatherDelay = 0,
      NASDelay = 0,
      SecurityDelay = 0,
      LateAircraftDelay = 0
    )
  )
head(replace_na)

### QUESTION 1: Let's try to replace NA in "CarrierDelay" column by the MEAN value.

# Calculate mean value CarrierDelay column
CarrierDelay_Mean <- mean(drop_na_rows$CarrierDelay)
CarrierDelay_Mean

# Replace NA by mean value in CarrierDelay column
sub_airline %>%
  replace_na(list(CarrierDelay = CarrierDelay_Mean))


## 1.3: Correct Data Format
sub_airline %>%
  summarise_all(class) %>%
  gather(variable, class)

# Reformat flightdate field into three separate fields (year, month, day) using separte()
data_airline <- replace_na %>%
  separate(FlightDate,
           sep = "-",
           into = c("Year", "Month", "Day"))
head(data_airline)

# Mutating Year, Month and Day values to numeric only
data_airline %>% 
  select(Year, Month, Day) %>% 
  mutate_all(type.convert) %>% 
  mutate_if(is.character, as.numeric)

data_airline %>%
  summarise_all(class) %>%
  gather(variable, class)

# Secondary Approach
data_airline$Year <-  as.integer(data_airline$Year)
class(data_airline$Year)

data_airline$Month <- as.integer(data_airline$Month)
class(data_airline$Month)

data_airline$Day <- as.integer(data_airline$Day)
class(data_airline$Day)

data_airline %>%
  summarise_all(class) %>%
  gather(variable, class)
#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#


### 2. DATA NORMALIZATION ###
# NORMALIZATION is a process of transforming values of several features (variables) into a similar range. An 
# example if why this could be important is if we have a variable for INCOME and another variable for AGE. INCOME
# is likely much BIGGER values than AGE, so in a model, INCOME would naturally influence the model more. Thus, 
# normalization helps make comparisons between variable more fair.


## 2.1. SIMPLE SCALING (for ArrDelay)
simple_scale <- sub_airline$ArrDelay / max(sub_airline$ArrDelay)
head(simple_scale)

### QUESTION 2: Normalize the column "DepDelay" using simple scaling technique.
simple_scale_2 <- sub_airline$DepDelay / max(sub_airline$DepDelay)
head(simple_scale_2)


## 2.2. MIN-MAX (for ArrDelay)
minmax_scale <- (sub_airline$ArrDelay - min(sub_airline$ArrDelay)) / 
  (max(sub_airline$ArrDelay) - min(sub_airline$ArrDelay))
head(minmax_scale)


## 2.3. Data Standardization (Z- Score)
z_scale <- (sub_airline$ArrDelay - mean(sub_airline$ArrDelay)) / sd(sub_airline$ArrDelay)
head(z_scale)


### QUESTION 3: Standardize the "DepDelay" column
z_scale_2 <- (sub_airline$DepDelay - mean(sub_airline$DepDelay)) / sd(sub_airline$DepDelay)
head(z_scale_2)
#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

### 3. BINNING
ggplot(data = sub_airline, mapping = aes(x = ArrDelay)) + 
  geom_histogram(bins = 100, color = "white", fill = 'red') + 
  coord_cartesian(xlim = c(-73, 682))

# Use dplyr function 'ntile' to break 'arrdelay' into 4 buckets
binning <- sub_airline %>%
  mutate(quantile_rank = ntile(sub_airline$ArrDelay, 4))
head(binning)

ggplot(data = binning, mapping = aes(x = quantile_rank)) + 
  geom_histogram(bins =4, color = 'white', fill = 'red')

### 4. INDICATOR VARAIBLE
sub_airline %>%
  mutate(dummy = 1) %>%    # Column with single value
  spread(key = Reporting_Airline,   # Column to spread
         value = dummy,
         fill = 0) %>%
  slice(1:5)

sub_airline %>%
  spread(Repoting_Airline, ArrDelay) %>%
  slice(1:5)

## VISUALIZE AIRLINE CATEGORY
sub_airline %>% # start with data
  mutate(Reporting_Airline = factor(
    Reporting_Airline,
    labels = c("AA", "AS", "DL", "UA", "B6", "PA (1)", "HP", "TW", "VX")
  )) %>%
  ggplot(aes(Reporting_Airline)) +
  stat_count(width = 0.5) +
  labs(x = "Number of data points in each airline")

### QUESTION 4: Create indicator variable to the column of "Month"
sub_airline %>% 
  mutate(dummy = 1) %>%     # column with single value
  spread(
    key = Month,   # column to spread
    value = dummy,
    fill = 0) %>%
  slice(1:5)

### QUESTION 5: Create indicator variable to the column of "Month" by applying departure delay values
sub_airline %>%
  spread(Month, DepDelay) %>%
  slice(1:5)

