##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##

## 1: DATA ACQUISITION
install.packages("tidyverse")
library(tidyverse)

## READ DATA
url <-
  "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# Download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# Untar the file so we can get the csv only
untar("lax_to_jfk.tar.gz", tar = "internal")

sub_airline <- read_csv(
  "lax_to_jfk/lax_to_jfk.csv",
  col_types = cols('DivDistance' = col_number(),
                   'DivArrDelay' = col_number())
)

# Show the first n = 3 rows
head(sub_airline, 3)

# Show the first 6 rows as default
head(sub_airline)

# Show the last 6 rows as default
tail(sub_airline)

# Retrieves the column names of the dataframe
colnames(sub_airline)

# Retrieves the dimension (number of rows and columns) of the dataframe
dim(sub_airline)

## Question #1: Check the last 10 rows of data frame: "SUB_AIRLINE"
tail(sub_airline, 10)


### PREPROCESS DATA ###
# URL where the data is located
url <-
  "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/airline_2m.tar.gz"

# Download the file
download.file(url, destfile = "airline_2m.tar.gz")

# UNTAR the file so we can get the csv only
untar("airline_2m.tar.gz")

# Read_CSV only
airlines <- read_csv(
  "airline_2m.csv",
  col_types = cols(
    'DivDistance' = col_number(),
    'Div1Airport' = col_character(),
    'Div1AirportID' = col_character(),
    'Div1AirportSeqID' = col_character(),
    'Div1WheelsOn' = col_character(),
    'Div1TotalGTime' = col_number(),
    'Div1LongestGTime' = col_number(),
    'DivReachedDest' = col_number(),
    'DivActualElapsedTime' = col_number(),
    'DivArrDelay' = col_number(),
    'Div1WheelsOff' = col_character(),
    'Div1TailNum' = col_character(),
    'Div2Airport' = col_character(),
    'Div2AirportID' = col_character(),
    'Div2AirportSeqID' = col_character(),
    'Div2WheelsOn' = col_character(),
    'Div2TotalGTime' = col_number(),
    'Div2LongestGTime' = col_number(),
    'Div2WheelsOff' = col_character(),
    'Div2TailNum' = col_character()
  )
)

## As we are going to be focusing on flights from LAX to JFK and we will exclude flights that got cancelled or diverted, we are also going to
## get only useful columns

sub_airline <- airlines %>%
  filter(Origin == "LAX", Dest == "JFK", Cancelled != 1, Diverted != 1) %>%
  select(
    Month,
    DayOfWeek,
    FlightDate,
    Reporting_Airline,
    Origin,
    Dest,
    CRSDepTime,
    CRSArrTime,
    DepTime,
    ArrTime,
    ArrDelay,
    ArrDelayMinutes,
    CarrierDelay,
    WeatherDelay,
    NASDelay,
    SecurityDelay,
    LateAircraftDelay,
    DepDelay,
    DepDelayMinutes,
    DivDistance,
    DivArrDelay
  )

dim(sub_airline)

## Question #2: Find the name of the columns of the dataframe
colnames(sub_airline)

### SAVE DATASET ###
write_csv(sub_airline, "lax_to_jfk.csv")

##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##----##

## 2. BASIC INSIGHTS OF THE DATASET
# Data Types (data type of each column)
sapply(sub_airline, typeof)

# 2.1: Numeric and Integer Types
x = 10.5
class(x)

k = 1
class(k)

y = as.integer(3)
class(y)

# 2.2: Complex Type
z = 0i
class(z)

# 2.3: Logical Type
logical_values = c(TRUE, T, FALSE, F)
class(logical_values)

# 2.4: Character Type (a character object is used to represent string values in R)
character_value = "This Is A CHARACTER!"
print(character_value)
class(character_value)

## PIPE OPERATOR ##
# An advantage of using pipe is it replaces having messy nested functions that can be difficult to interpret like:
summarise(
  group_by(filter(sub_airline, Month == 1), Reporting_Airline),
  avg_carrier_delay = mean(CarrierDelay, na.rm = TRUE)
)

# The same code can be written using pipe and makes it much easier to understand
sub_airline %>%
  filter(Month == 2) %>%
  group_by(Reporting_Airline) %>%
  summarise(Avg_Carrier_Delay_2 = mean(CarrierDelay, na.rm = TRUE))

## SUMMARIZE ##
# Group_By/ Summarize workflow example
sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(Avg_Carrier_Delay_Overall = mean(CarrierDelay, na.rm = TRUE))

# Use SD instead of Mean
sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(SD_Carrier_Delay_Overall = sd(CarrierDelay, na.rm = TRUE))


## Question #3: Use "sub_airline", get the mean of "arrdelay" for each "reporting_airline". In other words, group_by reporting_airline" and 
## summarize the mean of "arrdelay" of each reporting airline. Remember to use na.rm = TRUE.
sub_airline %>% 
  group_by(Reporting_Airline) %>%
  summarise(Avg_Arrival_Delay_Overall = mean(ArrDelay, na.rm = TRUE))

# to get concise summary of complete dataframe
glimpse(sub_airline)


