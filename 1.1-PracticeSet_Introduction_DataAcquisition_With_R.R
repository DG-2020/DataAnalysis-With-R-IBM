
# Load the Packages in R
install.packages("tidyverse")
library(tidyverse)

# URL where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.01/lax_to_jfk.tar.gz"

# Download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# UNTAR the file so we can get the csv only
untar("lax_to_jfk.tar.gz")

# READ_Only
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols("DivDistance" = col_number(),
                                         "DivArrDelay" = col_number()))
# PRINT the data in R
head(sub_airline)
tail(sub_airline)

# EXPORT Data from R
write_csv(sub_airline, "lax_to_jfk.csv")

glimpse(sub_airline)

# SELECT Example
select(sub_airline,
       c(Month, DayOfWeek, Reporting_Airline, CRSDepTime))

# FILTER Example
filter(sub_airline, Reporting_Airline == "AA")

# SELECT & FILTER Example (Use of Pipe Operator ~ %>%)
sub_airline %>%
  select(Month, DayOfWeek, Reporting_Airline, CRSDepTime) %>%
  filter(Reporting_Airline == "AA")

# Return a Statistical Summary of the Data: GROUP_BY/SUMMARIZE workflow example
sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarise(Avg_Delay_Overall = mean(ArrDelay, na.rm = TRUE))

sub_airline %>%
  group_by(Reporting_Airline == "PA (1)") %>%
  summarise(SD_Delay = sd(ArrDelay, na.rm = TRUE))

sub_airline %>%
  group_by(Reporting_Airline == "AS") %>%
  summarise(Median_Delay = median(ArrDelay, na.rm = TRUE))

