# Load Libraries and Data
library(tidyverse)

# URL where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# Download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# UNTAR the file so we can get the csv only, if we run this on our local machine, then can remove
# **tar = "internal"**untar("lax_to_jfk.tar.gz")
untar("lax_to_jfk.tar.gz", tar = "internal")

# Read CSV Only
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv", 
                        col_types = cols('DivDistance' = col_number(),
                                         'DivArrDelay' = col_number()))

#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
### 1: Analyzing Individual Feature Patterns using VISUALIZATION 

# Boxplot
ggplot(data = sub_airline, mapping = aes(x = Reporting_Airline, y = ArrDelay)) + 
  geom_boxplot(fill = "bisque", color = "black", alpha = 0.3) + 
  geom_jitter(aes(color = 'blue'), alpha = 0.2) +
  labs(x = 'Airline') +
  ggtitle("Arrival Delays by Airline") +
  guides(color = FALSE) +
  theme_minimal() +
  coord_cartesian(ylim = quantile(sub_airline$ArrDelay, c(0, 0.99)))

# Load Alaska data, deleting rows that have missing departure delay or arrival delay data
alaska_flights <- sub_airline %>%
  filter(Reporting_Airline == "AS") %>%
  filter(!is.na(DepDelay) & !is.na(ArrDelay)) %>%
  filter(DepDelay < 40)

ggplot(data = alaska_flights, mapping = aes(x = DepDelay, y = ArrDelay)) + 
  geom_point() +
  ggtitle("Alaska Flight - Departure Delays vs Arrival Delays")

# List the data types for each column
str(sub_airline)


## QUESTION 1: What is the data type of column "ArrDelayMinutes"?
## NUMERICAL (num)

## QUESTION 2: Find correlation between the following columns: DepDelayMinutes and ArrDelayMinutes?
cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)

# POSITIVE LINEAR RELATIONSHIP: Find the scatter plot of "DepDelayMinutes" and "ArrDelayMinutes" of all airlines. 
# "DepDelayMinutes" as potential predictor variable of "ArrDelayMinutes"
ggplot(data = sub_airline, mapping = aes(x = DepDelayMinutes , y =ArrDelayMinutes)) +
  geom_point() +
  geom_smooth(method = "lm", na.rm = TRUE)

cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)

# WEAK LINEAR RELATIONSHIP: If "WeatherDelay" is a good predictor variable of "ArrDelayMinutes"
ggplot(data = sub_airline, mapping = aes(x = WeatherDelay, y = ArrDelayMinutes)) + 
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)

cor(sub_airline$WeatherDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")

## QUESTION 3(A): Find the correlation between x = "CarrierDelay", y = "ArrDelayMinutes"
cor(sub_airline$CarrierDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")

## QUESTION 3(B): Give correlation results between x = "CarrierDelay", y = "ArrDelayminutes", 
## do we expect linear relationship ?
# There is a decent correlation between the variable "Carrierdelay" and "ArrDelayMinutes" since the correlation
# is somewhat close to 1. We can further demonstrate it using "geom_point" and "geom_smooth".
ggplot(data = sub_airline, mapping = aes(x = CarrierDelay, y = ArrDelayMinutes)) +
  geom_point() +
  geom_smooth(method = "lm", na.rm = TRUE)


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
### 2: Descriptive Statistical Analysis
## We can apply the method "summarize" and "group_by" as follows:
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(count = n(),
            mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE),
            min = min(ArrDelayMinutes, na.rm = TRUE),
            median = median(ArrDelayMinutes, na.rm = TRUE),
            iqr = IQR(ArrDelayMinutes, na.rm = TRUE),
            max = max(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays


## To identify the data type of each column in a data frame
sapply(sub_airline, typeof)


## Value Counts: summarize the categorical data by using count() function
sub_airline %>%
  count(Reporting_Airline)


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
### 3: Basics Of Grouping

avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarise(mean_delays = mean(ArrDelayMinutes), .groups = 'keep')

head(avg_delays)


## Sort the dataframe in R using multiple variables with dplyr
sorted <- avg_delays %>%
  arrange(desc(mean_delays))

head(sorted)


## The color is still hard to see and identify, lets change the color
avg_delays %>%
  ggplot(aes(x = Reporting_Airline,
             y = DayOfWeek,
             fill = mean_delays)) +
  # set the title's borders to be white with size 0.2
  geom_tile(color = "white", size = 0.2) + 
  # define gradient color scales
  scale_fill_gradient(low = "yellow", high = "red")
  

## For something more sophisticated, we can add more blocks in our code
# This visualization will use lubridate package
library(lubridate)

# Let's take a sample average across Reporting_Airline and DayOfWeek
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarise(mean_delays = mean(ArrDelayMinutes), .groups = 'keep') %>%
  # Create a new variable "bins" from mean_delays
  # make the first range -0.1 to 0.1 to include zero values
  mutate(bins = cut(mean_delays, breaks = c(-0.1, 0.1, 10, 20, 30, 50, max(mean_delays)),
                    labels = c("0", "0-10", "10-20", "20-30", "30-50", ">50"))) %>%
  mutate(bins = factor(as.character(bins), levels = rev(levels(bins))))

ggplot(avg_delays, aes(x = Reporting_Airline,
                       y = lubridate::wday(DayOfWeek, label = TRUE),
                       fill = bins)) +
  geom_tile(color = "white", size = 0.2) +
  geom_text(aes(label = round(mean_delays, 3))) +
  guides(fill = guide_legend(title = "Delays Time Scale")) +
  labs(x = "Reporting Airline", y = "Day Of Week", title = "Average Arrival Delays") + 
  # Define color palette for the scale
  scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4"))

  
## QUESTION 4: Use the "GROUPBY()" and "SUMMARIZE" function to find the average "ArrDelayMinutes" of each 
## flight based on "Reporting_Airlines"? 
sub_airline %>%
  group_by(Reporting_Airline)%>%
  summarise(mean_delays = mean(ArrDelayMinutes))


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
### 4: Correlation and Causation
## PEARSON CORRELATION: measures the linear dependence between two variables X and Y
# The coefficient is the value between -1 and 1 inclusive where:
# 1: total positive linear correlation
# 0: no linear correlation, the two variables most likely do not effect each other
# -1: total negative linear correlation
sub_airline %>%
  select(DepDelayMinutes, ArrDelayMinutes) %>%
  cor(method = "pearson")

## P-VALUE: By convention, when the
# p-value is $<$ 0.001: we say there is strong evidence that the correlation is significant.
# p-value is $<$ 0.05: there is moderate evidence that the correlation is significant.
# p-value is $<$ 0.1: there is weak evidence that the correlation is significant.
# p-value is $>$ 0.1: there is no evidence that the correlation is significant.
## In R, to conduct a significance test and get the p-values, you can use `cor.test()`.
sub_airline %>%
  cor.test(~DepDelayMinutes + ArrDelayMinutes, data = .)

## CORRELATION BETWEEN MULTIPLE VARIABLES
correlation <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, 
         LateAircraftDelay) %>%
  cor(use = "pairwise.complete.obs", method = "pearson")

correlation

#install.packages("corrplot")
library(corrplot)
numeric_airline <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay,
         LateAircraftDelay)

airline_cor <- cor(numeric_airline, method = "pearson", use = "pairwise.complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(airline_cor, method = "color", col = col(200),
         type = "upper", order = "hclust",
         addCoef.col = "black", # Add Coefficient of Correlation
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         )


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
### 5: ANOVA (Analysis of Variance)
# In GEOM_BAR(), we should set STAT = "IDENTITY" since we are passing in the Y values ("AVERAGE_DELAYS").
# If we left out this parameter then by default STAT = "COUNT", which would instead count the frequency
# of each airline.
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarise(Average_Delays = mean(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays %>%
  ggplot(aes(x = Reporting_Airline, y = Average_Delays)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Arrival Delays by Airline")
# To analyze categorical variables like "REPORTING_AIRLINE" variable, we can use a method such as the
# ANNOVA method

## The "Analysis of Variance (ANNOVA)" is a statistical method used to test whether there are significant 
# differences between the means of two or more groups. ANNOVA returns two parameters:
## F-TEST SCORE: ANNOVA assumes the means of all groups are the same, calculates how much the actual means
# deviate from the assumption and reports it as the F-Test Score. A larger score means there is a larger 
# difference between the means.
## P-Value: The p-value tells us how statistically significant the calculated score value is.

# In our ARRDELAY variable is strongly correlated with the variable we are analyzing, expect ANOVA to return 
# a sizable F-test score and a small p-value.

## AMERICAN AIRLINES (AA) & ALASKA AIRLINES (AS)
# The ANNOVA test can be performed in base R Stats package using "AOV()" function. We can pass in the ARRIVAL 
# DELAY data of the two airline groups that we want to compare and it calculates the ANOVA results.

## In this first example, we can compare AMERICAN AIRLINE and ALASKA AIRLINE. The results confirm what we guessed
# at first. The flight delay in "AA" and "AS" are not significantly different, as the f-score (0.13) is less 
# than 1  and p-value is larger than than 0.05.
aa_as_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'AS')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(ad_aov)


### AMERICAN AIRLINE (AA) & PAN AM AIRLINE (PA(1))
## As another example, we can compare AMERICAN AIRLINE & PAN AM AIRLINE. From the below output, the arrival 
# delay between "AA" and "PA(1)" are significantly different, since the F-force is very large (F=17.95) and
# the P-value is 0.0000245 which smaller than 0.05. All in all, we can say that there is a STRONG correlation 
# between a categorical variable and other variables, if the ANNOVA TEST gives us a LARGER F-test value and 
# a small P-value.
aa_pa_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'PA (1)')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_pa_subset)
summary(ad_aov)


### CONCLUSION: IMPORTANT VARIABLES

## Continuous Numerical Variables:
# 1. DepDelayMinutes
# 2. CarrierDelay
# 3. LateAircraftDelay

## Categorical Variables:
# 1. ReportingAirlines

## While building machine learning models to automate our analysis, feeding the model with variables that 
# meaningfully affect our target variable will improve our model's prediction performance.

