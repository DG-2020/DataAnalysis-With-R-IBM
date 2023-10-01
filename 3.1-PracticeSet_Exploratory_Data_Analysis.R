### 1: DESCRIPTIVE STATISTICS: SUMMARIZE()
summary_airline_delays <- sub_airline %>% 
  group_by(Reporting_Airline) %>%
  summarise(mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE))
head(summary_airline_delays)


### 2. GROUP_BY() EXAMPLE
## 2.1: Create a simple average across Reporting_Airline and DayofWeek
avg_delays <- sub_airline %>% 
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarise(mean_delays = mean(ArrDelayMinutes))
print(avg_delays)

## 2.2: Sort Data in Descending Order
# sort the dataframe in R using multiple variables with dplyr
avg_delays %>%
  arrange(desc(mean_delays))

## 2.3: Plot target variable over multiple variables (change the color)
avg_delays %>%
  ggplot(aes(x = Reporting_Airline, y = DayOfWeek, fill = mean_delays)) +
  # set the tile's border to be white with size 0.2
  geom_tile(color = "grey", linewidth = 0.50) + 
  # define the gradient color scales
  scale_fill_gradient(low = "yellow", high = "red")


### 3. Analysis of Variance [ANOVA] in R
## 3.1.  ANNOVA between 'AA' and  'AS'
aa_as_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'AS')
print(aa_as_subset)

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(ad_aov)

## 3.2.  ANNOVA between 'AA' and  'PA(1)'
aa_pa_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'PA(1)')
print(aa_pa_subset)

ad_aov_2 <- aov(ArrDelay ~ Reporting_Airline, data = aa_pa_subset)
summary(ad_aov_2)



### 4. CORRELATION IN R 
## 4.1. Positive Linear Relationship
# Correlation Between ArrDelayMinutes and DepDelayMinutes
ggplot(sub_airline,
       aes(DepDelayMinutes, 
           ArrDelayMinutes)) +
  geom_point() +
  geom_smooth(method = "lm")

# WEAK Correlation Between ArrDelayMinutes and WeatherDelay
ggplot(sub_airline, 
        aes(WeatherDelay, 
            ArrDelayMinutes)) + 
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm",
              na.rm = TRUE)


## 4.2. Negative Linear Relationship
# WEAK Correlation Between CarrierDelay and LateAircraftDelay
ggplot(sub_airline,
       aes(CarrierDelay,
           LateAircraftDelay)) + 
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm",
              na.rm = TRUE)

### 5. CORRELATION - STATISTICS (PEARSON CORRELATION)
## 5.1. Strong Correlation
sub_airline %>%
  select(DepDelay, ArrDelay) %>%
  cor(method = "pearson")

sub_airline %>%
  cor.test(~DepDelay + ArrDelay, data = .)

## 5.2. Correlation Matrix
library(Hmisc)
numeric_airline <- sub_airline %>%
  select(
    ArrDelayMinutes,
    DepDelayMinutes,
    CarrierDelay,
    WeatherDelay,
    NASDelay,
    SecurityDelay,
    LateAircraftDelay
  )

airline_cor <- rcorr(as.matrix(numeric_airline),
                     type = 'pearson')
head(airline_cor)

