## 1: SIMPLE LINEAR REGRESSION MODEL ##
ArrDelayMinutes = 17.35 + 0.7523 * (20)
ArrDelayMinutes

# 1.1: Define the data set as "aa_delay"
aa_delays <- sub_airline %>% filter(CarrierDelay != "NA", Reporting_Airline == "AA")
aa_delays

# 1.2: Fit the data into linear regression model
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)

# 1.3: Summarize the Regression Model
summary(linear_model)

# 1.4: Create a never seen data
new_depdelay <- data.frame(DepDelayMinutes = c(12, 19, 24))
new_depdelay 

# 1.5: Predict the regression model
pred <- predict(linear_model, newdata = new_depdelay, interval = "confidence")
pred


## 2: MULTIPLE LINEAR REGRESSION MODEL ##
mlr <- lm(ArrDelayMinutes ~ CarrierDelay + LateAircraftDelay, data = aa_delays)
summary(mlr)

# 2.1: Create a new data set
CarrierDelay <- c(10, 20, 30)
LateAircraftDelay <- c(20, 60, 30)
new_multidelay <- data.frame(CarrierDelay, LateAircraftDelay)
new_multidelay

# 2.2: Calculate the prediction 
pred <- predict(mlr, newdata = new_multidelay, interval = 'confidence')
pred


## 3: ASSESSING MODELS VISUALLY ##
# Regression Plot #
library(ggplot2)
ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) + geom_point() +
  stat_smooth(method = "lm", col = "red", formula = y ~ x)

# Diagnostic Plot #
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
plot(linear_model)


## 4: POLYNOMIAL REGRESSION ##
time <- 6:19
temp <- c(4, 6, 7, 9, 10, 11, 11.5, 12, 12, 11.5, 11, 10 ,9 ,8)
plot(time, temp)

# Fit the 2nd order polynomial regression
polyfit2 <- lm(temp ~ poly(time, 2, raw = TRUE))

# Print the summary of the model
summary(polyfit2)

# Plot the Line
ggplot(data = NULL, aes(time, temp)) + geom_point() + geom_smooth(method = "lm", 
        
                                                                                                                            formula = y ~ poly(x, 2))
## 5: ASSESSING THE MODEL ##
# Mean Squared Error (MSE)
mse <- mean(linear_model$residuals ^ 2)
mse

# Roote Mean Sqaure Error (RMSE)
rmse <- sqrt(mse)
print(rmse)

# For R-Squared value or Multiple R-Squared 
summary(linear_model)
summary(linear_model)$r.squared


## 6: PREDICTION AND DECISION MAKING ##
# FIRST: fit the model
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)

# SECOND: predict the arrival delays of flight with 12, 19, 24 minutes departure delays
new_depdelay <- data.frame(DepDelayMinutes = c(12, 19,24))
pred <- predict(linear_model, newdata = new_depdelay, interval = "confidence")
print(pred)

# THIRD: simply visualizing our data with a regression
ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red", se = FALSE)

## PLOT THE ACTUAL AND PREDICTED VALUES
# Ad te=he Predicted Values in the Original Data set
aa_delays$predicted <- predict(linear_model)

ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  # Plot the Actual Points
  geom_point() +
  # Plot Regression Line
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  # Add the Predictd Values
  geom_point(aes(y = predicted), color = "green") +
  # Connect tehy Actual Data Points with their Corresponding Predicted Values
  geom_segment(aes(xend = DepDelayMinutes, yend = predicted), alpha = 0.2)
