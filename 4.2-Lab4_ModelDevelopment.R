## MODEL DEVELOPMENT WITH TIDYVERSE ##
# A model estimator can be thought of as a mathematical equation used to predict a value given one
# or more other values. Relating one or more independent variables or feature to department variables.
# For Example, we input a flight's departure delay as the independent variable or feature, the output
# of the model or dependent variable is the arrival delay. Usually the more relevant data you have the 
# more accurate model is,

# In data analysis, we often use MODEL DEVELOPMENT to help us predict future observation from the data
# we have. A model will help us understand the exact relationship between different variables and how
# these variables are used to predict the result.

## LOAD LIBRARIES AND DATA
# load tidyverse
#install.packages("tidyverse")
load(tidyverse)

# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# Download the file
download.file(url, destfile = "lag_to_jfk.tar.gz")

# If you run this on your location meeting, then can remove tar = "internal"
untar("lag_to_jfk.tar.gz", tar = "internal")

# Read_csv only
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(),
                                         'DivArrDelay' = col_number()))
             
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

### 1: SIMPLE LINEAR REGISTRATION ###

# It is a method to help us understand the relationship between two variables:
  # x -- The predictor/independent variable 
  # y -- The response/dependent variable (that we want to predict)
# The result of Linear Regression is a linear function that predicts the response (dependent)
# variable as a function of the predictor (independent) variable.

# **LINEAR MODEL FUNCTION **  
# y_hat = b_0 + b_1x
# b_0: the INTERCEPT of regression line; the value of Y when X is 0.
# b_1: the SLOPE of the regression line; the value with which Y changes when X increased by 1 unit.
# y_hat: the predicted value from the linear model.

## FIT THE DATA INTO THE MODEL ##
# Define data set with just AA (Alaska Airline) as the Reporting_Airline
aa_delays <- sub_airline %>%
  filter(CarrierDelay != "NA", Reporting_Airline == "AA")
head(aa_delays)

# For this example, we want to look at how Departure_Delay can help us predict Arrival_Delay.
# Using Simple Linear Regression, we create a linear function with "DepDelayMinutes" as the 
# predictor variable and the "ArrDelayMinutes" as the response variable. We can use base R's 
# function lm() to create a linear model.
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)

# The output displays the learned coefficient ("Estimate" in the output) of the model, b_0 and b_1
# as well as other information about the fitted model. 
summary(linear_model) 


## We can output a prediction of three new data points
# Input data we use to predict
new_depdelay <- data.frame(DepDelayMinutes = c(12, 19, 24))

# Predict the data points
pred <- predict(linear_model, newdata = new_depdelay, interval = "confidence")
pred

# When we print the "pred" object, we can see that there are 3 columns, FIT, LWR & UPR. 
# FIT: the prediction results of the input.
# LWR & UPR: the lower and upper bound of the 95% confidence intervals of the prediction results. 
# CONFIDENCE INTERVAL: The uncertainty around the mean predictions.

# For Example: Given that the DepDelayMinutes is 12, then the model predicts the ArrDelayMinutes to
# be 26.38 and we are 95% confident that the interval (21.99, 30.77) captures the true mean 
# arrival delay for this instance.


## WHAT IS THE VALUE OF THE INTERCEPT (b_0) AND THE SLOPE (b_1)?
# Using the fitted model, "linear_model", we can grab the attribute "coefficients" using $. These coefficients 
# corresponds to b_0 (the intercept) and b_1 (the slope and coefficient of DepDealyMinutes). 
linear_model$coefficients


## WHAT ARE THE FINAL ESTIMATED LINEAR MODEL WE GET?
# We should get a final linear model with the structure: y_hat = b_0 + b_1X
# Remember that we are predicting ArrDelayMinutes using DepDelayMinutes. So, plugging in the actual values we get:
# ArrDelayMinutes = 17.35 + 0.7523 * DepDelayMinutes

## QUESTION 1 A): Create a linear function with "CarrierDelay"  as the predictor variable and the 
## "ArrDelayMinutes" as the response variable.
linear_model2 <- lm(ArrDelayMinutes ~ CarrierDelay, data = aa_delays)
linear_model2

## QUESTION 1 B): Find the coefficients (intercept & slope) of the model.
linear_model2$coefficients

## QUESTION 1 C): Write is the equation of the predicted line.  We can use X and Y_hat
## or 'CarrierDelay' or 'ArrDelayMinutes'?
# Using X or Y
y_hat  = 35.12 + 0.7032 * X
ArrDelayMinutes = 35.12 + 0.7032 * CarrierDelay

##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

### 2: MULTIPLE LINEAR REGISTRATION ###
# What if we want to predict arrival delay minutes using more than one variable?
# If we want to use more variables in our model to predict arrival delay minutes, we can use MULTIPLE LINEAR REGRESSION.
# MLR  is very similar to SLR, but this method is used to explain the relationship between one continuous 
# response (dependent) variable and TWO or MORE predictor (independent) variables. Most of the real-world 
# regression models involve multiple predictors.  (here 2 predictor variables used in this example but any amount
# of  predictor variables can be used)

# Y : Response Variable
# X_1: Predictor Variable 1
# X_2: Predictor Variable 2
# Y_hat = b_0 + b_1*X_1 + b_2 * X_2 
# where b_0: intercept; b_1: coefficient of variable 1; b_2: coefficient of variable 2

# The other two good predictors of ArrDelayMinute will be :
#** DepDelayMinutes
#** LateAircraftDelay
## Develop a model using these variables as the predictor variables by fitting the data
mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay, data = aa_delays)
summary(mlr)

## What are the values of the intercept(b_0) and the coefficients(b_1, b_2)?
mlr$coefficients

## What is the final estimated linear model that we got?
# y_hat = b_0 + b_1*X_1 + b_2*X_2

## What is the linear function we get in this example?
# ArrDelayMinutes = 17.32 + 0.76 * DepDelayMinutes - 0.0103 * LateAircraftDelay

## QUESTION 2 A): Create and train a Multiple Linear Regression Model "mlr2" where the response variable is
## ArrDelayMinutes and the predictor variable is "DepDelayMinutes", "LateAircraftDelay" and "CarrierDelay"?
mlr2 <- lm(ArrDelayMinutes ~  DepDelayMinutes + LateAircraftDelay + CarrierDelay, data = aa_delays)
summary(mlr2)

## QUESTION 2 B): Find the Coefficients of the model?
mlr2$coefficients

## QUESTION 2 C):  Using the fitted model "mlr2" what are the predicted values for the following new data points?
DepDelayMinutes <- c(10, 20, 30)
LateAircraftDelay <- c(20, 60, 30)
New_MultiDelay <- data.frame(DepDelayMinutes, LateAircraftDelay)
New_MultiDelay

pred2 <- predict(mlr, newdata = New_MultiDelay, interval = "confidence")
pred2

##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##

### 3: ASSESSING MODELS VISUALLY ###
# Now we have developed some models, how do we evaluate our models and how do we choose the best one? One way 
# to do this is by using VISUALIZATION. 

## REGRESSION PLOT ##
# When it comes to simple linear regression, an excellent way to visualize the fit of our model is using 
# REGRESSION PLOTS.
# REGRESSION PLOTS are a good estimate of:
  # * The relationship between two variables;
  # * The strength of the correlation;
  # * The direction of the relationship (Positive or Negative);
# There are several ways to plot a regression plot ; a simple way is to use "ggplot" from the tidyverse library.
# This plot will show a combination of an scattered data points (a SCATTER PLOT)  as well as the fitted 
# LINEAR REGRESSION line going through the data. This will give us a reasonable estimate of the relationship
# between the two variables, the strength of the correlation, as well as the direction (Positive or Negative 
# Correlation). 

## Lets visualize the DepDelayMinutes and ArrDelayMinutes of "Alaska Airlines" subset dataframe. 
# To visualize a fitted linear model using "ggplot", we can use "geom_smooth()". Additionally, we can use 
# "stat_smooth()" to create the same model. The default model if unspecified is "formula = y ~ x". In this case,
# we will predict -- arrival delay minutes -- using -- departure delay minutes--. So the predictor variable is
# "DepDelayMinutes" and the response variable is "ArrDelayMinutes".
ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

## We can see from this point that "ArrDelayMinutes" is positively correlated with "DepDelayMinutes", since the 
# regression slope is POSITIVE.
# One thing to keep in mind when looking at a regression plot is to pay attention to how scatted the data points 
# are around the regression line. This will give us a good indication of the variance of the data, and whether 
# a linear model would be the best fit or not. If the data is too far off from the line, this linear model,
# might not be the best model for this data.   

# QUESTION 3 A): Create a Regression Plot of "CarrierDelay" and "ArrDelayMinutes" using "aa_delays" dataset.
ggplot(aa_delays, aes(x = CarrierDelay, y = ArrDelayMinutes)) +
  geom_point() +
  stat_smooth(method = "lm", col = "purple")

# QUESTION 3 B): Given the regression plots above is "DepDelayMinutes" or "CarrierDelay" more strongly correlated 
# with "ArrDelayMinutes". Use the method "cor()" to verify our answer.
cor(aa_delays$DepDelayMinutes, aa_delays$ArrDelayMinutes)
cor(aa_delays$CarrierDelay, aa_delays$ArrDelayMinutes)
# The variable "DepDelayMinutes" has a stronger correlation with "ArrDelayMinutes" at 0.87, compared to 
# "CarrierDelay" which is 0.62.


## RESIDUAL PLOT ##
# A good way to visualize the variance of the data is to use a residual plot.  

## WHAT IS A RESIDUAL?
# The difference between the observed value (Y) and the predicted value (Y_hat) is called the residual (or error).
# When we look at a regression plot, the residual is the distance from the data point to the fitted regression line.

## WHAT IS A RESIDUAL PLOT?
# A residual plot is a graph that shows the residuals on the vertical y-axis and the independent variable on 
# the horizontal x-axis. 

## WHATDO WE PAY ATTENTION TO WHEN LOOKING A RESIDUAL PLOT?
# HOMOSCEDASTICITY: If the residual plot is homoscedastic, then the points in the plot are RANDOMLY SPREAD OUT 
# AROUND THE X-AXIS, which means, that a LINEAR MODEL IS APPROPRIATE for the data. WHY IS THAT? Randomly spread 
# out residuals means that the variance is constant, and thus the linear model is a good fit for this data.

## Let's  visualize the residuals on the plot (Response = ArrDelayminutes & Predictor = DepDelayMinutes)
# -- RED LINE: The Regression line
# -- BLACK DOTS: The Observed Values of ArrDelayMinutes
# -- WHITE DOTS: The Predicted Values from the linear Regression Model
# -- LIGHT GRAY LINES: The Residuals or Errors. It shows how far the Observed Values are from the 
# -- Predicted Values. So a Longer Lines means a Larger Error.

aa_delays <- sub_airline %>% 
  filter(CarrierDelay != "NA", Reporting_Airline == "AA")

score_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)

aa_delays$predicted <- predict(score_model) 

ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Plot Regression Slope
  geom_segment(aes(xend = DepDelayMinutes, yend = predicted), alpha = 0.2) + # Alpha to fade lines
  geom_point() + 
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look

## Now we can create a residual plot, which graphs the residual (light grey lines) against the observed 
# DepDelayMinutes. The code to do this is similar to a normal scatter plot, but we pass in the linear 
# model "lm(ArrDelayMinutes ~ DepDelayMinutes)" and when setting the y-axis we can use, ".resid" which 
# will use the residuals form the model we inputted.

## We can see from this residual plot that the residuals not randomly spread around x-axis which leads us 
# to be believe that may be a non-linear model is more approp[rioated for this data.
ggplot(lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)) + 
  geom_point(aes(x = DepDelayMinutes, y = .resid))

## OTHER DIAGNOSTIC PLOSTS ##
# In addition to residual plots, there are other useful plots. A simple way to view diagnostic plots is to 
# first create the linear model using "lm()", then call the base R's "plot()" function on the model. The 
# below code will output FOUR graphs:
# 1. RESIDUAL PLOT: Identical to the graph we made with ggplot, here it again shows that the residuals 
#                   are not randomly spread around X-axis.

# 2. Q-Q PLOT: The dotted diagonal line represents what normally distributed error (residual) values 
#             would follow. In this case, the residuals do not look normally distributed since there are
#             many observations that fall above the line on the right side.

# 3. SCALE-LOCATION PLOT: This plot helps check the homoscedasticity assumption. Here, it shows a red 
#                         line that is not straight and validates the homoscedasticity assumption is not 
#                         satisfied.


# 4. RESIDUAL VS LEVERAGE PLOT: This helps determine "INFLUENTIAL POINTS". Any points outside the dotted
#                               lines (Cook's Distance) would make it influential. Here, none of the 
#                               points cross the lines, however several points come close and could be 
#                               removed or analyzed further.

linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
plot(linear_model)
 

##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
### 4: POLYNIMIAL REGRESSION ###

## "POLYNOMIAL REGRESSION" is particular case of the general linear regression model or multiple linear 
# regression models. That is, although the data is nonlinear in polynomial regression (the Predictor 
# variables have higher order terms), the model in all cases is LINEAR. The model is always LINEAR
# because it predicts the coefficients (b_0, b_1,...) which are always of order 1.

## There are different orders of polynomial regression:
# $$ Y = b_0 + b_1X + b_2X^2 $$     QUADRATIC - 2ND ORDER
# $$ Y = b_0 + b_1X + b_2X^2 + b_3X^3 $$     CUBIC - 3RD ORDER
# $$ Y = b_0 + b_1X + b_2X^2 + b_3X^3 ... + b_nX^n $$     HIGHER($ n^{th} $) ORDER

## Here we want to create random -predictor- variable 'q' and random -response- variable 'y' that follows
#   a 3rd order polynomial but then we add some random noise to it to get "noise.y". We set the seed so 
#   that this result can be reproduced.

set.seed(20)
x <- seq(from = 0, to = 20, by = 0.1)

# Value of predict (y):
y <- 500 + 0.4 * (x - 10)^3

# Some noise is generated and added to the real signal (y):
noise <- rnorm(length(x), mean = 10, sd = 80)
noise.y <- y + noise

# We fit a first order linear model to this example data set and can see that the model does not fit the 
# data very well.
# Fit Linear Model
ggplot(data = NULL, aes(x, noise.y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# Instead we can use a polynomial model. It is similar to the first order linear model except that we 
# include "poly()" within "geom_smooth()" to indicate what order polynomial to use. For Example, using 
# "poly(x, 5)" equates to having "b_0 + b_1X^2 + b_2X^2 + b_3X^3 + b_4X^4 + b_5X^5".
ggplot(data = NULL, aes(x, noise.y)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 5))

# We can already see from plotting that this "polynomial model" performs better than the "linear model". 
# This is because the generated polynomial function "hits" more of the data points.


## POLYNOMIAL 2ND ORDER ##
# This time using 2nd order polynomial. Again, we use a toy data set where time is the predictor and
# temp is the response. 
time <- 6:19
temp <- c(4, 6, 7, 9, 10, 11, 11.5, 12, 12, 11.5, 11, 10, 9, 8)

ggplot(data = NULL, aes(time, temp)) + 
  geom_point()

# We can create a model like how we saw before using 'lm()' and to include higher order, we can used 
# 'poly()'. for this data set, we try a 2nd order polynomial model to see how it fits. The equation to
# model as follows:   'temp = b_0 + b_1*time + b_2*time^2'
polyfit2 <- lm(temp ~ poly(time, 2, raw = TRUE))
summary(polyfit2)

# From the summary output of the model, we can find the coefficients, so to predict temp, we could use:
# temp = -13.71 + 3.76 * time - -0.1384 * time^2
# Like for the first order linear models, we can use 'ggplot' to graph the model.
ggplot(data = NULL, aes(time, temp)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2))

# QUESTION 4 A): Create a 4th order polynomial model with the variables time and temp from above and 
# display the summary of the model.
polyfit4 <- lm(temp ~ poly(time, 4, raw = TRUE))
summary(polyfit4)

# QUESTION 4 B): Using the predicted coefficients from the summary output for the 4th order model,
# Write down the model equation.
temp = 0.958 -1.683 * time + 0.577 * time^2 + -0.0397 * time^3 +0.0008 * time^4


##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
### 5: ASSESSING THE MODEL ###
# When evaluating our models, not only do we want to visualize the results but we also want a
# quantitative measure to determine how accurate the model is. Two very important measures that are often
# used in Statistics to determine the accuracy of a model are:

## 1: R^2 / R-Squared -- It is also known as the "coefficient of determination", is a measure to indicate
## how close the data is to fitted regression line. The value of the R-Squared is the "percentage of 
## variation of response variable (y)" that is explained by a linear model. 

## 2: Mean Squared Error(MSE) --        MSE = Average((y_hat - y)^2) & RMSE = SQRT of MSE
## The mean squared error measures the average of the squares of error, that is, the difference between 
## "actual value(y)" and the "estimated value(y_hat)". Another metric that is related to MSE is 
## "ROOT MEAN SQAURED ERROR (RMSE)" and is simply SQRT of MSE.


### MODEL 1: SIMPLE LINEAR REGRESSION ###
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, aa_delays)

# Using this model, we can calculate MSE and RMSE.
mse <- mean(linear_model$residuals^2)
mse

rmse <- sqrt(mse)
rmse

## R^2 can be obtained from the summary of the model. From the output below, we can say that 
# approximately 75.9% of the variation of price is explain by this simple linear model.
summary(linear_model)$r.squared


### MODEL 2: MULTIPLE LINEAR REGRESSION ###
mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay, data = aa_delays)

mse_mlr <- mean(mlr$residuals^2)
mse_mlr

rmse_mlr <- sqrt(mse_mlr)
rmse_mlr

# From the R-Squared value below, we can say that approximately 75.9 % of the variation of ArrDelayMinutes
# is explained by the multiple linear regression "MLR".
summary(mlr)$r.squared


### MODEL 3: POLYNOMIAL REGRESSION ###
poly_reg <- lm(ArrDelayMinutes ~ poly(DepDelayMinutes, 3), data = aa_delays)

# Similar to model 1 and model 2, we can find MSE, RMSE and R^2.
mse_poly <- mean(poly_reg$residuals^2)
mse_poly

rmse_poly <- sqrt(mse_poly)
rmse_poly

summary(poly_reg)$r.squared



##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
### 5: ASSESSING THE MODEL ###

## PREDICTION
# Previously, we trained model using the method "lm()" and we used the method "predict()" to produce 
# prediction.
# For example we want to predict the score model we created in a previous section
head(predict(score_model))

## DECISION MAKING: DETERMINING A GOOD MODEL FIT
# Now that we have visualize the different models, and generated the R-Squared and MSE values for the fits,
# how do we determine a good model fit?

## What is a good R-SQUARED value?
  #* When comparing models, the model with the **higher R-Squared** value is a better fit for the data.

## What is a good MSE?
  #* When comparing models, the model with the **smallest MSE** value is a better fit for the data,

## SIMPLE LINEAR REGRESSION: Using "DepDelayMinutes" as a PREDICTOR variable of "ArrDelayMinutes"
  #* R-Squared: 0.7588
  #* MSE: 394.064

## MULTIPLE LINEAR REGRESSION: Using "DepDelayMinutes" and "LateAircraftDelay" as PREDICTOR variable of 
## "ArrDelayMinutes"      
  #* R-Squared: 0.7588
  #* MSE: 394.011

## POLYNOMIAL FIR: Using 3rd order polynomial of "DepDelayMinutes" as a PREDICTOR variable of 
# "ArrDelayMinutes"
  #* R-Squared: 0.7986
  #* MSE: 328.970


## SIMPLE LINEAR REGRESSION MODEL (SLR) vs. MULTIPLE LINEAR REGRESSION MODEL (MLR) ##
# Usually, the more variables we have, the better our model is at predicting, but it is not always TRUE.
# Sometimes we may not have enough data , we may run  into numerical problems, or many of the variables 
# may not be useful and or even act as noise. As a result, we should always check the MSE and R^2.

# So to be able to compare the results of the MLR vs SLR models, we look at a combination of both the 
# R-Squared and MSE to make the best conclusion about the fit of the model.

#* MSE: The MSE of SLR model is 394.06 while MLR has an MSE of 394.01. The MSE of MLR model is 
    #* slightly SMALLER. 
  
#* R-SQUARED: In this case, we can see that the R-Squared for the SLR is slightly LOWER than the 
    #* R-Squared for the MLR model.

# This R-Squared in combination with the MSE show that MLR seems like a slightly BETTER MODEL fit in this
# case, compared to SLR. However, we could try adding more PREDICTOR VARIABLES in the MLR model to see if
# that made a bigger improvement since in our example only two were used.


## SIMPLE LINEAR REGRESSION MODEL (SLR) vs. POLYNOMIAL FIT ##

#* MSE: We see that Polynomial model brought down the MSE, since this MSE (328.97) is SMALLER than the one from 
#* the SLR (394.06).
#* R-SQUARED: The R-Squared for the Polyfit (0.7986) is LARGER for SLR (0.7588), so the Polynomial Fit 
#* also BROUGHT UP R-Squared quite a bit.

# Since the Polynomial Fit resulted in a LOWER MSE and a HIGHER R-Squared, we conclude that this was a
# better fit model than the simple linear regression for predicting "ArrDelayMinutes".


## MULTIPLE LINEAR REGRESSION MODEL (MLR) vs. Polynomial Fit ##

#* MSE: The MSE for the polynomial model (328.97) is SMALLER than the MSE for the MLR model (394.11).

#* R-SQUARED: The R-Squared for the polynomial model (0.7986) is also LARGER than the MLR model's (0.7588).


### CONCLUSION ###
# Comparing these three models, the MLR model performs slightly BETTER than the SLR model. Perhaps, if we
# tried adding some more PREDICTOR VARIABLES, the MLR model could do even better. Of the three models,
# we conclude that the "POLYNOMIAL OF ORDER 3 MODEL" seems  to be the best fit it as it has the HIGHEST
# R^2 and the lower SLR.

# As a bonus, we can try using more PREDICTOR VARIABLES and different order POLYNOMIALS to perhaps find
# even better results.