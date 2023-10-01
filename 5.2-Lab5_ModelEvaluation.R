#install.packages("rlang")
#install.packages("tidymodels")
#install.packages("readr")

library(rlang)
library(tidymodels)
library(readr)

### LOAD DATA ###
# URL where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# Download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# UNTAR the file so we can get the csv only (for local machine: remove --tar = "internal"--)
untar("lax_to_jfk.tar.gz")

# READ_CSV only
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))

# Define data set with AA(Alaska Airways) as the Reporting_Airline
aa_delays <- sub_airline %>% filter(CarrierDelay != "NA", Reporting_Airline == "AA")
head(aa_delays)


### 1. MODEL EVALUATION ###

## 1.1: TRAINING & TESTING DATA ##
# An important step is testing our model is to split our data into training and testing data. The training
# data will be used to train (fit) models, while the testing data will not be touched until we are 
# evaluating the model.  Using other packages or programming languages may require to separate out the 
# response variable (ArrDelayMinutes) into another dataframe, but here that is not necessary. The response
# and predictor variables can all stay in one dataframe.

# 1.1.1: Use the principles learned previously and use "REPLACE_NA()" to replace the NAs in the variables
# we are using to predict. Here, we choose to replace the values with 0 because having NA in these
# variables mean that there is no delay.

# 1.1.2: Use "SELECT()"  to only include the variables we will use to create a final model.
flight_delays <- sub_airline %>%
  replace_na(list(CarrierDelay = 0,
                  WeatherDelay = 0,
                  NASDelay = 0,
                  SecurityDelay = 0,
                  LateAircraftDelay = 0)) %>%
  select(c(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, 
           LateAircraftDelay, DayOfWeek, Month))

set.seed(1234)
flight_split <- initial_split(flight_delays)
train_data <- training(flight_split) 
test_data <- testing(flight_split) 

## In INITIAL_SPLIT(), we can also set the "prop" parameter to set the proportion of the data to use
# for training. If it is unspecified like here in the example, then by default it set to 0.75. This means 
# that the proportion of data that is split into the training data is 75% (so testing data is 25%).

## QUESTION 1: ##
# Use the function "initial_split" to split up the data set such that 80% of the data samples will utilize 
# for training. The output of the function should be the following: "flight_split2", "train_data2",
# "test_data2".

set.seed(1234)
flight_split2 <- initial_split(flight_delays, prop = 0.8)
train_data2 <- training(flight_split2)
test_data2 <- testing(flight_split2)


### 1.2: TRAINING A MODEL ###
# After splitting the dataset, the next step is to create a Linear Regression object by using "linear_reg()"
# to specify which package is used to create the model.

# Pick Linear Regression
lm_spec <- linear_reg() %>%
  # Set Engine
  set_engine(engine = "lm") 
print(lm_spec)

# In this example, we will use "ArrDelayMinutes" as the response variable and "DepDelayMinutes" as the 
# predictor variable to fit (train) a model. We will use "train_data" because we are training the model.
# The "test_data" will be used later.

# Use "fit()" to fit the model we just specified in "lm_spec". The output is the fitted (trained) model.
train_fit <- lm_spec %>%
  fit(ArrDelayMinutes ~ DepDelayMinutes, data = train_data)
print(train_fit)

# To look at some of the predictions of the fitted model, use "predict()", which will output one column 
# with predictions (.pred). Here, since "new_data = train_data", we are looking at how will the model is 
# predicting the original training data.
train_results <- train_fit %>%
  # Make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  # Create a new column to save the true values
  mutate(truth = train_data$ArrDelayMinutes)

head(train_results)

# Additionally, we can use the same fitted model to predict on test data and save the dataset called
# "test_results". There are two columns in the dataset, including both predicted values and true values.

# Now it is time to evaluate the models to estimate how well the models perform on new data, the TEST DATA.
# This example uses the same model in "train_fit" to make the predictions. Again, from "predict()", the 
# output is stored in a data frame with only one column, ".pred". We can then add a new column to this 
# data frame using the "mutate()" function. This new column is named "truth" and contains values of 
# "ArrDelayMinutes" from the "test_data". In the end, we will have a dataframe with the predictions and 
# the true values. 
test_results <- train_fit %>% 
  # Make th predictions and save the predicted values
  predict(new_data = test_data) %>%
  # Create a new column to save the true values
  mutate(truth = test_data$ArrDelayMinutes)

head(test_results)  


### 1.3: EVALUATING THE MODEL ###
# Using metrics like RMSE or R^2 are good ways to evaluate REGRESSION models. Calculate RMSE with
# combinations of functions like "MEAN()" and "SQRT()", which is a good exercise. However, in practice,
# this may not be ideal. So more conveniently with "TIDYMODELS", there are already functions like "RMSE()"
# as well as many other metric functions. 
rmse(train_results, truth = truth, estimate = .pred)
rsq(train_results, truth = truth, estimate = .pred)

rmse(test_results, truth = truth, estimate = .pred)
rsq(test_results, truth = truth, estimate = .pred)

# We can also make a plot to visualize how well we predicted the "ArrDelayMinutes".

# This example plots the actual values (the true values of ArrDelayMinutes) versus the model predictions 
# for both the TESTING and TRAINING datasets. It also plots the line "y = x" through the origin. This 
# line is a visual representation of the perfect model where all predicted values are equal to the true
# values in the test set. The farther the points are from the line, the worse the model fit.

# Break Down the Code:-
# 1. MUITATE:- add column called TRAIN to 'test_results' and set the values all to 'TESTING'.
# 2. BIND_ROWS:- do the same to 'train_results' and bind these rows the 'TEST_RESULTS'.
# 3. GGPLOT:- plot the truth vs. prediction values
# 4. GEOM_ABLINE:- add the "y = x" line
# 5. GEOM_POINT:- add the truth vs. prediction points to the plot
# 6. FACET_WRAP:- since 'TRAIN' contains two values - "TESTING" and "TRAINING", this splits the data into
#                 two  graphs.
# 7. LABS:- add labels

test_results %>%
  mutate(train = "testing") %>%
  bind_rows(train_results %>% mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "orange",
              size = 1.5) + 
  geom_point(color = "#006EA1", 
             alpha = 0.5) + 
  facet_wrap(~ train) +
  labs(x = "Truth", 
       y = "Predicted Arrival Delays (Mins.)")

## QUESTION 2: ##
# Using "ArrDelayMinutes" as the response variable and "DepDelayMinutes" as the PREDICTOR variable, find 
# the R^2 on the test data using 80% of the data for training data.
train_fit2 <- lm_spec %>%
  fit(ArrDelayMinutes ~ DepDelayMinutes,
      data = train_data2)

test_results2 <- train_fit2 %>% 
  predict(new_data = test_data2) %>% # Make the predictions and save the predicted values
  mutate(truth = test_data2$ArrDelayMinutes) # Create a new column to save the true values

rsq(test_results2,
    truth = truth, 
    estimate = .pred)

## Sometimes we may not have large enough testing data, as a result, we perform CROSS VALIDATION. ##


### 1.4: CROSS VALIDATION ###
# One of the most common "out-of-sample" evaluation techniques is CROSS VALIDATION.
# Cross Validation is an effective use of data because each observation is used for both TRAINING and 
# TESTING. In Cross Validation :-
# 1. First, the dataset is split into "k-equal" groups; each group is referred to as a fold.
# 2. k-1 of the folds are used to train a model, and the remaining fold is used to test with an 
# evaluation metric.
# 3. This is "repeated" until each of the k-groups is used as the 'test set'.
# 4. After all folds are used, there are 'k' evaluation metric results. They are 'averaged' to get an 
# estimate of out-of-sample error.

# For Example, in 4-fold cross validation we would use three folds for TRAINING and then use one fold 
# of TESTING. The same model would be trained and then tested 4 times using evaluation metric. The 
# evaluation metric that we use depends on the model, we use RMASE and R-Squared in our code example.  


## WHY IS IT WORTH THE EFFORT TO PERFORM CROSS VALIDATION ? ##
# Using cross validation means that a model is trained and elevated many (k) times, however it is still 
# worth the computational cost because it is used to test the generalizability of the model. 
# Generalizability is a measure of how useful the results of a study are for a broader group of people 
# and situations. 

# As we train a model on the training set, it tends to over fit most of the time. To avoid this situation
# we can use regularization techniques. Cross Validation provides a check on how the model is performing 
# on a test data (new unseen data) and since we have limited training instances, we need to be careful 
# while reducing the amount of training samples and reserving it for testing purposes.

# Moreover, cross validation still works well with a "small amount of data". For Example, assume that we
# only have 100 samples. If we do a train test with an 80 to 20 percent split, then we only have 20 samples 
# in the test set, which is too small to generalize reliable results. With cross validation, we can have
# k-folds, so we can build k different models. In this case, we can make predictions on all our data and 
# then average out the model performance. 


## CODE EXAMPLE ##
# To perform cross validation, we can use "vfold_cv()". Setting "v = 10" means that will use 10 folds. 
# The function "fit_resamples()" will keep refitting the model specified on the samples by the cross 
# validation object.
set.seed(1234)
cv_folds <- vfold_cv(train_data, v = 10)
results <- fit_resamples(lm_spec, 
                         ArrDelayMinutes ~ DepDelayMinutes, 
                         resamples = cv_folds)

# We can calculate the "average" RMSE and "R-Squared" of our estimate:
results %>% collect_metrics()
  
## QUESTION 3: ##
# Calculate the average RMSE and R-Squared using three folds utilizing DepDelayMinutes as a feature:
cv_folds3 <- vfold_cv(train_data, v = 3) 
results <- fit_resamples(lm_spec, ArrDelayMinutes ~ DepDelayMinutes, resamples = cv_folds3)
results %>% collect_metrics() 



### 2. OVERFITTING, UNDERFITTING & MODEL SELECTION ###
# It turns out that the test data sometimes referred to as the out the sample data is a much better 
# measure of how well our model performs in the real world. One reason is UNDERFITTING. A model that is
# under fit will have "high training" and "high testing" error.

## HOW TO PREVENT UNDERFITTING? ##
# Increase the model complexity; 
# Try different models;

# An example of underfitting using as simple dataset included in R called 'CARS'. We will predict the 
# distance (dist) it takes for cars to stop using the car's speed (speed). In the first example model, the
# model is defined a line set to the car's stopping distance. Based on the plot, this model is 
# underfitting because of the speeds less than 10 and greater than 20 are very far from the prediction 
# line.
ggplot(cars, aes(x = speed, y =- dist)) +
  geom_point() +
  geom_hline(yintercept = mean(cars$dist), col = "purple")

# Another reason that using the test data to measure the performance of the model is because of OVERFITTING.
# These difference are more apparent in Multiple Linear Regression and Polynomial Regression.


## HOW TO PREVENT OVERFITTING ##
#* Reduce Model Complexity
#* Training with More Data
#* Cross-Validation
#* Regularization

# In this example we use 8th degree polynomial here with "POLY(X, 8)" to fit the "CARS" dataset.
ggplot(cars, aes(x = speed , y = dist)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 8), col = "maroon", se = FALSE)

# The model is fitting to the points in the top right. If this model received new speeds, it may not be
# able to predict accurate distances.

# Going back to example with the "CARS" dataset, we can reduce the complexity of the model. In the 
# previous overfitting example, a polynomial model of 8 degrees was used. Instead, we can use a 
# polynomial of degree 1 or a simple linear regression model. In R, we can set formula to "y over x". In
# this example, we demonstrated how we can prevent overfitting and underfitting models by changing the 
# model complexity.
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, col = "magenta", se = FALSE)



### 3. REGULARIZATION ###
# Regularization is a way to handle the problem of OVERFITTING. It is a technique we can use to reduce 
# the complexity of the model by adding a penalty on the different parameters of the model. After it is 
# applied, the model will be less likely to fit the noise of the training data and will improve the 
# generalization abilities of the model. So, regularization is a way of "avoiding overfitting" by 
# restricting the magnitude of models coefficient.

# There are a few methods of regularizing linear models including
#* Ridge (L2) Regularization
#* Lasso (L1) Regularization
#* Elastic Net (mix of L1 and L2) Regularization

## 3.1: Ridge (L2) Regularization ##
# First, create a "recipe()" that includes the model formula. We could prepares the data more in this 
# step, but the data here is already pre-processed. The dot (.) in the formula is a special character 
# that tells R to use all the variables in "train_data".
flight_recipe <- recipe(ArrDelayMinutes ~ ., data = train_data)

# Next, use the "linear_reg()" function from the tidymodels library to specify the model. "PENALTY" is
# the value of "lambda". "mixture" is the proportion of L1 penalty is used. For Lasso regression, we
# would use "mixture = 1".
ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>%
  set_engine("glmnet")

# Now, create a workflow object so we can conveniently combine pre-processing, modelling and 
# post-processing requests. 
ridge_wf <- workflow() %>%
  add_recipe(flight_recipe)

# Finally add the ridge model and fit the model
ridge_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)

# To view the results of the fitted ridge regression model , use the "PULL_WORKFKOW_FIT()" function.
ridge_fit %>%
  pull_workflow_fit() %>%
  tidy()

# There are two result columns . The estimate column contains the **estimate of the coefficients** learned
# by the model. Penalty contains the value of lambda, which in this example is "0.1".


## 3.2: Lasso (L1) Regularization ##
lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")

lasso_wf <- workflow() %>%
  add_recipe(flight_recipe)

lasso_fit <- lasso_wf %>%
  add_model(lasso_spec) %>%
  fit(data = train_data)

print(lasso_fit)


## 3.3: Elastic Net (L1 and L2) Regularization ##
# Moreover, here is the code for elastic net regularizations. Like mentioned before, "mixture" is the 
# proportion of L1 penalty used. Since elastic net uses a combination of L1 and L2 regularization, then
# when "mixture" is set to a value between 0 and 1 (not including 0 and 1) then it is considered elastic 
# net regularization. In this example, it uses less L1 penalty than L2.
elasticnet_spec <- linear_reg(penalty = 0.1, mixture = 0.3) %>%
  set_engine("glmnet")

elasticnet_wf <- workflow() %>%
  add_recipe(flight_recipe)

elasticnet_fit <- elasticnet_wf %>%
  add_model(elasticnet_spec) %>%
  fit(data = train_data)

print(elasticnet_fit)

## QUESTION 4: ##
# Perform elastic net regression with "mixture = 0.5" and "penalty = 0.2". using all features (variables)
# in the training data, and then output the result of the fitted regression model.
flight_recipe <- recipe(ArrDelayMinutes ~ ., data = train_data)

ela_spec <- linear_reg(penalty = 0.5, mixture = 0.2) %>%
  set_engine("glmnet")

ela_wf <- workflow() %>%
  add_recipe(flight_recipe)

ela_fit <- ela_wf %>%
  add_model(ela_spec) %>%
  fit(data = train_data)

ela_fit %>%
  pull_workflow_fit() %>%
  tidy()


### COMPARING REGULARIZATION TYPES ###
# Now that we know more about regularization, it is also good to understand when we would use a technique
# over the other.

#* LASSO (L1): 
#** PROS: **#
# Lasso is primarily used for variable selection, that is, reducing the number of variables/features
# used in a model by shrinking the coefficients to ZERO. We would use this if we have many variables and 
# think just a select few would will be useful in a final model.

#** CONS: **#
# The downside of Lasso is that its variable selection is unstable, as in, for correlated variables it will 
# arbitrary select one. Additionally, if the number of data point (n) is less than the number of features
# (p), then Lasso can select at most 'n' of the features.


#* RIDGE (L2): 
#** PROS: **#
# If we don't want to reduce the number of variables, we can use this. Ridge also works well when there 
# is multicollinearity in the features because it reduce the variance while increasing bias.

#** CONS: **#
# Will not reduce the number of variables if that is our goal. Also, the bias in the model may be high.


#* ELASTIC NET (L1/L2) *#
#** PROS: **#
# Elastic net combines the benefits of Lasso and Ridge. It solve some of the issues that Lasso has when 
# doing variable selection because it works well when the variables are highly correlated and it can work 
# when the number of variables is greater than the number of samples.

#** CONS: **# 
# May be computationally more expensive than Lasso or Ridge because it computes both L1 and L2 penalties.



### 4. GRID SEARCH ###
# The goal of grid search is to find the values of the hyperparameters that results in the best models.
# This is know as "TUNING HYPERPARAMETERS". Hyperparameters are parameters that are not derived from 
# training models. For Example, "LAMBDA" in ridge/lasso is a hyperparameter. 

# Grid search takes a list of values for each hyperparameter it is tuning and iterates through each 
# combination. It then uses very combination of parameters to produce a model. For each model, a metric
# like RMSE is calculated. We then determine the best value of the hyperparameters by choosing the model
# with the best RMSE. In R, we can use functions in tidymodels to run grid search. 

## FIRST, define the lasso model. In this example, we will be tuning a lasso model so "MIXTURE = 1". We 
# will tune lambda, which is "PENALTY" in the function.
tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lasso_wf <- workflow() %>%
  add_recipe(flight_recipe)

## NEXT, define cross validation to resample the data:
flight_cvfolds <- vfold_cv(train_data)

## NOW, you can set up the grid using "GRID_REGULAR()". The LEVELS are how many values to use and in
# "PENALTY()" we can specify the range of values to use. By default, the range values are inverse log
# transformed. This means that -3 is really 10 ^ -3 and 0.3 is really 10 ^ 0.3.
lambda_grid <- grid_regular(levels = 50, penalty(range = c(-3, 0.3)))

# To tune the grid, use "TUNE_GRID()" and include the lambda grid just specified
lasso_grid <- tune_grid(lasso_wf %>% 
                          add_model(tune_spec), 
                        resamples = flight_cvfolds,
                        grid = lambda_grid)

## FINALLY, to view best results
show_best(lasso_grid, metric = "rmse")

# From the table, and using RMSE as the metric, using lambda (penalty) is equal to 1.46 gives the best 
# result. 

# Additionally, to visualize the RMSE results
lasso_grid %>% 
  collect_metrics() %>% 
  ggplot(aes(penalty, mean)) +
  geom_line(size = 1, color = "purple") +
  scale_x_log10() +
  ggtitle("RMSE")

# The dip in RMSE graph corresponds to the best value for lambda. so again, we see that using lambda 
# (penalty) of about 1.46 gives the best result. 

### QUESTION 5 ###
# Perform a grid search for the lambda (penalty) parameter on ridge regression, then find the best 
# values of the parameter. 
ridge_spec_2 <- linear_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet") 

ridge_wf_2 <- workflow() %>% 
  add_recipe(flight_recipe)

flight_cvfolds <- vfold_cv(train_data)

ridge_grid_2 <- tune_grid(ridge_wf_2 
                          %>% add_model(tune_spec),
                          resamples = flight_cvfolds,
                          grid = lambda_grid)

