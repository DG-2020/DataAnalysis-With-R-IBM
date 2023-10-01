
## LOAD TIDYMODELS LIBRARY
#install.packages("tidymodels")
library(tidymodels)


## MODEL EVALUATION ##
# Provide Seed Value
set.seed(1234)

# Create single binary data into a Training Set and Testing Set
flight_split <- initial_split (aa_delays, prop = 0.8)
train_data <- training(flight_split)
test_data <- testing(flight_split)

# Pick the model(linear regression)
lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")     # Set Engine
  # Fit the model
  train_fit <- lm_spec %>% 
    fit(ArrDelayMinutes ~ DepDelayMinutes, data = train_data)
  print(train_fit)
  
# Evaluate the models to estimate their performance on new data
  test_results <- train_fit %>%
    predict(new_data = test_data) %>%    # Make the Prediction
    mutate(truth = test_data$ArrDelayMinutes)  # Create the New Column
## In the end we have a dataframe with the predictions and the true values

# Testing RMSE for an estimate of how well our model performs on new data
  rmse(test_results, truth = truth, estimate = .pred)
  rsq(test_results, truth = truth, estimate = .pred)

train_results <- train_fit %>% 
  predict(new_data = train_data) %>%
  mutate(truth = train_data$ArrDelayMinutes)

# Training RMSE for an estimate of models performance upon training
rmse(train_results, truth = truth, estimate = .pred)
rsq(train_results, truth = truth, estimate = .pred)

# Visualize how well we predicted the Arrival Delay
test_results %>% 
  mutate(train = "testing") %>%
  bind_rows(train_results) %>%
  mutate(train = "training") %>%
  ggplot(aes(truth, .pred)) + geom_abline(lty = 2, color = "orange", linewidth = 1.5) + 
  geom_point(color = "#006EA1", alpha = 0.5) + facet_wrap(~ train) + 
  labs(x = "Truth", y = "Predicted Arrival Delays (Mins)")


## CROSS VALIDATION ##
set.seed(1234)
cv_folds <- vfold_cv(train_data, v = 10)
results = fit_resamples(lm_spec, ArrDelayMinutes ~ DepDelayMinutes, resamples = cv_folds)
results %>% collect_metrics()


## EXAMPLE OF "UNDERFITTING" ##
# Cars Dataset: Predict the DISTANCE it takes for cars to STOP, given their SPEED
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_hline(yintercept = mean(cars$dist), col = 'red')

# Cars Dataset: Predict the DISTANCE it takes for cars to STOP, given their SPEED,
# with a polynomial with 8 degrees
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 8),
              col = "green", se = FALSE)

# Regularization: Reduce the model's complexity
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method = "lm",
              formula = y ~ x,
              col  = "purple",
              se = FALSE)

## REGULARIZATION ## 
## Code for RIDGE Regression ##
flight_recipe <- recipe(ArrDelayMinutes ~ ., data = train_data)

ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>% 
  set_engine("glmnet")

ridge_wf <- workflow() %>% 
  add_recipe(flight_recipe)

ridge_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)

ridge_fit %>%
  pull_workflow_fit() %>%
  tidy()


## GRID SEARCH ##
# 1: DEFINE THE LASSO MODEL
tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

#2: DEFINE CROSS VALIDATION TO RESAMPLE THE DATA
flight_cvfolds <- vfold_cv(train_data)

#3: SET UP THE GRID
lambda_grid <- grid_regular(levels = 50, penalty(range = c(-3, 0.3)))

lasso_wf <- workflow() %>%
  add_model(tune_spec) %>%
  fit(data = train_data)

#4: TUNE THE GRID
lasso_grid <- tune_grid(
  lasso_wf %>% add_model(tune_spec),
  resamples = flight_cvfolds,
  grid = lambda_grid)

#5: VIEW BEST RESULTS
show_best(lasso_grid, metric = "rmse")
