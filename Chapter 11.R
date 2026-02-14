# Install the Prophet package from CRAN
install.packages("prophet")

# Load the Prophet library
library(prophet)

# Load the data from a CSV file
data <- read.csv('Daily Bike Sharing training.csv')
head(data)

# Drop Variables That Are Not Needed
library(dplyr)
data <- data %>% select(-instant, -season, -yr, -mnth, -weekday, -casual, -registered)

# Rename the target variable to 'y' and the date variable to 'ds'
data <- data %>% rename(y = cnt, ds = dteday)
head(data)

# Convert the 'ds' column to Date format
data$ds <- as.Date(data$ds, format = "%m/%d/%Y")
head(data)

# Fit the Prophet model to the data
model <- prophet(data)

# Create a dataframe with future dates for predictions
future <- make_future_dataframe(model, periods = 30)

# Generate forecasts for the future dates
forecast <- predict(model, future)

# Plot the forecast along with the actual data
plot(model, forecast)

# Plot the individual components of the forecast
prophet_plot_components(model, forecast)

# Initialize the Prophet model with desired parameters
m <- prophet(
  yearly.seasonality = TRUE,        # Include yearly seasonality
  weekly.seasonality = TRUE,        # Include weekly seasonality
  seasonality.mode = 'multiplicative', # Use multiplicative seasonality
  seasonality.prior.scale = 10,     # Increase flexibility of seasonality
  holidays.prior.scale = 10,        # Increase impact of holidays
  changepoint.prior.scale = 0.05    # Control flexibility in detecting trend changes
)

# Add additional regressors (external factors) to the model
m <- add_regressor(m, 'workingday')   # Whether it's a working day
m <- add_regressor(m, 'temp')         # Temperature
m <- add_regressor(m, 'atemp')        # Apparent temperature
m <- add_regressor(m, 'hum')          # Humidity
m <- add_regressor(m, 'windspeed')    # Wind speed
m <- add_regressor(m, 'weathersit')   # Weather situation category

# Fit the model to the data
m <- fit.prophet(m, data)

# Extract necessary regressors from the past data
regressors <- data %>% select(ds, holiday, workingday, weathersit, temp, atemp, hum, windspeed)

# Import future regressors from a CSV
future_regressors <- read.csv("Daily Bike Sharing future.csv")

# Select only the necessary columns
future_regressors_selected <- future_regressors %>%
  select(dteday, holiday, workingday, weathersit, temp, atemp, hum, windspeed)

# Rename the 'dteday' column to 'ds'
future_regressors_selected <- future_regressors_selected %>% rename(ds = dteday)

# Convert the 'ds' column to 'yyyy-mm-dd' format
future_regressors_selected$ds <- as.Date(future_regressors_selected$ds, format = "%m/%d/%Y")
tail(future_regressors_selected)

# Combine the data and future regressors
combined_data <- bind_rows(regressors, future_regressors_selected)
tail(combined_data)

# Generate forecasts for the future dates
forecast <- predict(m, combined_data)

# Creating a dataframe for holidays
holidays <- data.frame(
  holiday = c('New Year', 'Christmas'),
  ds = as.Date(c('2012-01-01', '2012-12-25')),
  lower_window = 0,
  upper_window = 1
)

# Initialize the Prophet model with desired parameters
m2 <- prophet(
  yearly.seasonality = TRUE,        # Include yearly seasonality
  weekly.seasonality = TRUE,        # Include weekly seasonality
  seasonality.mode = 'multiplicative', # Use multiplicative seasonality
  holidays = holidays,              # Adding the holidays to the model
  seasonality.prior.scale = 10,     # Increase flexibility of seasonality
  holidays.prior.scale = 10,        # Increase impact of holidays
  changepoint.prior.scale = 0.05    # Control flexibility in detecting trend changes
)

# Cross-validation setup
df_cv <- cross_validation(model, initial = 365 , period = 15, horizon = 30, units = 'days')

# Evaluating model performance
df_p <- performance_metrics(df_cv)

# Define the range of parameter values to explore
changepoint_priors <- c(0.01, 0.05, 0.1)
seasonality_priors <- c(1, 10, 20)
holiday_priors <- c(1, 10, 20)

# Create combinations of parameters
param_combinations <- expand.grid(changepoint_priors, seasonality_priors, holiday_priors)

# Initialize a list to store the performance metrics
results <- list()

# Iterate over each combination of parameters
for (i in 1:nrow(param_combinations)) {
  # Extract the current combination of parameters
  cp <- param_combinations[i, 1]
  sp <- param_combinations[i, 2]
  hp <- param_combinations[i, 3]
  
  # Initialize the Prophet model with desired parameters (including daily seasonality if needed)
  m <- prophet(
    yearly.seasonality = TRUE, 
    weekly.seasonality = TRUE,        
    daily.seasonality = FALSE,  # Disable daily seasonality, or set to TRUE to enable
    seasonality.mode = 'multiplicative',
    holidays = holidays,     
    changepoint.prior.scale = cp, 
    seasonality.prior.scale = sp, 
    holidays.prior.scale = hp)
  
  # Add additional regressors (external factors) to the model **before fitting**
  m <- add_regressor(m, 'workingday')   # Whether it's a working day
  m <- add_regressor(m, 'temp')         # Temperature
  m <- add_regressor(m, 'atemp')        # Apparent temperature
  m <- add_regressor(m, 'hum')          # Humidity
  m <- add_regressor(m, 'windspeed')    # Wind speed
  m <- add_regressor(m, 'weathersit')   # Weather situation category
  
  # Now, fit the model with the data
  m <- fit.prophet(m, data)
  
  # Perform cross-validation
  df_cv <- cross_validation(m, 
                            initial = 365, 
                            period = 15,
                            horizon = 30,
                            units = 'days')
  
  # Evaluate the performance metrics
  df_p <- performance_metrics(df_cv)
  
  # Store the results with corresponding parameters
  param_key <- paste0("cp=", cp, "_sp=", sp, "_hp=", hp)
  results[[param_key]] <- df_p
}

# Identify the best performing parameter set
best_params <- NULL
best_performance <- Inf
for (param_set in names(results)) {
  current_performance <- mean(results[[param_set]]$rmse)
  if (current_performance < best_performance) {
    best_performance <- current_performance
    best_params <- param_set
  }
}

# Print the best performing parameters
print(paste("Best parameters:", best_params))
print(paste("Best RMSE:", best_performance))
