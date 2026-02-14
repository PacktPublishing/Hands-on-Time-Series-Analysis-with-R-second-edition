# Load necessary libraries
install.packages("tidyverse")
install.packages("TSA")
library(tidyverse)
library(ggplot2)
library(forecast)
library(lubridate)
library(TSA)

# Load the data and set the date as index
df <- read_csv("daily_revenue.csv", 
               col_types = cols(date = col_date(format = "%d/%m/%Y")))
head(df)

# Get information about the dataframe
str(df)

# Setting the frequency to daily
df <- df %>% 
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  fill(revenue)

# Rename the revenue column to 'y'
df <- df %>% 
  rename(y = revenue)
head(df)

# Plot daily revenue
ggplot(df, aes(x = date, y = y)) +
  geom_line() +
  ggtitle("Daily Revenues") +
  xlab("Date") +
  ylab("Revenue")

# Plotting the monthly seasonality
df %>% 
  group_by(month = floor_date(date, "month")) %>% 
  summarise(mean_revenue = mean(y, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = mean_revenue)) +
  geom_line() +
  ggtitle("Monthly Seasonality") +
  xlab("Month") +
  ylab("Revenue")

# Plotting the quarterly seasonality
df %>% 
  group_by(quarter = floor_date(date, "quarter")) %>% 
  summarise(mean_revenue = mean(y, na.rm = TRUE)) %>%
  ggplot(aes(x = quarter, y = mean_revenue)) +
  geom_line() +
  ggtitle("Quarterly Seasonality") +
  xlab("Quarter") +
  ylab("Revenue")

# Seasonal decomposition of time series
decomposed <- decompose(ts(df$y, frequency = 365), type = "multiplicative")
autoplot(decomposed)

# Plot the autocorrelation (ACF)
acf(df$y, lag.max = 100, main = "Autocorrelation Function")

# Plot the partial autocorrelation (PACF)
pacf(df$y, lag.max = 100, main = "Partial Autocorrelation Function")

# Load necessary library for ADF test
library(tseries)

# Assuming df$y is your time series
# Perform ADF test
adf_test <- adf.test(df$y, alternative = "stationary")

# Print p-value
print(paste("p-value:", adf_test$p.value))

# Interpreting the results
if (adf_test$p.value < 0.05) {
  print("Evidence suggests that the time series is stationary.")
} else {
  print("Evidence suggests that the time series is not stationary.")
}

# Difference the series
df <- df %>% mutate(y_diff = c(NA, diff(y)))
head(df)

# Plotting the original and differenced series
par(mfrow = c(2, 1))
# Plot original series
plot(df$date, df$y, type = "l", col = "blue", main = "Original Time Series", xlab = "Date", ylab = "Revenue")
legend("topright", legend = "Original Series", col = "blue", lty = 1)

# Plot differenced series
plot(df$date[-1], df$y_diff[-1], type = "l", col = "orange", main = "Differenced Time Series", xlab = "Date", ylab = "Differenced Revenue")
legend("topright", legend = "Differenced Series", col = "orange", lty = 1)

# Perform ADF test on differenced series
adf_test_diff <- adf.test(df$y_diff[-1], alternative = "stationary")

# Print p-value
print(paste("p-value:", adf_test_diff$p.value))

# Split the Data into training and test
test_days <- 30
train <- head(df$y, -test_days)
test <- tail(df$y, test_days)

# Using auto.arima for the ARIMA model and the best parameters
model <- auto.arima(train, seasonal = FALSE)
summary(model)

# Predictions
predictions_arima <- forecast(model, h = test_days)$mean
predictions_arima

# Function to assess model and visualize output without requiring time inputs
model_assessment <- function(train, test, predictions, chart_title) {
  
  # Combine all data points (train, test, and predictions) with a sequence index
  combined_length <- length(train) + length(test) + length(predictions)
  
  # Generate the index for the Date column based on the length of train, test, and predictions
  index <- seq(1, combined_length)
  
  # Create a data frame for plotting
  plot_df <- data.frame(
    Date = c(index[1:length(train)], index[(length(train)+1):(length(train) + length(test))], 
             index[(length(train) + length(test) + 1):combined_length]),
    Value = c(as.numeric(train), as.numeric(test), as.numeric(predictions)),
    Type = rep(c("Train", "Test", "Forecast"), c(length(train), length(test), length(predictions)))
  )
  
  # Plot the data using ggplot
  library(ggplot2)
  p <- ggplot(plot_df, aes(x = Date, y = Value, color = Type)) +
    geom_line() +
    labs(title = chart_title, x = "Index", y = "Value") +
    theme_minimal()
  
  print(p)  # Force the plot to display
  
  # Calculating the MAE, RMSE, and MAPE
  mae <- mean(abs(test - predictions))
  rmse <- sqrt(mean((test - predictions)^2))
  mape <- mean(abs((test - predictions) / test)) * 100
  
  cat(sprintf("The MAE is %.2f\n", mae))
  cat(sprintf("The RMSE is %.2f\n", rmse))
  cat(sprintf("The MAPE is %.2f %%\n", mape))
}

# Model assessment
dev.off()  # Reset the graphics device
plot.new()  # Open a new graphics device
model_assessment(train, test, predictions_arima, "ARIMA")

# Convert the training data to a time series with weekly seasonality
train_ts <- ts(train, frequency = 7)

# Fit SARIMA using auto.arima
model_sarima <- auto.arima(train_ts, seasonal = TRUE)
summary(model_sarima)

# Predictions
predictions_sarima <- forecast(model_sarima, h = test_days)$mean
predictions_sarima[1:5]

# Model assessment
model_assessment(train, test, predictions_sarima, "SARIMA")

# Transform regressors into numeric
df <- df %>%
  mutate(discount_rate = as.numeric(gsub("%", "", discount_rate)),
         coupon_rate = as.numeric(gsub("%", "", coupon_rate)))

# Check the structure of the dataframe
str(df)

# Split the Regressor Data into training and test
exog_train <- head(df[, 3:4], -test_days)
exog_test <- tail(df[, 3:4], test_days)
head(exog_test)

# Using auto.arima for the SARIMAX model with the best parameters
model_sarimax <- auto.arima(train_ts, 
                            seasonal = TRUE, 
                            xreg = as.matrix(exog_train))
summary(model_sarimax)

# Predictions
predictions_sarimax <- forecast(model_sarimax, xreg = as.matrix(exog_test), h = test_days)$mean

# Model assessment
model_assessment(train, test, predictions_sarimax, "SARIMAX")

# Define the rolling forecast cross-validation function
rolling_forecast_cv <- function(data, initial, h, step, arima_order, seasonal_order, xreg_cols = NULL, period = NULL) {
  n <- nrow(data)
  print(n)  # Check the number of rows in the dataset
  print(initial)  # Check initial size
  print(h)  # Check the horizon size
  print(step)  # Check the step size
  
  # Check if initial + h is valid given the size of the data
  if (n <= initial + h) {
    stop("Initial window size plus forecast horizon is larger than the dataset")
  }
  
  results <- list()
  
  for (start in seq(1, n - initial - h, by = step)) {
    # Split the data into training and testing sets
    train <- data[start:(start + initial - 1), ]
    test <- data[(start + initial):(start + initial + h - 1), ]
    
    # Ensure xreg is numeric and in matrix form
    if (!is.null(xreg_cols)) {
      train_xreg <- as.matrix(train[, xreg_cols, drop = FALSE])
      test_xreg <- as.matrix(test[, xreg_cols, drop = FALSE])
      
      # Fit the ARIMA or SARIMAX model
      fit <- Arima(train$y, order = arima_order, 
                   seasonal = list(order = seasonal_order), 
                   xreg = train_xreg)
      
      forecast_vals <- forecast(fit, h = h, xreg = test_xreg)$mean
    } else {
      # Fit the ARIMA model without external regressors
      fit <- Arima(train$y, 
                   order = arima_order, 
                   seasonal = list(order = seasonal_order))
      forecast_vals <- forecast(fit, h = h)$mean
    }
    
    # Calculate RMSE for each forecast
    rmse <- sqrt(mean((test$y - forecast_vals)^2))
    results[[length(results) + 1]] <- rmse
  }
  
  return(results)
}

# Rolling forecast cross-validation using the SARIMAX model
results <- rolling_forecast_cv(
  data = df, 
  initial = 1400, 
  h = test_days, 
  step = floor(test_days / 2),  # Ensure step is a valid integer
  arima_order = c(1, 1, 1), 
  seasonal_order = c(0, 0, 2), 
  xreg_cols = 3:4
)

# Calculate the mean RMSE from the list of results
rmse <- mean(unlist(results))

# Print the RMSE
cat(sprintf("The RMSE is %.2f\n", rmse))

# Expanded param_grid with more possible combinations of p, d, q, P, D, Q
param_grid <- expand.grid(
  p = c(0, 1, 2, 3),  # Non-seasonal AR order (p)
  d = c(0, 1),        # Non-seasonal differencing (d)
  q = c(0, 1, 2),     # Non-seasonal MA order (q)
  P = c(0, 1),        # Seasonal AR order (P)
  D = c(0, 1),        # Seasonal differencing (D)
  Q = c(0, 1)         # Seasonal MA order (Q)
)

nrow(param_grid)



# Cross-validation function with error handling for singular matrices
rolling_forecast_cv_tuning <- function(data, initial, h, step, param_grid, xreg_cols = NULL) {
  n <- nrow(data)
  
  best_params <- NULL
  best_rmse <- Inf  # Start with a high RMSE to track the best one
  error_count <- 0  # Track how many times an error occurs
  
  # Iterate over each row in the param_grid
  for (i in 1:nrow(param_grid)) {
    # Extract ARIMA and seasonal orders from param_grid
    arima_order <- c(param_grid$p[i], param_grid$d[i], param_grid$q[i])
    seasonal_order <- c(param_grid$P[i], param_grid$D[i], param_grid$Q[i])
    
    cat(sprintf("Testing params: p=%d, d=%d, q=%d, P=%d, D=%d, Q=%d\n",
                param_grid$p[i], param_grid$d[i], param_grid$q[i],
                param_grid$P[i], param_grid$D[i], param_grid$Q[i]))
    
    # Initialize a list to store RMSE for each iteration
    results <- list()
    
    for (start in seq(1, n - initial - h, by = step)) {
      # Split the data into training and testing sets
      train <- data[start:(start + initial - 1), ]
      test <- data[(start + initial):(start + initial + h - 1), ]
      
      # Ensure xreg is numeric and in matrix form
      if (!is.null(xreg_cols)) {
        train_xreg <- as.matrix(train[, xreg_cols, drop = FALSE])
        test_xreg <- as.matrix(test[, xreg_cols, drop = FALSE])
      }
      
      # Use tryCatch to handle errors
      result <- tryCatch({
        if (!is.null(xreg_cols)) {
          # Fit the ARIMA or SARIMAX model with external regressors
          fit <- Arima(train$y, order = arima_order, 
                       seasonal = list(order = seasonal_order), 
                       xreg = train_xreg)
          forecast_vals <- forecast(fit, h = h, xreg = test_xreg)$mean
        } else {
          # Fit the ARIMA model without external regressors
          fit <- Arima(train$y, order = arima_order, 
                       seasonal = list(order = seasonal_order))
          forecast_vals <- forecast(fit, h = h)$mean
        }
        
        # Calculate RMSE for each forecast
        rmse <- sqrt(mean((test$y - forecast_vals)^2))
        rmse  # Return RMSE
        
      }, error = function(e) {
        # Log the error and skip this iteration
        cat("Error: ", conditionMessage(e), "\n")
        error_count <<- error_count + 1
        return(NULL)  # Return NULL if there's an error
      })
      
      if (!is.null(result)) {
        results[[length(results) + 1]] <- result
      }
    }
    
    # If results are available (i.e., no error), calculate the average RMSE
    if (length(results) > 0) {
      avg_rmse <- mean(unlist(results))
      cat(sprintf("Average RMSE for current params: %.2f\n", avg_rmse))
      
      # Track the best parameters
      if (avg_rmse < best_rmse) {
        best_rmse <- avg_rmse
        best_params <- param_grid[i, ]
      }
    }
  }
  
  # Report number of errors
  cat(sprintf("Number of errors encountered: %d\n", error_count))
  
  # Return the best parameter combination and its corresponding RMSE
  if (!is.null(best_params)) {
    cat(sprintf("Best parameters: p=%d, d=%d, q=%d, P=%d, D=%d, Q=%d with RMSE: %.2f\n",
                best_params$p, best_params$d, best_params$q,
                best_params$P, best_params$D, best_params$Q, best_rmse))
  } else {
    cat("No valid model found.\n")
  }
  
  return(list(best_params = best_params, best_rmse = best_rmse))
}


# Perform cross-validation with parameter tuning
tuning_results <- rolling_forecast_cv_tuning(
  data = df, 
  initial = 1400, 
  h = 30, 
  step = 15, 
  param_grid = param_grid,  # Your param_grid data frame
  xreg_cols = 3:4  # Assuming columns 3 and 4 are external regressors
)

# Print the best parameters and RMSE
cat(sprintf("Best RMSE is %.2f with parameters: p=%d, d=%d, q=%d, P=%d, D=%d, Q=%d\n",
            tuning_results$best_rmse, tuning_results$best_params$p, tuning_results$best_params$d,
            tuning_results$best_params$q, tuning_results$best_params$P, tuning_results$best_params$D,
            tuning_results$best_params$Q))

# Save the best parameters
best_params <- tuning_results %>% filter(rmse == min(rmse)) %>% t()
best_params