# Load necessary libraries
install.packages("readr")
install.packages("tsibble")
install.packages("feasts")

library(ggplot2)
library(forecast)
library(TTR)
library(readr)
library(tsibble)
library(feasts)
library(dplyr)

# Load the data
df <- read_csv("weekly_customer_complaints.csv")

# Check the first few rows of the dataframe
head(df)

# Convert the data to a tsibble with weekly frequency
df <- df %>%
  mutate(week = yearweek(week)) %>%
  as_tsibble(index = week) %>%
  fill_gaps()

# Plot the weekly customer complaints
ggplot(df, aes(x = week, y = complaints)) +
  geom_line() +
  labs(title = "Weekly Customer Complaints")

# Plotting the monthly seasonality
df %>%
  index_by(month = ~ yearmonth(.)) %>%
  summarise(complaints = mean(complaints)) %>%
  gg_season(complaints, labels = NULL) +  # remove or adjust labels
  labs(title = "Monthly Seasonality of Customer Complaints")

# Plotting the quarterly seasonality
df %>%
  index_by(quarter = ~ yearquarter(.)) %>%
  summarise(complaints = mean(complaints)) %>%
  gg_season(complaints, labels = NULL) +
  labs(title = "Quarterly Seasonality of Customer Complaints")

# Set the period for the test data
periods <- 13

# Convert df to tsibble with 'week' as the index
df$week <- as.Date(df$week)
df <- df %>%
  as_tsibble(index = week)

# Split the data into training and test sets
library(lubridate)
train <- df %>%
  filter(week < (max_week - weeks(periods)))  # Training set: before last 13 weeks

test <- df %>%
  filter(week >= (max_week +1 - weeks(periods)))  # Test set: last 13 weeks

# View the head of the test set
head(test)

# Convert df$complaints into a time series object
complaints_ts <- ts(train$complaints, 
                    start = c(year(min(train$week)), week(min(train$week))),
                    frequency = 52)  # 52 for weekly data

# Fit the model
model_simple <- forecast::ses(complaints_ts, h = periods)

# Forecast
predictions_simple <- forecast::forecast(model_simple, h = periods)

# Plot training, test, and forecasts
complaints_ts_test <- ts(test$complaints, 
                    start = c(year(min(test$week)), week(min(test$week))),
                    frequency = 52)  # 52 for weekly data
autoplot(complaints_ts, series = "Train") +
  autolayer(complaints_ts_test, series = "Test") +
  autolayer(predictions_simple, series = "Forecast") +
  labs(title = "Train, Test, and Predictions with Simple Exponential Smoothing") +
  theme_minimal()

# Fit the model
model_double <- holt(complaints_ts, damped = FALSE, h = periods)

# Forecast
predictions_double <- forecast::forecast(model_double, 
                                         h = periods)

# Plot training, test, and forecasts
autoplot(complaints_ts, series = "Train") +
  autolayer(complaints_ts_test, series = "Test") +
  autolayer(predictions_double, series = "Forecast") +
  labs(title = "Train, Test, and Predictions with Double Exponential Smoothing") +
  theme_minimal()

# Fit the model
model_triple <- hw(complaints_ts, 
                   seasonal = "additive",
                   h = periods)


# Calculate the Mean Absolute Error (MAE)
mae <- mean(abs(test$complaints - predictions_double$mean))

# Calculate the Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((test$complaints - predictions_double$mean)^2))

# Calculate the Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((test$complaints - predictions_double$mean) / test$complaints)) * 100

cat(sprintf("The MAE is %.2f\n", mae))
cat(sprintf("The RMSE is %.2f\n", rmse))
cat(sprintf("The MAPE is %.2f%%\n", mape))

# Step 1: Convert df$complaints into a time series object
ts_full <- ts(df$complaints, start = c(year(min(df$week)), week(min(df$week))), frequency = 52)

# Step 2: Fit the Holt's linear trend model (no seasonality)
model_full <- holt(ts_full, h = periods)

# Step 3: Forecast the next 'periods'
forecast_full <- forecast(model_full, h = periods)

# Step 4: Plot the forecast
plot(forecast_full)

# Load the daily data
df_daily <- read_csv("bitcoin_price.csv")

# Ensure the 'Date' column is in Date format
df_daily$Date <- as.Date(df_daily$Date)

# Define the number of periods you want to subtract (e.g., 30 days for 1 month)
periods <- 30

# Find the max date in your data
max_date <- max(df_daily$Date)

# Step 1: Filter the data to create the training set, excluding the last 'periods' days
train_daily <- df_daily %>%
  filter(Date < (max_date - days(periods)))

# Step 2: Filter the data to create the test set, including the last 'periods' days
test_daily <- df_daily %>%
  filter(Date >= (max_date - days(periods)))

# View the head of the test set to check
head(test_daily)

# Step 1: Convert train_daily$Close into a time series object (assuming daily data with weekly seasonality)
ts_train_close <- ts(train_daily$Close, 
                     frequency = 7,  # Weekly seasonality
                     start = c(year(min(train_daily$Date)), yday(min(train_daily$Date))))

# Step 2: Fit the Holt-Winters model with multiplicative seasonality
model_daily_triple <- hw(ts_train_close, seasonal = "multiplicative", h = periods)

# Step 3: Forecast the next periods
forecast_daily_triple <- forecast(model_daily_triple, h = periods)

# Plot the forecast
plot(forecast_daily_triple)
