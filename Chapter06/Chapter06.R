# Hands-on Time Series Analysis with R second Edition
# Chapter 06 Code
#

# The package list
packages <- c("readr","tsibble","feasts", "ggplot2", "forecast", "TTR", "dplyr" )

# Check if the package installed or not
# If not installed, install the package

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    # If not installed, install them
    install.packages(p)
  
  }
  # Load the package accordingly
  library(p, character.only = TRUE)
}


# Load the data
df <- read_csv("weekly_customer_complaints.csv")

# Check the first few rows of the dataframe
head(df)

# Convert the data to a tsibble with weekly frequency
df <- df %>%
  mutate(week = yearweek(week)) %>%
  as_tsibble(index = week) %>%
  fill_gaps()
head(df)

# Plot the weekly customer complaints
ggplot(df, aes(x = week, y = complaints)) +
  geom_line() +
  labs(title = "Weekly Customer Complaints")


month_df <- df %>%
  index_by(month = ~ yearmonth(.))

head(month_df)

month_df<-  month_df %>%
  summarise(complaints = mean(complaints)) 
head(month_df)

# Plotting the Monthly seasonality
month_df %>%
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

# Load package and retrieve the max value of week
library(lubridate)
max_week <- max(df$week)

# Split the data into training and test sets
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


# Fit the simple exponential smoothing (SES) model
model_simple <- forecast::ses(complaints_ts, h = periods)

# Forecast
predictions_simple <- forecast::forecast(model_simple, h = periods)


complaints_ts_test <- ts(test$complaints, 
                    start = c(year(min(test$week)), week(min(test$week))),
                    frequency = 52)  # 52 for weekly data

# Plot training, test, and forecasts
autoplot(complaints_ts, series = "Train" ) +
  autolayer(complaints_ts_test, series = "Test") +
  autolayer(predictions_simple, series = "Forecast", alpha=0.5) +
  labs(title = "Train, Test, and Predictions with Simple Exponential Smoothing") +
  theme_minimal()

# Fit the model
model_double <- holt(complaints_ts, damped = FALSE, h = periods)
print(model_double$model)


# Forecast
predictions_double <- forecast::forecast(model_double,
                                         h = periods)

# Plot training, test, and forecasts
autoplot(complaints_ts, series = "Train") +
  autolayer(complaints_ts_test, series = "Test") +
  autolayer(predictions_double, series = "Forecast", alpha=0.5) +
  labs(title = "Train, Test, and Predictions with Double Exponential Smoothing") +
  theme_minimal()


# Fit the model
model_triple <- hw(complaints_ts, 
                   seasonal = "multiplicative",
                   h = periods)


# Fit the Holt's linear trend model (no seasonality)
model_holt <- holt(complaints_ts , h = periods)

# Forecast the next 'periods'
forecast_holt <- forecast(model_holt, h = periods)

# Plot the forecast
plot(forecast_holt)

# Retrieve error metrics 
accuracy(forecast_holt)
# Retrieve error metrics for test data
accuracy(forecast_holt, test$complaints)


# Load the daily data
df_daily <- read_csv("bitcoin_price.csv")

# Set the date as the index and ensure it is of Date typ
df_daily$Date <- as.Date(df_daily$Date)
df_daily <- df_daily %>% as_tsibble(index = Date)



# Define the number of periods you want to subtract (e.g., 30 days for 1 month)
periods <- 30

# Find the max date in your data
max_date <- max(df_daily$Date)

# Filter the data to create the training set, excluding the last 'periods' days
train_daily <- df_daily %>%
  filter(Date < (max_date - days(periods)))

# Filter the data to create the test set, including the last 'periods' days
test_daily <- df_daily %>%
  filter(Date >= (max_date+1 - days(periods)))

# View the head of the test set to check
head(test_daily)

# Convert train_daily$Close into a time series object (assuming daily data with weekly seasonality)
min_date <- min(train_daily$Date)
year_start <- year(min_date)
day_start <- yday(min_date)
ts_train_close <- ts(train_daily$Close, 
                     # Weekly seasonality
                     frequency = 7,  
                     start = c(year_start, day_start))

# Fit the Holt-Winters model with multiplicative seasonality
model_daily_triple <- hw(ts_train_close, seasonal = "multiplicative", h = periods)

# Forecast the next periods
forecast_daily_triple <- forecast(model_daily_triple, h = periods)

# Plot the forecast
plot(forecast_daily_triple)

