# Hands-on Time Series Analysis with R second Edition
# Chapter 02 Code
#

# Creating dummy data for your time series
my_data <- data.frame(
  Date = seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"),
  Value = runif(12)  # Random values for each month
)
# Create the time series object
my_time_series <- ts(data = my_data$Value, start = c(2020, 1), frequency = 12)

# Displaying the time series object
print(my_time_series)

# Getting a summary of the time series
summary(my_time_series)

# Plotting the time series
plot(my_time_series, main = "My First Time Series Plot", xlab = "Time", ylab = "Observations")

# Check if the zoo package is already installed
if (!requireNamespace("zoo", quietly = TRUE)) {
  # If not installed, install it
  install.packages("zoo")
}
# Load the zoo package
library(zoo)

# Assuming your data is in 'my_data' with 'dates' as your date column
zoo_object <- zoo(my_data$Value, order.by = my_data$Date)
print(zoo_object)
# Comments: Creates a zoo object from your data, ordering by the date

# Check if the xts package is already installed
if (!requireNamespace("xts", quietly = TRUE)) {
  # If not installed, install it
  install.packages("xts")
}
# Load the xts package
library(xts)

# Convert to xts object
xts_object <- xts(my_data$Value, order.by = as.POSIXct(my_data$Date))
print(xts_object)

# Basic plot of a zoo object
plot(zoo_object, main = "Stock Price Over Time", xlab = "Date", ylab = "Price")

# Advanced xts plotting
plot.xts(xts_object, main = "Detailed Stock Price Analysis")

# Check if the quantmod package is already installed
if (!requireNamespace("quantmod", quietly = TRUE)) {
  # If not installed, install it
  install.packages("quantmod")
}
# load the quantmod package
library(quantmod)

# Specify the symbol for Bitcoin from Yahoo Finance (BTC-USD)
getSymbols("BTC-USD", src = "yahoo", from = "2023-01-01", to = "2023-12-31", auto.assign = TRUE)

# View the data (Closing prices)
# Cl(x) extracts the closing prices 
# You can use Hi(x), Lo(x) to extract high, low prices accordingly 
# Check the R Documentation e.g. ?Cl()  
btc_data <- Cl(`BTC-USD`)

# 'btc_data' is an xts time series object of daily stock prices
specific_day_price <- btc_data['2023-03-15']
specific_day_price

# To slice data from March to April 2023
march_to_april <- btc_data['2023-03-01/2023-04-30']
march_to_april

# Calculating the average monthly Bitcoin prices
average_monthly_prices <- apply.monthly(btc_data, FUN = colMeans)

# View the average monthly prices
print(average_monthly_prices)

# Create dummy monthly data (you can replace this with actual data) library(xts) 
dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "month")
monthly_sales_data <- xts(runif(12, 1000, 2000), order.by = dates)
print(monthly_sales_data)

# Resample the monthly data to daily frequency
# Use to.daily() function to convert to daily frequency 
# daily frequency contains Open,High,Low,Close, we use x$Close to get close prices 
# na.approx function will replace NA by interpolation  
filled_daily_data <- na.approx(to.daily(monthly_sales_data)$monthly_sales_data.Close) 

# View the filled daily data 
print(filled_daily_data)


