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

# Load the zoo package
install.packages("zoo")
library(zoo)

# Assuming your data is in 'my_data' with 'dates' as your date column
zoo_object <- zoo(my_data$Value, order.by = my_data$Date)
print(zoo_object)
# Comments: Creates a zoo object from your data, ordering by the date

# Load the xts package
install.packages("xts")
library(xts)

# Convert to xts object
xts_object <- xts(my_data$Value, order.by = as.POSIXct(my_data$Date))
print(xts_object)

# Basic plot of a zoo object
plot(zoo_object, main = "Stock Price Over Time", xlab = "Date", ylab = "Price")

# Advanced xts plotting
plot.xts(xts_object, main = "Detailed Stock Price Analysis")

install.packages("quantmod")  # If not already installed
library(quantmod)

# Specify the symbol for Bitcoin from Yahoo Finance (BTC-USD)
getSymbols("BTC-USD", src = "yahoo", from = "2023-01-01", to = "2023-12-31", auto.assign = TRUE)

# View the data (Closing prices)
btc_data <- Cl(`BTC-USD`)  # Cl() extracts the closing prices

# Assuming 'btc_data' is an xts time series object of daily stock prices
specific_day_price <- btc_data['2023-03-15']
specific_day_price

# To slice data from March to April 2023
march_to_april <- btc_data['2023-03-01/2023-04-30']
march_to_april

# Calculating the average monthly Bitcoin prices
average_monthly_prices <- apply.monthly(btc_data, FUN = colMeans)

# View the average monthly prices
print(average_monthly_prices)

# Filling daily data from monthly averages with linear interpolation
filled_daily_data <- na.approx(apply.monthly(monthly_sales_data, to.daily))

# Create dummy monthly data (you can replace this with actual data) library(xts) 
dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-01"), by = "month") 
monthly_sales_data <- xts(runif(12, 1000, 2000), order.by = dates)

# Resample the monthly data to daily frequency and fill missing values using linear interpolation 
filled_daily_data <- na.approx(to.daily(monthly_sales_data)$monthly_sales_data.Close) 

# View the filled daily data 
print(filled_daily_data)


