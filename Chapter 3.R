# Convert a string to a Date object
my_date <- as.Date("2023-04-15")
print(my_date)

# Change date format to MM/DD/YYYY
formatted_date <- format(my_date, format = "%m/%d/%Y")
print(formatted_date)

# Generate a weekly sequence starting from April 15, 2023, for the next ten weeks
date_sequence <- seq(from = my_date, by = "week", length.out = 10)
print(date_sequence)

# Difference in days between two dates
end_date <- as.Date("2023-05-15")
date_diff <- difftime(end_date, my_date, units = "days")
print(date_diff)

# Current date and time in POSIXct
current_time <- Sys.time() # Automatically returns POSIXct
print(current_time)

# Adding one day (86400 seconds)
tomorrow <- current_time + 86400
print(tomorrow)

# Current date and time in POSIXlt
current_time_lt <- as.POSIXlt(Sys.time())

# Changing the hour and minute
current_time_lt$hour <- 20 # Set the hour to 8 PM
current_time_lt$min <- 30 # Set the minute to 30
print(current_time_lt)

# Convert POSIXct to POSIXlt
posixct_time <- Sys.time()
posixlt_time <- as.POSIXlt(posixct_time)
print(posixlt_time)

# Convert POSIXlt to POSIXct
posixct_converted <- as.POSIXct(posixlt_time)
print(posixct_converted)

# Create a POSIXct object for a specific date and time in the UTC time zone
time_utc <- as.POSIXct("2023-10-01 12:00:00", tz = "UTC")
print(time_utc)

# Convert to Eastern Standard Time (EST)
time_est <- format(time_utc, format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")
print(time_est)

# Date before daylight saving time ends
dst_date_before <- as.POSIXct("2023-11-04 12:00:00", tz = "America/New_York")
print(dst_date_before)

# Date after daylight saving time ends
dst_date_after <- as.POSIXct("2023-11-06 12:00:00", tz = "America/New_York")
print(dst_date_after)

# Display only the date
date_only <- format(Sys.time(), "%Y-%m-%d")
print(date_only)

# Display only the time
time_only <- format(Sys.time(), "%H:%M:%S")
print(time_only)

# Format including time zone
datetime_tz <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
print(datetime_tz)

# Format excluding seconds
datetime_no_seconds <- format(Sys.time(), "%Y-%m-%d %H:%M")
print(datetime_no_seconds)

# Load the 'bitcoin_price.csv' as a CSV
df = read.csv("bitcoin_price.csv")

# 'df' is a data frame loaded with a 'Date' column
df$Date <- as.Date(df$Date, format="%Y-%m-%d")
print(df$Date)

# Safe parsing with tryCatch to handle errors
safe_parse_date <- function(date_string) {
  tryCatch(
    as.Date(date_string, format="%Y-%m-%d"),
    error=function(e) NA # Return NA on error
  )
}
parsed_dates <- sapply(df$Date, safe_parse_date)

# Formatting date-time for a user-friendly report
report_date <- Sys.time()
formatted_date <- strftime(report_date, format="%A, %B %d, %Y at %H:%M")
print(formatted_date)

# Parsing a date-time string from an international source
date_string <- "Monday, 21 March 2023 14:00"
parsed_date <- strptime(date_string, format="%A, %d %B %Y %H:%M", tz="Europe/Berlin")
print(parsed_date)

# Define a standard date-time format for a project
standard_datetime_format <- "%Y-%m-%d %H:%M:%S"
# Function to format any date-time object according to the standard
format_datetime <- function(datetime) {
  format(datetime, standard_datetime_format)
}
# Example of applying the standard format
current_time <- Sys.time()
formatted_time <- format_datetime(current_time)
print(formatted_time)

# Function to safely parse date-time strings with error handling
safe_parse_datetime <- function(datetime_string) {
  tryCatch(
    as.POSIXct(datetime_string, format = "%Y-%m-%d %H:%M:%S"),
    error = function(e) {
      warning("Invalid datetime format: ", datetime_string)
      return(NA)
    }
  )
}
# Example usage
datetime_input <- "2023-02-30 12:00:00" # Incorrect date
parsed_datetime <- safe_parse_datetime(datetime_input)
print(parsed_datetime)

# Assuming 'stock_prices' is a vector of daily closing stock prices
daily_changes <- diff(stock_prices)

# Apply 'diff' to compute the period-over-period return
daily_changes <- diff(df$Adj.Close)
print(daily_changes)

# Example inflation rates for demonstration (use actual rates)
inflation_rates <- rep(0.02, nrow(df)) # Assuming a constant 2% inflation for simplicity

# Adjusting the 'Adj.Close' for inflation
real_data <- df$Adj.Close / (1 + inflation_rates)

# Add the adjusted data back to the DataFrame
df$Real.Adj.Close <- real_data

# Display the first few rows of the DataFrame to check
head(df)

# Calculate a 7-day moving average for 'Adj.Close' in the DataFrame
library(zoo)
rolling_avg <- rollmean(df$Adj.Close, 7, fill = NA, align = "center")
print(rolling_avg)

# Calculate a rolling sum for the 'Volume' column over a 7-day window
weekly_rolling_sum <- rollsum(df$Volume, 7, fill = NA, align = "right")
print(weekly_rolling_sum[1:15])

# Calculate a rolling median for 'Adj.Close' over a 7-day window
weekly_median_prices <- rollmedian(df$Adj.Close, 7, fill = NA, align = "center")
print(weekly_median_prices[1:5])

# Adjusting the stock prices to account for weekend non-trading days
#install.packages("dplyr")
library(dplyr)
adjusted_stock_data <- df %>%
  mutate(Adjusted.Close = if_else(weekdays(as.Date(Date)) %in% c("Saturday", "Sunday"), NA, Adj.Close))
print(adjusted_stock_data[1:5])

# Assuming 'public_holidays' includes Christmas and other holidays
public_holidays <- c("2014-12-25") # Christmas in 2014

# Adjusting the DataFrame
adjusted_for_holidays <- df %>%
  mutate(Adjusted.Close = if_else(as.Date(Date) %in% as.Date(public_holidays), NA, Adj.Close))

# Print the rows where the date matches public holidays
holidays_adjusted <- adjusted_for_holidays %>%
  filter(as.Date(Date) %in% as.Date(public_holidays))

print(holidays_adjusted)

# Linear interpolation for missing values in 'Adj.Close'
df$Adj.Close <- na.approx(df$Adj.Close)
# This line of code fills in missing stock prices in the 'Adj.Close' column by interpolating between available data points.

# Display the first few rows after interpolation
cat("Data after linear interpolation for missing values:\n")
print(head(df))

# Z-score normalization for 'Adj.Close'
df$Normalized.Adj.Close <- (df$Adj.Close - mean(df$Adj.Close, na.rm = TRUE)) / sd(df$Adj.Close, na.rm = TRUE)

# Display the first few rows after normalization
cat("\nData after Z-score normalization:\n")
print(head(df))

# Assuming 'df' is a dataframe with daily adjusted closing prices in 'Adj.Close'
df$lag_1 <- lag(df$Adj.Close, 1)
df$lag_7 <- lag(df$Adj.Close, 7)
print(head(df,10))


# Calculate a 7-day moving average for 'Adj.Close'
df$moving_average_7 <- rollmean(df$Adj.Close, 7, fill = NA)

# Calculate a 30-day rolling standard deviation for 'Adj.Close'
df$rolling_std_30 <- rollapply(df$Adj.Close, 30, sd, fill = NA)

# Display the first few rows after adding rolling window features
print(head(df[10:13],10))

