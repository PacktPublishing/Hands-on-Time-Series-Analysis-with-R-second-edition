# Read the data
df = read.csv("choco_monthly_revenue.csv")

# Ensure Month.with.Year is in Date format
df$Month.with.Year <- as.Date(df$Month.with.Year)

# Plotting the data using ggplot2
library(ggplot2)
ggplot(df, aes(x = Month.with.Year, y = revenue)) +
  geom_line(color = "blue", size = 1) + # Line color and thickness
  labs(title = "Time Series Trend Analysis",
       x = "Date",
       y = "Value",
       caption = "Revenue Data Over 5 years.") +
  theme_minimal() # Apply a minimalistic theme for clarity

# Adding trend lines to the graph to highlight different trends
ggplot(df, aes(x = Month.with.Year, y = revenue)) +
  geom_line(color = "blue", size = 1) +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Add a linear trend line
  labs(title = "Analysis of Trend Types",
       subtitle = "Red line represents a linear fit to the data",
       x = "Date",
       y = "Value") +
  theme_minimal()

# Load necessary libraries
install.packages("Kendall")
library(Kendall)

# Assuming time_series_data is already defined
# Run the Mann-Kendall test
mk_test <- MannKendall(df$revenue)

# Print the results of the test
print(mk_test)

# Extract the p-value and tau correctly
p_value <- mk_test$sl
tau_value <- mk_test$tau

# Interpret the results
if (p_value < 0.05) {
  if (tau_value > 0) {
    cat("There is a statistically significant increasing trend in the data.\n")
  } else {
    cat("There is a statistically significant decreasing trend in the data.\n")
  }
} else {
  cat("No significant trend detected in the data.\n")
}

# Calculate the Simple Moving Average (SMA) with a window of 10
sma_results <- stats::filter(df$revenue, rep(1/6, 6), sides = 1)

# Plotting the original data and the SMA
plot(df$revenue, type = "l", col = "blue", lwd = 1, ylab = "Revenue", xlab = "Time", main = "Simple Moving Average")
lines(sma_results, col = "red", lwd = 2)
legend("topright", legend = c("Original Data", "SMA"), col = c("blue", "red"), lty = 1, lwd = 2)

# Install TTR if you don't have it already
install.packages("TTR")

# Load the TTR package
library(TTR)

# Calculate Exponential Moving Average for a window of 10 periods
ema_results <- EMA(df$revenue, n = 6)

# Plotting the original data and the EMA
plot(df$revenue, type = "l", col = "blue", lwd = 1, ylab = "Value", xlab = "Time", main = "Exponential Moving Average")
lines(ema_results, col = "green", lwd = 2)
legend("topright", 
       legend = c("Original Data", 
                  "EMA"), 
       col = c("blue", "green"), 
       lty = 1, 
       lwd = 2)

# Convert the Date to numeric (e.g., the number of days since 1970-01-01)
df$Month.with.Year.numeric <- as.numeric(df$Month.with.Year)

# Apply loess smoothing
loess_smoothed <- loess(revenue ~ Month.with.Year.numeric, 
                        data = df, 
                        span = 0.3)

# Predict the smoothed values
smoothed_values <- predict(loess_smoothed)

# Plot the original data and the smoothed values
plot(df$Month.with.Year, df$revenue, type = "l", col = "blue", lwd = 1, 
     ylab = "Revenue", xlab = "Date", main = "LOESS Smoothing")
lines(df$Month.with.Year, smoothed_values, col = "red", lwd = 2)
legend("topright", legend = c("Original Data", "LOESS"), col = c("blue", "red"), lty = 1, lwd = 2)

# Create the smoothed data frame as before
smoothed_data <- data.frame(
  Date = df$Month.with.Year,
  Smoothed_Price = predict(loess_smoothed)
)

# Identifying trends through derivative analysis
smoothed_data$Shifted_Price <- dplyr::lag(smoothed_data$Smoothed_Price, 1)  # Use dplyr for lag
smoothed_data$Price_Change <- smoothed_data$Smoothed_Price - smoothed_data$Shifted_Price

# Plot the smoothed time series
ggplot(smoothed_data, aes(x = Date, y = Smoothed_Price)) +
  geom_line(color = "blue") +
  labs(title = "Smoothed Time Series Analysis",
       x = "Date",
       y = "Smoothed Price") +
  theme_minimal()

# Load necessary library
library(stats)

# Creating a sample time series data with monthly observations for two years
time_series <- ts(df$revenue, frequency = 12)

# Applying the decompose function
decomposed_data <- decompose(time_series)
plot(decomposed_data)

# Applying the STL function
stl_data <- stl(time_series, s.window = "periodic")
plot(stl_data)

# Load necessary libraries
install.packages("forecast")
library(forecast)

# Generating seasonal subseries plot
ggseasonplot(time_series, year.labels = TRUE, col = 1:12) +
  labs(title = "Seasonal Subseries Plot", x = "Month", y = "Value")

# Performing Fourier analysis to detect frequency
spectrum(time_series, main = "Spectral Density", span = 12)
´
# Load dplyr if not already loaded
library(dplyr)

# Create the lag data for the time series (revenue)
lag_data <- data.frame(
  actual = df$revenue[-1],  # Remove the first observation
  lag1 = lag(df$revenue, 1)[-1]  # Lag by 1, remove the first NA
)

# Plot the lag plot using ggplot2
library(ggplot2)

ggplot(lag_data, aes(x = lag1, y = actual)) +
  geom_point(color = "blue") +
  labs(title = "Lag Plot of Time Series (Revenue)", 
       x = "Lag 1", 
       y = "Actual Values") +
  theme_minimal()

# Calculate and plot the ACF using base R
acf(df$revenue, main = "ACF of Time Series (Revenue)")
PACF(ts_data, main = "PACF of Time Series")

# Plotting the PACF
pacf(df$revenue, main = "PACF of Time Series (Revenue)")


# Calculate Z-scores
z_scores <- scale(time_series) # scale() function standardizes the data

# Define a threshold for identifying outliers
threshold <- 2

# Identify outliers
outliers <- which(abs(z_scores) > threshold)

# Print outliers
cat("Outlier indices:", outliers, "\n")

# Calculate IQR
Q1 <- quantile(time_series, 0.25)
Q3 <- quantile(time_series, 0.75)
IQR_value <- Q3 - Q1

# Define bounds for outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify outliers
outliers <- which(time_series < lower_bound | time_series > upper_bound)

# Print outliers
cat("Outlier indices:", outliers, "\n")

# Install devtools if not already installed
if(!require(devtools)) install.packages("devtools")

# Install AnomalyDetection from Twitter's GitHub repository
devtools::install_github("twitter/AnomalyDetection")

# Load the AnomalyDetection library
library(AnomalyDetection)

# Ensure the data has the required structure for AnomalyDetectionTs
anomaly_data <- data.frame(
  timestamp = df$Month.with.Year,  # Assuming this is your time column
  revenue = df$revenue             # Assuming this is your numeric data
)

# Now run the anomaly detection
result <- AnomalyDetectionTs(anomaly_data, max_anoms = 0.05, direction = 'both', plot = TRUE)
# The function returns a list containing detected anomalies and a plot if specified
anomalies <- result$anoms
plot <- result$plot

# View anomalies
print(anomalies)

# Save the plot
ggsave("anomaly_detection_plot.png", plot=plot)
