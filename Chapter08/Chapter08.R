# Hands-on Time Series Analysis with R second Edition
# Chapter 08 Code
#


# Check if the forecast package is already installed
if (!requireNamespace("forecast", quietly = TRUE)) {
  # If not installed, install it
  install.packages("forecast")
}
# Load the forecast package
library(forecast)

# Load the AirPassengers dataset
data("AirPassengers")

# Plot the dataset to visualize the time series
plot(AirPassengers, main="AirPassengers Data", ylab="Number of Passengers", xlab="Time")

# Fit a TBATS model to the AirPassengers data
tbats_model <- tbats(AirPassengers,   seasonal.periods = 12)

# Print the model summary to see the components and parameters
summary(tbats_model)

# Forecast the next 24 months using the TBATS model
tbats_forecast <- forecast(tbats_model, h=24)

# Plot the forecast along with the original data
plot(tbats_forecast, main="TBATS Forecast for AirPassengers", ylab="Number of Passengers", xlab="Time")

# Calculate the accuracy of the TBATS model
accuracy(tbats_forecast)


# Adjusting the number of Fourier terms in the TBATS model
# Using seasonal period of 12 (month) 
# TBATS automatically picks k for each period
tbats_model <- tbats(AirPassengers, use.parallel=TRUE, num.cores=NULL, seasonal.periods=c(12))
# Print the k value for the periods 
print(tbats_model$k.vector)

# Applying a Box-Cox transformation in the TBATS model
lambda <- BoxCox.lambda(AirPassengers)
tbats_model <- tbats(AirPassengers, lambda=lambda)
print(lambda)


# Including ARMA errors in the TBATS model
tbats_model <- tbats(AirPassengers, use.arma.errors=TRUE)

# Specifying seasonal periods in the TBATS model
tbats_model <- tbats(AirPassengers, seasonal.periods=c(12, 6))

# Fine-tuning the TBATS model parameters
tbats_model <- tbats(AirPassengers, use.trend=TRUE, use.damped.trend=TRUE, seasonal.periods=c(12), use.parallel=TRUE)

