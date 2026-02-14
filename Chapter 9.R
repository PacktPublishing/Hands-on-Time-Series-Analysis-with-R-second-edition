# Install brms if you haven't already
install.packages("brms")

# Load the package
library(brms)

# Assuming you have some time series data (e.g., sales data)
data <- data.frame(
  time = 1:12,
  sales = rnorm(12, 100, 10)  # Random example sales data
)

# Fit a simple Bayesian regression model
fit <- brm(
  sales ~ time,  # Formula for the time series
  data = data,
  family = gaussian(),  # You can choose different distributions if needed
  chains = 4, iter = 2000, warmup = 500  # Bayesian MCMC parameters
)

# Summarize the model
summary(fit)

# Extract posterior samples
posterior_samples <- as.data.frame(fit)

# Plot posterior distribution of the mean sales (Intercept)
hist(posterior_samples$b_Intercept, breaks = 30, main = "Posterior Distribution of Mean Sales", xlab = "Mean Sales")
abline(v = mean(posterior_samples$b_Intercept), col = "red", lwd = 2)

# New observed data
new_y <- rnorm(12, mean = 115, sd = 10)  # New data points

# Update data list
data_list$y <- c(data_list$y, new_y)
data_list$N <- length(data_list$y)

# Refit model with new data
fit_updated <- brm(
  sales ~ time,
  data = data.frame(time = 1:data_list$N, sales = data_list$y),
  family = gaussian(),
  chains = 4, iter = 2000, warmup = 500
)

# Print updated results
summary(fit_updated)

# Example data
set.seed(123)
N <- 50
J <- 5
store <- factor(sample(1:J, N, replace = TRUE))
mu_global <- 100
sigma_global <- 10
mu <- rnorm(J, mean = mu_global, sd = sigma_global)
y <- rnorm(N, mean = mu[as.numeric(store)], sd = 10)

# Combine into a data frame
data_hierarchical <- data.frame(store = store, y = y)

# Fit a hierarchical Bayesian model with brms
fit_hierarchical <- brm(
  y ~ (1 | store),  # Random intercept for each store
  data = data_hierarchical,
  family = gaussian(),
  chains = 4, iter = 2000, warmup = 500
)

# Print results
summary(fit_hierarchical)

# Example data generation for AR(1) process
set.seed(123)
N <- 100
phi <- 0.6
sigma <- 1
y <- numeric(N)
y[1] <- rnorm(1)
for (t in 2:N) {
  y[t] <- phi * y[t-1] + rnorm(1, sd = sigma)
}

# Combine into a data frame
data_ar1 <- data.frame(time = 1:N, y = y)

# Fit the AR(1) model with brms
fit_ar1 <- brm(
  bf(y ~ ar(time = time, p = 1)),  # AR(1) structure with lag 1
  data = data_ar1,
  family = gaussian(),
  chains = 4, iter = 2000, warmup = 500
)

# Print the results
summary(fit_ar1)

# Example data generation for AR(1) process
set.seed(123)
N <- 100
phi <- 0.6
sigma <- 1
y <- numeric(N)
y[1] <- rnorm(1)
for (t in 2:N) {
  y[t] <- phi * y[t-1] + rnorm(1, sd = sigma)
}

# Combine into a data frame
data_ar1 <- data.frame(time = 1:N, y = y)

# Define a weakly informative prior for sigma
priors <- c(
  prior(cauchy(0, 5), class = "sigma")  # Weakly informative prior for sigma
)

# Fit the AR(1) model with weakly informative prior on sigma using brms
fit_weakly_informative <- brm(
  bf(y ~ ar(time = time, p = 1)),  # AR(1) structure with lag 1
  data = data_ar1,
  family = gaussian(),
  prior = priors,  # Using the weakly informative prior for sigma
  chains = 4, iter = 2000, warmup = 500
)

# Print the results
summary(fit_weakly_informative)

# Install and load necessary packages
library(bayesplot)  # For traceplot
library(loo)  # For WAIC

# Posterior predictive checks
posterior_predictive <- posterior_predict(fit_ar1)

# Plot posterior predictive distribution
plot(y, type = "l", col = "blue", main = "Posterior Predictive Check", xlab = "Time", ylab = "Observed vs Predicted")
for (i in 1:100) {
  lines(1:N, posterior_predictive[i, ], col = adjustcolor("red", alpha.f = 0.1))
}
lines(1:N, y, col = "blue", lwd = 2)

# Convergence diagnostics using traceplot
traceplot(fit_ar1)

# Model comparison using WAIC
waic_fit <- waic(fit_ar1)
print(waic_fit)

# Generate example data for ARMA(1,1) with trend
set.seed(123)
N <- 100
phi <- 0.6
theta <- 0.4
sigma <- 1
trend <- 0.1
y <- numeric(N)
y[1] <- rnorm(1)
for (t in 2:N) {
  y[t] <- phi * y[t-1] + theta * (y[t-1] - trend) + rnorm(1, sd = sigma)
}

# Combine data into a data frame
data_arma <- data.frame(time = 1:N, y = y)

# Create lagged variables for AR(1) and MA(1)
data_arma$lag_y <- c(NA, head(data_arma$y, -1))  # AR(1) lagged term
data_arma$lag_error <- c(NA, diff(data_arma$y))  # MA(1) lagged error term

# Remove the first row with NAs
data_arma <- data_arma[-1,]

# Define weakly informative priors for the model
priors <- c(
  prior(normal(0, 0.5), class = "b", coef = "lag_y"),     # Prior for AR(1) coefficient
  prior(normal(0, 0.5), class = "b", coef = "lag_error"), # Prior for MA(1) coefficient
  prior(normal(0, 1), class = "sigma"),                   # Prior for standard deviation
  prior(normal(0, 1), class = "b", coef = "time")         # Prior for trend
)

# Fit the ARMA(1,1) model with trend using brms
fit_arma <- brm(
  bf(y ~ time + lag_y + lag_error),  # ARMA(1,1) with trend modeled using lagged terms
  data = data_arma,
  family = gaussian(),
  prior = priors,
  chains = 4, iter = 2000, warmup = 500
)

# Print model summary
summary(fit_arma)

# Fit the AR(1) model with lagged terms
fit_arma <- brm(
  bf(y ~ time + lag_y + lag_error),  # ARMA(1,1) with trend modeled using lagged terms
  data = data_arma,
  family = gaussian(),
  chains = 4, iter = 2000, warmup = 500
)

# Generate posterior predictive distribution
posterior_predictive <- posterior_predict(fit_arma)

# Plot the posterior predictive distribution
plot(data_arma$y, type = "l", col = "blue", main = "Posterior Predictive Check", xlab = "Time", ylab = "Observed vs Predicted")
for (i in 1:100) {
  lines(1:nrow(data_arma), posterior_predictive[i, ], col = adjustcolor("red", alpha.f = 0.1))
}
lines(1:nrow(data_arma), data_arma$y, col = "blue", lwd = 2)

# Calculate residuals: difference between observed data and mean of posterior predictive samples
residuals <- data_arma$y - apply(posterior_predictive, 2, mean)

# Plot residuals
plot(residuals, type = "h", main = "Residuals", xlab = "Time", ylab = "Residuals")
abline(h = 0, col = "red")

# Example of fitting the model with different priors using brms
fit_weak_prior <- brm(
  bf(y ~ time + lag_y + lag_error), 
  data = data_arma, 
  family = gaussian(),
  prior = prior(normal(0, 1), class = "b", coef = "time"),  # Weak prior
  chains = 4, iter = 2000, warmup = 500
)

fit_strong_prior <- brm(
  bf(y ~ time + lag_y + lag_error), 
  data = data_arma, 
  family = gaussian(),
  prior = prior(normal(0, 0.1), class = "b", coef = "time"),  # Strong prior
  chains = 4, iter = 2000, warmup = 500
)

# Compute LOO for each model
loo_weak_prior <- loo(fit_weak_prior)
loo_strong_prior <- loo(fit_strong_prior)

# Compare models using LOO
loo_compare(loo_weak_prior, loo_strong_prior)

# Assuming the data is stored in 'data_arma'

# Create Subset 1: First half of the dataset
subset1_data <- data_arma[1:(nrow(data_arma) / 2), ]

# Create Subset 2: Second half of the dataset
subset2_data <- data_arma[((nrow(data_arma) / 2) + 1):nrow(data_arma), ]

# Fit models on different subsets
fit_subset1 <- brm(
  bf(y ~ time + lag_y + lag_error), 
  data = subset1_data,  # Subset 1 of the data
  family = gaussian(), 
  chains = 4, iter = 2000, warmup = 500
)

fit_subset2 <- brm(
  bf(y ~ time + lag_y + lag_error), 
  data = subset2_data,  # Subset 2 of the data
  family = gaussian(),
  chains = 4, iter = 2000, warmup = 500
)

# Compute the LOO criterion for both models
fit_subset1 <- add_criterion(fit_subset1, "loo")
fit_subset2 <- add_criterion(fit_subset2, "loo")

# Compare models based on LOO
loo_compare(fit_subset1, fit_subset2)

