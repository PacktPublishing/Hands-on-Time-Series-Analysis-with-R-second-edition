# Install and load torch
install.packages("torch")
library(torch)

# Load dplyr for data manipulation
library(dplyr)

# Load your data
data <- read.csv("electricity.csv")

# Normalize the data
data <- data %>%
  mutate(across(c(y, Exogenous1, Exogenous2), scale))

# Split data into 80% training and 20% testing
set.seed(123)  # for reproducibility
train_size <- floor(0.8 * nrow(data))
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]

# Define function to create sequences for LSTM input
create_sequences <- function(data, time_steps) {
  X <- list()
  Y <- list()
  
  for (i in 1:(nrow(data) - time_steps)) {
    # Collect sequences of length `time_steps`
    X[[i]] <- as.matrix(data[i:(i + time_steps - 1), c("y", "Exogenous1", "Exogenous2")])
    Y[[i]] <- data$y[i + time_steps]
  }
  
  # Convert lists to torch tensors
  X <- torch_tensor(unlist(X), dtype = torch_float32())$view(c(length(X), time_steps, 3))
  Y <- torch_tensor(unlist(Y), dtype = torch_float32())$view(c(length(Y), 1))
  
  list(X = X, Y = Y)
}

# Set time steps for sequence length
time_steps <- 5
train <- create_sequences(train_data, time_steps)
test <- create_sequences(test_data, time_steps)

lstm_model <- nn_module(
  # Initialize the model's layers and structure
  initialize = function(input_size, hidden_size, output_size) {
    # LSTM layer: takes input sequences and produces hidden states
    self$lstm <- nn_lstm(input_size = input_size, hidden_size = hidden_size, batch_first = TRUE)
    # Fully connected layer to produce final output
    self$fc <- nn_linear(hidden_size, output_size)
  },
  
  # Define the forward pass of the model
  forward = function(x) {
    # Pass the input `x` through the LSTM layer and retrieve only the output tensor
    lstm_out <- self$lstm(x)[[1]]
    # Take the last hidden state and pass it through the fully connected layer
    output <- self$fc(lstm_out[ , -1, ])
    output  # Return the output
  }
)

# Define model parameters
input_size <- 3      # Number of features (`y`, `Exogenous1`, `Exogenous2`)
hidden_size <- 50    # Number of hidden units in the LSTM layer
output_size <- 1     # Size of output (predicting a single value `y`)

# Initialize the model
model <- lstm_model(input_size, hidden_size, output_size)

# Define loss function and optimizer
loss_fn <- nnf_mse_loss  # Mean Squared Error loss for regression
optimizer <- optim_adam(model$parameters, lr = 0.001)  # Adam optimizer for training

# Set the number of epochs
epochs <- 100

# Training loop
for (epoch in 1:epochs) {
  model$train()  # Set the model to training mode
  
  optimizer$zero_grad()  # Reset gradients for each epoch
  
  # Forward pass: make predictions on training data
  predictions <- model(train$X)
  
  # Compute the loss between predictions and actual target values
  loss <- loss_fn(predictions, train$Y)
  
  # Backward pass: compute gradients
  loss$backward()
  
  # Update model parameters
  optimizer$step()
  
  # Print loss every 10 epochs for monitoring
  if (epoch %% 10 == 0) {
    cat("Epoch:", epoch, " Loss:", loss$item(), "\n")
  }
}


# Evaluation: Set model to evaluation mode
model$eval()

# Make predictions on the test set
predictions <- model(test$X)
predicted_values <- as.numeric(predictions)
actual_values <- as.numeric(test$Y)

# Calculate RMSE for model performance
rmse <- sqrt(mean((predicted_values - actual_values)^2))
print(paste("Test RMSE:", rmse))

# Optional: Plot the predicted values against actual values
plot(actual_values, type = "l", col = "blue", main = "LSTM Predictions vs Actual", ylab = "y")
lines(predicted_values, col = "red")
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)
