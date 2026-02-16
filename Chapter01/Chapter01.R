# Assigning a value to a variable
x <- 5
print(x)

# Basic arithmetic in R
sum <- 5 + 3 # Adds 5 and 3
difference <- 10 - 2 # Subtracts 2 from 10
product <- 4 * 2 # Multiplies 4 and 2
quotient <- 20 / 4 # Divides 20 by 4

# Using the sqrt() function to calculate square root
sqrt_result <- sqrt(16) # Calculates the square root of 16

# Creating a numeric vector with elements 1 to 5
numeric_vector <- c(1, 2, 3, 4, 5)

# Creating a character vector with three fruit names
character_vector <- c("apple", "banana", "cherry")
print(character_vector)

# Creating a 3x3 matrix with numbers 1 to 9
my_matrix <- matrix(1:9, nrow=3, ncol=3)
print(my_matrix)

# Creating a data frame with names, ages, and salaries
my_data_frame <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35),
  Salary = c(50000, 60000, 65000))
print(my_data_frame)

# Creating a list containing a name, an age, and a vector of hobbies
my_list <- list(Name="Alice", Age=25, Hobbies=c("Reading", "Cycling"))
print(my_list)

# Addition
sum_result <- 5 + 3
print(sum_result)

# Subtraction
difference_result <- 10 - 2
print(difference_result)

# Multiplication
product_result <- 4 * 2
print(product_result)

# Division
quotient_result <- 20 / 4
print(quotient_result)

# Raising to a power
power_result <- 2^3
print(power_result)

# Modulus (remainder of division)
modulus_result <- 7 %% 3
print(modulus_result)

# Calculating the mean (average) of a numeric vector
mean_value <- mean(c(1, 2, 3, 4, 5))
print(mean_value)

# Summing up elements of a vector
total_sum <- sum(c(1, 2, 3, 4, 5))
print(total_sum)

# Generating a sequence of numbers
number_sequence <- seq(from = 1, to = 10, by = 2)
print(number_sequence)

# Sorting elements of a vector
sorted_vector <- sort(c(5, 2, 3, 4, 1))
print(sorted_vector)

# Calculating Square Root for Numbers in a Vector Using sapply:
# Applying sqrt function to each element of a vector (square root numbers)
sqrt_vector <- sapply(c(1, 4, 9, 16, 25), sqrt)
print(sqrt_vector)
