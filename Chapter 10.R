# Example Top-Down Forecasting in R

# Load necessary libraries
library(forecast)

# Historical sales data by region
historical_sales <- data.frame(
  Region = c("North", "South", "East", "West"),
  Sales = c(100000, 150000, 120000, 130000)
)

# Calculate total sales
total_sales <- sum(historical_sales$Sales)

# Forecast total future sales (e.g., using a simple method)
total_forecast <- 550000  # Example total forecast for the next period

# Calculate the proportion of sales by region
historical_sales$Proportion <- historical_sales$Sales / total_sales

# Disaggregate the total forecast into regional forecasts
historical_sales$Forecast <- historical_sales$Proportion * total_forecast

# Print the forecasted sales by region
print(historical_sales)

# Forecasted sales data for individual products in different regions
product_forecasts <- data.frame(
  Product = c("A", "A", "B", "B", "C", "C"),
  Region = c("North", "South", "North", "South", "North", "South"),
  Forecast = c(5000, 6000, 7000, 8000, 4500, 5500)
)

# Aggregate forecasts to get total sales forecast per product
total_forecast_product <- aggregate(Forecast ~ Product, data = product_forecasts, sum)

# Aggregate forecasts to get total sales forecast per region
total_forecast_region <- aggregate(Forecast ~ Region, data = product_forecasts, sum)

# Aggregate forecasts to get overall total sales forecast
total_forecast_overall <- sum(product_forecasts$Forecast)

# Print the aggregated forecasts
print(total_forecast_product)
print(total_forecast_region)
print(total_forecast_overall)

# Sales data forecasted at an intermediate level (e.g., category level)
category_forecasts <- data.frame(
  Category = c("Electronics", "Furniture", "Clothing"),
  Forecast = c(150000, 100000, 120000)
)

# Proportions for disaggregating category forecasts into products
product_proportions <- data.frame(
  Category = c("Electronics", "Electronics", "Furniture", "Furniture", "Clothing", "Clothing"),
  Product = c("Phones", "Computers", "Chairs", "Tables", "Shirts", "Pants"),
  Proportion = c(0.6, 0.4, 0.5, 0.5, 0.55, 0.45)
)

# Disaggregate category forecasts into product-level forecasts
product_forecasts <- merge(category_forecasts, product_proportions, by = "Category")
product_forecasts$Product_Forecast <- product_forecasts$Forecast * product_forecasts$Proportion

# Aggregate product forecasts to confirm consistency with category forecasts
aggregated_category_forecasts <- aggregate(Product_Forecast ~ Category, data = product_forecasts, sum)

# Print the product-level forecasts and aggregated category-level checks
print(product_forecasts)
print(aggregated_category_forecasts)

install.packages("hts")
library(hts)

data(infantgts)

plot(infantgts)

h <- 10  # Forecast horizon
forecast_infant <- forecast(infantgts, h = h, method = "comb", fmethod = "arima")
plot(forecast_infant)
