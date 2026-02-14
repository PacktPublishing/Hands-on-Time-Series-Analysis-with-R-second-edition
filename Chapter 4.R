# Create a sequence of dates for one month
dates <- seq(as.Date("2024-01-01"), by="day", length.out=31)

# Simulate daily temperatures (in degrees Celsius)
temperatures <- rnorm(31, mean=22, sd=3) # Random data centered around 22 degrees Celsius

# Combine into a data frame
temperature_data <- data.frame(Date=dates, Temperature=temperatures)

# Check the structure to confirm date format
str(temperature_data)

# Plotting the data
plot(temperature_data$Date, temperature_data$Temperature, type="l", xlab="Date", ylab="Temperature (°C)",
     main="Simulated Daily Temperatures Over Time", col="blue", lwd=2)

plot(temperature_data$Date, temperature_data$Temperature, type="l",
     main="Daily Temperatures Over Time",
     xlab="Date",
     ylab="Temperature (°C)",
     col="blue")

# Adding a legend to the plot
legend("topright", legend=c("Observed Temperature", "Trend Line"), 
       col=c("blue", "red"), lty=1:1, cex=0.8)

# Adjusting colors
plot(temperature_data$Date, temperature_data$Temperature, type="l", col="darkgreen")

# Adjusting line types
plot(temperature_data$Date, temperature_data$Temperature, type="l", lty=2, col="darkgreen")

# Load ggplot2
install.packages("ggplot2")
library(ggplot2)

# Example data frame
temp_data <- data.frame(
  Date = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
  Temperature = rnorm(30, mean = 20, sd = 5)
)
# Creating a line plot
ggplot(data = temp_data, aes(x = Date, y = Temperature)) +
  geom_line() +
  labs(title = "Daily Temperature Over Time",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal()

# Simulating another time series
temp_data$Temperature2 = rnorm(30, mean = 15, sd = 5)

# Melting data for ggplot2
install.packages("reshape2")
library(reshape2)
temp_data_melted <- melt(temp_data, id.vars = "Date")

# Plotting multiple time series
ggplot(data = temp_data_melted, aes(x = Date, y = value, color = variable)) +
  geom_line() +
  labs(title = "Comparison of Two Temperature Time Series",
       x = "Date",
       y = "Temperature (°C)")

# Creating an area plot
ggplot(data = temp_data, aes(x = Date, y = Temperature)) +
  geom_area(fill = "lightblue") +
  labs(title = "Cumulative Daily Temperature",
       x = "Date",
       y = "Cumulative Temperature (°C)")

# Creating a step plot
ggplot(data = temp_data, aes(x = Date, y = Temperature)) +
  geom_step() +
  labs(title = "Step Plot of Temperature Changes",
       x = "Date",
       y = "Temperature (°C)")

# Assuming a third temperature series for stacking
temp_data$Temperature3 = rnorm(30, mean = 10, sd = 5)

# Melting data
temp_data_melted <- melt(temp_data, id.vars = "Date")

# Creating a stacked area chart
ggplot(data = temp_data_melted, aes(x = Date, y = value, fill = variable)) +
  geom_area(position = "stack") +
  labs(title = "Stacked Area Chart of Temperature Data",
       x = "Date",
       y = "Temperature (°C)")

# Create a simple line plot with a minimalistic theme
ggplot(temp_data, aes(x = Date, y = Temperature)) +
  geom_line() +
  theme_minimal()  # Applies a minimal theme

# Create a line plot and customize various theme elements
ggplot(temp_data, aes(x = Date, y = Temperature)) +
  geom_line() +
  theme_minimal() +
  theme(title = element_text(face = "bold", color = "#333333"),  # Bold title with specific color
        axis.title.x = element_text(size = 12, color = "blue"),  # Blue color and size for x-axis title
        axis.title.y = element_text(size = 12, color = "blue"),  # Blue color and size for y-axis title
        legend.title = element_text(face = "italic"))            # Italicized legend title

# Adding main title, subtitle, axis labels, and caption to the plot
ggplot(temp_data, aes(x = Date, y = Temperature)) +
  geom_line() +
  labs(
    title = "Daily Temperature Overview",  # Main plot title
    subtitle = "January 2024",  # Subtitle
    x = "Date",  # X-axis label
    y = "Temperature (°C)",  # Y-axis label
    caption = "Source: Weather Data"  # Caption at the bottom
  )

# Applying a logarithmic transformation to the y-axis
ggplot(temp_data, aes(x = Date, y = Temperature)) +
  geom_line() +
  scale_y_log10()  # Logarithmic transformation for the y-axis

# Customizing x-axis date labels to show only every week, with a 45-degree tilt
ggplot(temp_data, aes(x = Date, y = Temperature)) +
  geom_line() +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +  # Display labels in "Month Day" format
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Tilt text at 45 degrees for readability

# Adding a vertical line and text annotation to indicate an important event
ggplot(temp_data, aes(x = Date, y = Temperature)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2024-01-15"), color = "red", linetype = "dashed") +  # Vertical line at a specific date
  annotate("text", x = as.Date("2024-01-15"), y = 25, label = "Important Event", color = "red")  # Text annotation

# Highlighting points where temperature exceeds a threshold (25 °C) using red color
ggplot(temp_data, aes(x = Date, y = Temperature)) +
  geom_line() +
  geom_point(data = subset(temp_data, Temperature > 25), color = 'red')  # Red points for temperatures > 25°C

# Converting a ggplot2 graph to an interactive Plotly graph
install.packages("plotly")
library(plotly)
p <- ggplot(temp_data, aes(x = Date, y = Temperature)) + geom_line()  # Creating a basic ggplot2 plot
ggplotly(p)  # Convert ggplot2 plot to a Plotly object

install.packages("shiny")
library(shiny)

ui <- fluidPage(
  titlePanel("Simple Shiny Application"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("num", "Choose a number:", 1, 100, 50)
    ),
    mainPanel(
      textOutput("numberText")
    )
  )
)

server <- function(input, output) {
  output$numberText <- renderText({
    paste("You selected:", input$num)
  })
}

# Define the UI for the application
ui <- fluidPage(
  titlePanel("Dynamic Time Series Plot"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", "Select Number of Points:", 
                  min = 1, max = 100, value = 50)
    ),
    mainPanel(
      plotOutput("timePlot")
    )
  )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  
  output$timePlot <- renderPlot({
    # Generate sample time series data
    time_data <- data.frame(
      Time = seq(as.Date("2020-01-01"), by = "days", length.out = input$slider),
      Value = cumsum(rnorm(input$slider))
    )
    
    # Plot the time series data
    plot(time_data$Time, time_data$Value, type = 'l', 
         xlab = 'Time', ylab = 'Value', main = 'Dynamic Time Series')
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

ui <- fluidPage(
  # Slider input allows the user to select the number of days for time series data
  sliderInput("slider", "Select number of days:", 
              min = 1, max = 365, value = 30),
  
  # Output area for the time series plot
  plotOutput("timePlot")
)

server <- function(input, output) {
  
  # Reactive expression to generate time series data based on user input
  reactive_data <- reactive({
    # Create a data frame with two columns:
    # Time: Sequence of dates starting from 2020-01-01, with a length based on the slider input
    # Value: Cumulative sum of random numbers generated using rnorm()
    data.frame(
      Time = seq(as.Date("2020-01-01"), by = "days", length.out = input$slider),
      Value = cumsum(rnorm(input$slider))  # Cumulative sum of random normal values
    )
  })
  
  # Render a plot using the reactive time series data
  output$timePlot <- renderPlot({
    # Get the data from the reactive expression
    data <- reactive_data()
    
    # Create a line plot of Time vs Value
    plot(data$Time, data$Value, type = 'l', xlab = 'Time', ylab = 'Value')
  })
}

# Combine the UI and server to create the Shiny app
shinyApp(ui = ui, server = server)

# Example of using an observer function
server <- function(input, output, session) {
  observe({
    # Log when slider value changes
    cat("Slider value is now", input$slider, "\n")
  })
}

# Combine the UI and server to create the Shiny app
shinyApp(ui = ui, server = server)

# Create a ggplot2 time series plot
p <- ggplot(data.frame(x = 1:100, y = cumsum(rnorm(100))), aes(x, y)) + geom_line()

# Convert to an interactive Plotly plot
ggplotly(p)


