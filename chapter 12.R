#libraries
library(dplyr)
library(ggplot2)
library(forecast)

#load the dataset
df = read.csv("DHS_Daily_Report_2020.csv")

#Transform the Date Variable
df$Date = strptime(x = df$Date,
                   format = "%m/%d/%Y")
df$Date = as.Date(df$Date)

#Pick the variables
df = df %>% select(Date,
                   Total.Individuals.in.Shelter,
                   Easter,
                   Thanksgiving,
                   Christmas,
                   Temperature)

#Change variable name
colnames(df)[2] = "y"

##################### Model Preparation #################

#Training and test set
training = df %>% filter(Date < '2020-12-01')
test = df %>% filter(Date >= '2020-12-01')

#Transform the time series
#7 for daily, 52 for weekly, 12 for monthly, 4 for quarterly
#5 for weekdays
training_y = ts(training$y, frequency = 7)

#Extract the regressors
training_reg = as.matrix(training[, 3:ncol(training)])
test_reg = as.matrix(test[, 3:ncol(test)])

#Model
model = nnetar(y = training_y,
               p = 1,
               P = 1, 
               size = 3,
               xreg = training_reg,
               decay = 0.1)

#Predictions
predictions = forecast(model, xreg = test_reg)

#accuracy
accuracy(predictions$mean, test$y)


######################## Cross-Validation ########

#Generating Dates
last_date = tail(df$Date, 1) - 30
first_date = tail(df$Date, 1) - 200
dates = seq.Date(from = first_date, 
                 to = last_date,
                 by = "week")

#Results vector
results_cv = vector(mode = 'numeric',
                    length = length(dates))

#Cross Validation

for (j in 1:length(dates)) {
  
  #Training and test set
  training = df %>% filter(Date <= dates[j])
  test = df %>% filter(Date > dates[j] & Date <= (dates[j] + 31))
  
  #Time Series Object
  training_y  = ts(training$y, frequency = 7)
  
  #Regressors
  training_reg = as.matrix(training[, 3:ncol(training)])
  test_reg = as.matrix(test[, 3:ncol(test)])
  
  #Model
  model = nnetar(y = training_y,
                 p = 1,
                 P = 1,
                 decay = 0.1,
                 size = 3,
                 xreg = training_reg)
  
  #Predictions
  predictions = forecast(model, xreg = test_reg)
  
  #Accuracy
  results_cv[j] = accuracy(predictions$mean, test$y)[1,2]
  print(dates[j])
  
}

#Get the results
mean(results_cv)

###################### Parameter tuning ###############

#Generating Dates
last_date = tail(df$Date, 1) - 30
first_date = tail(df$Date, 1) - 200
dates = seq.Date(from = first_date, 
                 to = last_date,
                 by = "week")

#Results vector
results_cv = vector(mode = 'numeric',
                    length = length(dates))

#Grid
Grid = expand.grid(p = seq(from = 1, to = 6, by = 1),
                   P = c(1,2,3),
                   decay = c(0.05, 0.1, 0.15, 0.2),
                   size = c(2,3,4))

#Results for the Grid
results_grid = vector(mode = 'numeric', length = nrow(Grid))

#PArameter tuning
for (i in seq_len(nrow(Grid))) {
  #Fetch the parameters
  parameters = Grid[i, ]
  
  #Cross Validation
  for (j in 1:length(dates)) {
    
    #Training and test set
    training = df %>% filter(Date <= dates[j])
    test = df %>% filter(Date > dates[j] & Date <= (dates[j] + 31))
    
    #Time Series Object
    training_y  = ts(training$y, frequency = 7)
    
    #Regressors
    training_reg = as.matrix(training[, 3:ncol(training)])
    test_reg = as.matrix(test[, 3:ncol(test)])
    
    #Model
    model = nnetar(y = training_y,
                   p = parameters$p,
                   P = parameters$P,
                   decay = parameters$decay,
                   size = parameters$size,
                   xreg = training_reg)
    
    #Predictions
    predictions = forecast(model, xreg = test_reg)
    
    #Accuracy
    results_cv[j] = accuracy(predictions$mean, test$y)[1,2]
    print(dates[j])
    
  }
  
  #Store Results Grid
  results_grid[i] = mean(results_cv)
  print(i)
}

#Fetch the best parameters
Grid = cbind(Grid, results_grid)
best_params = Grid[Grid$results_grid == min(results_grid), ]
