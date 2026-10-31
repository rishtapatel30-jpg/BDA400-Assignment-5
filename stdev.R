# Standard Deviation (stdev) function
stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)
  
  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value
  
  # Calculate the squared differences
  squared_diff <- diff_values ^ 2
  
  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)
  
  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}

# Test example (from assignment instructions)[cite: 17]
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
stdev_result <- stdev(data)
print(stdev_result)