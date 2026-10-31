# Exponential Moving Average (EMA) function
ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)
  
  # Initialize an empty array to store EMA values
  ema_values <- c()
  
  # Loop through the data array
  for (i in 1:length(data)) {
    # Calculate EMA for the first data point
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }
  
  return(ema_values)
}

# Test example (from assignment instructions)
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
ema_result <- ema(data, period = 3)
print(ema_result)