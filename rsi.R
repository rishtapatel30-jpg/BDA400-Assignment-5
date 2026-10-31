# Relative Strength Index (RSI) function
rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)
  
  # Initialize two vectors to store the gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  # Calculate gains and losses
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
      losses[i] <- 0
    } else {
      gains[i] <- 0
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA, length(data))
  
  # Calculate the average gains and average losses for the first 'period' data points
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])
  
  # Calculate RSI values using Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    if (i > (period + 1)) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    }
    
    # Handle division by zero for flat markets
    if (avg_loss == 0) {
      rs <- 0
    } else {
      rs <- avg_gain / avg_loss
    }
    
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }
  
  return(rsi_values)
}

# Test example (from assignment instructions)[cite: 22]
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
rsi_result <- rsi(data, period = 5)
print(rsi_result)