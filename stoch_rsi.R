# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # Filter out NA values to calculate min and max RSI
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  if (length(valid_rsi) == 0) {
    stop("Not enough data to calculate valid RSI values for StochRSI.")
  }
  
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  
  # Normalize RSI to get %K values
  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(valid_rsi))
  } else {
    k_values <- (valid_rsi - min_rsi) / (max_rsi - min_rsi)
  }
  
  # Calculate the %K line (SMA of normalized RSI values)[cite: 23]
  k_line <- sma(k_values, k_period)
  
  # Calculate the %D line (SMA of the %K line)[cite: 24]
  d_line <- sma(k_line, d_period)
  
  # Return the %K and %D lines as a list[cite: 24]
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}

# Test example with sufficient data length for period = 14
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62, 58, 61, 64, 66, 70, 68, 72, 75, 73, 78)
stoch_rsi_result <- stoch_rsi(data, period = 14, k_period = 3, d_period = 3)
print(stoch_rsi_result)