# Moving Average Convergence Divergence (MACD) function
macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  
  # Calculate the MACD line (the difference between short_ema and long_ema)
  macd_line <- short_ema - long_ema
  
  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line
  
  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}

# Test example (from assignment instructions)
data <- c(100, 105, 110, 115, 120, 125, 130)
macd_result <- macd(data, short_period = 3, long_period = 5, signal_period = 2)
print(macd_result)