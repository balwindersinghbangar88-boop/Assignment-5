# Stochastic RSI (StochRSI) - base R only
# Depends on rsi() and sma() (loaded automatically if not already defined)
if (!exists("rsi", mode = "function")) source("rsi.R")
if (!exists("sma", mode = "function")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI (min/max ignore the leading NA values of the RSI)
  if (all(is.na(rsi_values))) {
    min_rsi <- NA_real_
    max_rsi <- NA_real_
  } else {
    min_rsi <- min(rsi_values, na.rm = TRUE)
    max_rsi <- max(rsi_values, na.rm = TRUE)
  }
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)

  # Calculate the %K line (StochRSI smoothed with an SMA)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  return(result)
}
