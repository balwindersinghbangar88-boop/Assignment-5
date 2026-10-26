# Crossover: arr1 crosses ABOVE arr2 ("Up"); the pseudocode also reports "Down"/"None"
crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- rep("None", length(arr1))

  # Check for crossovers at each data point
  # (NA values, e.g. from indicator warm-up periods, give "None")
  for (i in seq_along(arr1)[-1]) {
    if (anyNA(c(arr1[i], arr2[i], arr1[i - 1], arr2[i - 1]))) {
      crossover_signals[i] <- "None"
    } else if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
      crossover_signals[i] <- "Up"
    } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossover_signals[i] <- "Down"
    } else {
      crossover_signals[i] <- "None"
    }
  }

  return(crossover_signals)
}
