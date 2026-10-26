# Crossunder: arr1 crosses BELOW arr2 (signals follow the assignment pseudocode)
crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- rep("None", length(arr1))   # first element is always "None"

  # Check for crossunder signals at each data point
  # (NA values, e.g. from indicator warm-up periods, give "False")
  for (i in seq_along(arr1)[-1]) {
    if (anyNA(c(arr1[i], arr2[i], arr1[i - 1], arr2[i - 1]))) {
      crossunder_signals[i] <- "False"
    } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- "True"
    } else {
      crossunder_signals[i] <- "False"
    }
  }

  return(crossunder_signals)
}
