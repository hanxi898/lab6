brute_force_knapsack <- function(x, W) {
  if (!is.data.frame(x) || ncol(x) != 2 || any(x$w <= 0) || any(x$v < 0)) {
    stop("error")
  }
  
  n <- nrow(x)
  max_value <- 0
  best_combination <- integer(0)
  
  for (i in 0:(2^n - 1)) {
    current_weight <- 0
    current_value <- 0
    current_combination <- integer(0)
    
    for (j in 1:n) {
      if (bitwAnd(i, 2^(j - 1)) != 0) {
        current_weight <- current_weight + x$w[j]
        current_value <- current_value + x$v[j]
        current_combination <- c(current_combination, j)
      }
    }
    
    if (current_weight <= W && current_value > max_value) {
      max_value <- current_value
      best_combination <- current_combination
    }
  }
  
  return(list(value = max_value, elements = best_combination))
}
