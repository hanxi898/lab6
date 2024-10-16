#' Brute Force Knapsack Solver
#'
#' This function solves the knapsack problem using a brute force approach.
#' It checks all possible combinations of items and returns the one with the highest value
#' without exceeding the weight limit.
#'
#' @param x A data frame with two columns: `w` for item weights and `v` for item values.
#' @param W An integer representing the maximum weight capacity of the knapsack.
#'
#' @return A list with two components:
#' \itemize{
#'   \item `value`: The maximum value of items that can be carried without exceeding the weight.
#'   \item `elements`: A vector of indices representing the selected items.
#' }
#' @name brute_force_knapsack
#' @examples
#' knapsack_objects <- generate_knapsack_objects(8)
#' brute_force_knapsack(knapsack_objects, W = 3500)
#'
#' @export
RNGkind("Mersenne-Twister")  
set.seed(42)

generate_knapsack_objects <- function(n) {
  knapsack_objects <- data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, 0, 10000)
  )
  return(knapsack_objects)
}
knapsack_objects <- generate_knapsack_objects(2000)


print(knapsack_objects)

brute_force_knapsack <- function(x, W, parallel = TRUE) {
  if (!is.data.frame(x) || !all(c("v", "w") %in% names(x))) {
    stop("False")
  }
  
  if (any(x$v <= 0) || any(x$w <= 0)) {
    stop("False")
  }
  
  n <- nrow(x)
  max_value <- 0
  best_combination <- NULL
  
  for (i in 0:(2^n - 1)) {
    combination <- as.logical(intToBits(i)[1:n])
    
    total_weight <- sum(x$w[combination])
    total_value <- sum(x$v[combination])
    
    if (total_weight <= W && total_value > max_value) {
      max_value <- total_value
      best_combination <- combination
    }
  }
  
  return(list(
    value = max_value,
    elements = which(best_combination)
  ))
}

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,parallel = TRUE)