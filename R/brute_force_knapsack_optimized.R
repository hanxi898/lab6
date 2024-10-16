#' Brute Force Knapsack Algorithm
#'
#' This function solves the knapsack problem using a brute force approach.
#' It iterates over all possible combinations of items and selects the one
#' with the highest total value without exceeding the specified weight limit.
#'
#' @param x A data frame containing two columns: `v` (values) and `w` (weights).
#'          Both `v` and `w` should be positive numeric values.
#' @param W A numeric value representing the maximum allowed weight of the knapsack.
#' @param parallel A logical value (default is FALSE). If TRUE, the function will parallelize
#'                 the computation across available cores to speed up the process.
#'
#' @return A list containing two elements:
#'   \item{value}{The total value of the selected items.}
#'   \item{elements}{A vector of indices representing the selected items.}
#'
#' @examples
#' # Example usage with a small dataset
#' knapsack_objects <- data.frame(
#'   w = sample(1:100, 10, replace = TRUE),
#'   v = runif(10, min = 1, max = 100)
#' )
#' result <- brute_force_knapsack(knapsack_objects, W = 500, parallel = TRUE)
#' print(result)
#' @name brute_force_knapsack_optimized
#' @import future.apply
#' @importFrom future plan multisession
#' @export

library(future.apply)

brute_force_knapsack_optimized <- function(x, W, parallel = FALSE) {
 
  if (!is.data.frame(x) || !all(c("v", "w") %in% names(x))) {
    stop("Invalid input data: 'x' must be a data frame with columns 'v' and 'w'.")
  }
  
  if (any(x$v <= 0) || any(x$w <= 0)) {
    stop("Invalid input data: 'v' and 'w' must be positive.")
  }
  
  n <- nrow(x)
  max_value <- 0
  best_combination <- NULL
  

  if (parallel) {

    plan(multisession)
    

    result <- future_lapply(0:(2^n - 1), function(i) {
      combination <- as.logical(intToBits(i)[1:n])
      total_weight <- sum(x$w[combination])
      total_value <- sum(x$v[combination])
      
      if (total_weight <= W && total_value > max_value) {
        max_value <<- total_value
        best_combination <<- combination
      }
    })
    
  } else {

    for (i in 0:(2^n - 1)) {
      combination <- as.logical(intToBits(i)[1:n])
      total_weight <- sum(x$w[combination])
      total_value <- sum(x$v[combination])
      
      if (total_weight <= W && total_value > max_value) {
        max_value <- total_value
        best_combination <- combination
      }
    }
  }
  
  return(list(
    value = max_value,
    elements = which(best_combination)
  ))
}
options(repos = c(CRAN = "https://cloud.r-project.org/"))
file.path(Sys.getenv("HOME"), ".Rprofile")
getOption("repos")

