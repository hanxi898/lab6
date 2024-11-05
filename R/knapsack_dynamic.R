#' Dynamic Programming Knapsack Solver
#'
#' This function solves the knapsack problem using dynamic programming.
#' It computes the optimal value and selected items for a given weight limit.
#'
#' @param x A data frame with two columns: `w` for item weights and `v` for item values.
#' @param W An integer representing the maximum weight capacity of the knapsack.
#'
#' @return A list with two components:
#' \itemize{
#'   \item `value`: The maximum value of items that can be carried without exceeding the weight.
#'   \item `elements`: A vector of indices representing the selected items.
#' }
#'
#' @examples
#' knapsack_objects <- generate_knapsack_objects(500)
#' knapsack_dynamic(knapsack_objects, W = 3500)
#' @name knapsack_dynamic
#' @export



knapsack_dynamic <- function(x, W) {
  n <- nrow(x)
  
  dp <- matrix(0, nrow = n + 1, ncol = W + 1)
  
  for (i in 1:n) {
    for (w in 1:W) {
      if (x$w[i] <= w) {
        dp[i + 1, w + 1] <- max(dp[i, w + 1], dp[i, w + 1 - x$w[i]] + x$v[i])
      } else {
        dp[i + 1, w + 1] <- dp[i, w + 1]
      }
    }
  }
  
  selected_items <- integer(0)
  w <- W
  for (i in n:1) {
    if (dp[i + 1, w + 1] != dp[i, w + 1]) {
      selected_items <- c(selected_items, i)
      w <- w - x$w[i]
    }
  }
  
  return(list(value = dp[n + 1, W + 1], elements = selected_items))
}

n <- 500
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

W <- 3500

start_time <- Sys.time()
result <- knapsack_dynamic(knapsack_objects, W)
end_time <- Sys.time()

cat("Running time:", end_time - start_time, "\n")