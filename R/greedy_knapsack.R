#' Greedy Knapsack Solver
#'
#' This function solves the knapsack problem using a greedy approach.
#' It selects items based on the value-to-weight ratio and attempts to fit as many items 
#' as possible without exceeding the weight limit.
#'
#' @param x A data frame with two columns: `w` for item weights and `v` for item values.
#' @param W An integer representing the maximum weight capacity of the knapsack.
#'
#' @return A list with two components:
#' \itemize{
#'   \item `value`: The total value of the selected items.
#'   \item `elements`: A vector of indices representing the selected items.
#' }
#' @name greedy_knapsack
#' @examples
#' knapsack_objects <- generate_knapsack_objects(10)
#' greedy_knapsack(knapsack_objects, W = 3500)
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

greedy_knapsack <- function(x, W) {

  x <- na.omit(x)

  x <- x[order(x$v / x$w, decreasing = TRUE), ]
  
  total_value <- 0
  total_weight <- 0
  selected_items <- integer(0)

  for (i in 1:nrow(x)) {

    if (total_weight + x$w[i] <= W) {
      total_weight <- total_weight + x$w[i]
      total_value <- total_value + x$v[i]
      selected_items <- c(selected_items, i)
    }
  }
  
  return(list(value = total_value, elements = selected_items))
}

knapsack_objects <- generate_knapsack_objects(800)
greedy_knapsack(x = knapsack_objects, W = 3500)
