#' Generate Knapsack Objects
#'
#' This function generates a data frame with random weights and values for knapsack items.
#' The number of items is specified by the user.
#'
#' @param n An integer specifying the number of knapsack objects to generate. Default is 2000.
#'
#' @return A data frame with two columns: `w` (weights) and `v` (values) of the knapsack items.
#' 
#' @examples
#' generate_knapsack_objects(10)
#' @name generate_knapsack_objects
#' @export
RNGkind("Mersenne-Twister")  
set.seed(42)


generate_knapsack_objects <- function(n = 2000) {
  set.seed(42) 
  knapsack_objects <- data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, min = 0, max = 10000)
  )
  return(knapsack_objects)
}

knapsack_objects <- generate_knapsack_objects(2000)


print(knapsack_objects)
