RNGversion(min(as.character(getRversion()), "4.4.1"))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

generate_knapsack_objects <- function(n) {
  knapsack_objects <- data.frame(
    w = sample(1:4000, size = n, replace = TRUE),
    v = runif(n = n, 0, 10000)
  )
  return(knapsack_objects)
}
