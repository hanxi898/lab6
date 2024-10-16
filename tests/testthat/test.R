library(testthat)
setup({
  set.seed(42)
  knapsack_objects <<- data.frame(  
    w = sample(1:4000, size = 2000, replace = TRUE),
    v = runif(n = 2000, min = 0, max = 10000)
  )
})


library(lab6)  

test_that("brute_force_knapsack works correctly", {
  result <- brute_force_knapsack(knapsack_objects[1:8,], 3500)
  expect_equal(result$value, 16770.3, tolerance = 0.1)
  
})


test_that("greedy_knapsack works correctly", {
  result <- greedy_knapsack(knapsack_objects[1:800,], 3500)
  expect_true(result$value <= 200000)
})
