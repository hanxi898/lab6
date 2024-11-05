library(testthat)
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)


library(lab6)  

test_that("Correct object is returned", {
  # Ensure the function runs silently and returns an object with the correct structure
  expect_silent(bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(bfk, c("value", "elements"))
  
  # Check that value is a double type (numeric in R is double)
  expect_type(bfk$value, "double")
  expect_type(bfk$elements, "integer")
})

test_that("Functions reject erroneous input", {
  # Ensure that the function throws an error when the input is incorrect
  expect_error(brute_force_knapsack("hej", 3500), "Input 'x' must be a data frame with columns 'w' and 'v'")
  expect_error(brute_force_knapsack(x = knapsack_objects[1:8,], W = -3500), "Invalid weight capacity. W must be positive.")
})

test_that("Function returns reasonably correct results", {
  # Check that the value is a positive number and the elements are reasonable
  bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_true(bfk$value >= 0)  # Ensure value is non-negative
  expect_true(all(bfk$elements > 0 & bfk$elements <= 8))  # Ensure selected items are within the correct range
  
  bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_true(bfk$value >= 0)
  expect_true(all(bfk$elements > 0 & bfk$elements <= 12))
  
  bfk <- brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_true(bfk$value >= 0)
  expect_true(all(bfk$elements > 0 & bfk$elements <= 8))
  
  bfk <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_true(bfk$value >= 0)
  expect_true(all(bfk$elements > 0 & bfk$elements <= 12))
  
  # Timing test to ensure it runs reasonably fast
  st <- system.time(bfk <- brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[3] <= 2)  # Reasonable running time (2 seconds for example)
})