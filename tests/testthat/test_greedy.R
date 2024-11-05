library(testthat)
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)


library(lab6) 

test_that("Correct object is returned", {
  
  expect_silent(gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
  

  expect_type(gk$value, "double")
  
 
  expect_type(gk$elements, "integer")
})

test_that("functions reject erroneous input", {
  
  expect_error(greedy_knapsack("hej", 3500))
  expect_error(greedy_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function returns reasonable results", {
  
  gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_type(gk$value, "double")
  expect_type(gk$elements, "integer")
  

  expect_true(gk$value > 0)
  
  
  expect_true(all(gk$elements >= 1 & gk$elements <= 8))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_type(gk$value, "double")
  expect_type(gk$elements, "integer")
  expect_true(gk$value > 0)
  expect_true(all(gk$elements >= 1 & gk$elements <= 12))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_type(gk$value, "double")
  expect_type(gk$elements, "integer")
  expect_true(gk$value > 0)
  expect_true(all(gk$elements >= 1 & gk$elements <= 8))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_type(gk$value, "double")
  expect_type(gk$elements, "integer")
  expect_true(gk$value > 0)
  expect_true(all(gk$elements >= 1 & gk$elements <= 12))
  
  
  st <- system.time(gk <- greedy_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)  
})


test_that("Larger dataset results are reasonable", {
  gk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_type(gk$value, "double")
  expect_type(gk$elements, "integer")
  expect_true(gk$value > 0)
  expect_true(all(gk$elements >= 1 & gk$elements <= 800))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 3500)
  expect_type(gk$value, "double")
  expect_type(gk$elements, "integer")
  expect_true(gk$value > 0)
  expect_true(all(gk$elements >= 1 & gk$elements <= 1200))
})
