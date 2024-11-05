library(testthat)
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)


library(lab6)  
test_that("Correct object is returned", {

  expect_silent(dk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))  
  expect_named(dk, c("value", "elements"))
  
  expect_type(dk$value, "double")
  

  expect_type(dk$elements, "integer")
})

test_that("functions reject erroneous input", {
  
  expect_error(knapsack_dynamic("hej", 3500))  
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))  
})

test_that("Function returns reasonable results", {
  
  dk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)  
  expect_type(dk$value, "double")
  expect_type(dk$elements, "integer")
  
  
  expect_true(dk$value > 0)
  
 
  expect_true(all(dk$elements >= 1 & dk$elements <= 8))
  
  dk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)  
  expect_type(dk$value, "double")
  expect_type(dk$elements, "integer")
  expect_true(dk$value > 0)
  expect_true(all(dk$elements >= 1 & dk$elements <= 12))
  
  dk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)  
  expect_type(dk$value, "double")
  expect_type(dk$elements, "integer")
  expect_true(dk$value > 0)
  expect_true(all(dk$elements >= 1 & dk$elements <= 8))
  
  dk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)  
  expect_type(dk$value, "double")
  expect_type(dk$elements, "integer")
  expect_true(dk$value > 0)
  expect_true(all(dk$elements >= 1 & dk$elements <= 12))
  
  
  st <- system.time(dk <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))  
  expect_true(as.numeric(st)[2] <= 1)  
})


test_that("Larger dataset results are reasonable", {
  dk <- knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)  
  expect_type(dk$value, "double")
  expect_type(dk$elements, "integer")
  expect_true(dk$value > 0)
  expect_true(all(dk$elements >= 1 & dk$elements <= 800))
  
  dk <- knapsack_dynamic(x = knapsack_objects[1:1200,], W = 3500)  
  expect_type(dk$value, "double")
  expect_type(dk$elements, "integer")
  expect_true(dk$value > 0)
  expect_true(all(dk$elements >= 1 & dk$elements <= 1200))
})
