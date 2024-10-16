usethis::use_testthat()
install.packages("profvis")
library(profvis)             
profvis({
  result <- brute_force_knapsack(knapsack_objects[1:12,], W = 3500)  
})
