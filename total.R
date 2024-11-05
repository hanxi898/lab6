usethis::use_testthat()
install.packages("profvis")
library(profvis)             
profvis({
  result <- brute_force_knapsack(knapsack_objects[1:12,], W = 3500)  
})
usethis::use_github_action_check_standard()
usethis::use_vignette("vignette")
devtools::build_vignettes()
