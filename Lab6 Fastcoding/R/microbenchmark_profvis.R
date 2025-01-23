library(microbenchmark)
library(ggplot2)
library(profvis)
library(compiler)
source("R/package_problems.R")
# compare for method and vectorized method
# compare the performance of the parallel and non-parallel brute force knapsack algorithms
knapsack_objects <- data_generator()

knapsack_res <-  microbenchmark(
  non_parallel_res = brute_force_knapsack(knapsack_objects[1:10,], 2000, parallel = FALSE),
  parallel_res = brute_force_knapsack(knapsack_objects[1:10,], 2000, parallel = TRUE),
  dynamic_res = knapsack_dynamic(knapsack_objects[1:10,], 2000),
  times = 2
)
autoplot(knapsack_res)



profvis({

  brute_force_knapsack(knapsack_objects[1:23,], 2000, parallel = TRUE)
})


profvis({

  knapsack_dynamic(knapsack_objects[1:20,], 2000)
})



fast_knapsack_dynamic <- cmpfun(knapsack_dynamic)
profvis({

  fast_knapsack_dynamic(knapsack_objects[1:20,], 2000)
})

