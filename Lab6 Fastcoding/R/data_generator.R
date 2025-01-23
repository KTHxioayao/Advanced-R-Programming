data_generator <- function(){
  RNGversion(min(as.character(getRversion()),"3.5.3"))
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  n <- 2000
  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=runif(n = n, 0, 10000)
    )
  return(knapsack_objects)
}



small_data_generator <- function() {

  knapsack_objects <-
    data.frame(
      w = c(1,4,3,1),
      v = c(1500,3000,2000,2000)
    )
  return(knapsack_objects)



}
