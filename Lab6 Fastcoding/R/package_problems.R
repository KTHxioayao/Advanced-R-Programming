library(parallel)
library(bigmemory)

#' Knapsack Dynamic Programming Solution
#'
#' This function solves the knapsack problem using dynamic programming.
#' It optimally selects items to fit within a limited weight capacity.
#'
#' @param x A dataframe where each row is an item with two columns: weight and value.
#' @param W The total weight capacity of the knapsack.
#'
#' @return The maximum value that can be achieved within the given weight limit.
#' @import parallel
#' @import bigmemory
#' @export
#' @examples
#' items <- data.frame(weight=c(1,2,3), value=c(6,10,12))
#' W <- 5
#' knapsack_dynamic(items, W)
knapsack_dynamic <- function(x, W) {
  start_time <- Sys.time()
  if (W == 0 || nrow(x) == 0 ) {
    return(0)
  }
  # max_list = rep(list(0), W)
  update_detail_df <- data.frame(matrix(0, nrow = nrow(x), ncol = W))
  max_df <- data.frame(matrix(0, nrow = nrow(x), ncol = W))
  for(i in 1:nrow(x)) {
    weight <- x[i,1]
    if (weight > W ) {
      update_detail_df[i,] <- unlist(rep(list(0), W))
      if (i == 1) {
        max_df[i,] <- unlist(rep(list(0), W))
        update_detail_df[i,] <- unlist(rep(list(0), W))
      }else{
        max_df[i,] <- max_df[i - 1,]
        update_detail_df[i,] <- max_df[i - 1,]
      }
      next
    }
    #cat(paste('current item:',i, "\n",sep=""))
    if (weight > 1) {
      # R is bad here, must a brackets to enclose 'weight - 1'
      update_detail_df[i,1:(weight -1)] <- unlist(max_df[i - 1,1:weight - 1])
    }
    if (i == 1){
      updated_max_list <- rep(list(0), W)
    }else {
      updated_max_list <- max_df[i - 1,]
    }
    for (j in weight:W) {
      value <- x[i,2]
      if (i == 1) {
        updated_max_list[[j]] <- value
        update_detail_df[i,j] <- value
        next
      }
      if (weight == j) {
        current_max_v <- max_df[i -1,j]
        new_max_value <- max(current_max_v,value)
        update_detail_df[i,j] <- new_max_value
      } else {
        pre_max_value <- max_df[i - 1 ,(j- weight)]
        new_value <-  pre_max_value + value
        new_max_value <- max(new_value,max_df[i - 1,j])
        update_detail_df[i,j] <- new_max_value
      }
      updated_max_list[[j]] <- new_max_value
    }
    max_df[i,] <- unlist(updated_max_list)
  }
  end_time <- Sys.time()
  execution_time <- end_time - start_time
  print(execution_time)
  return(update_detail_df[nrow(x),W])
}

#' Brute Force Knapsack Solution
#'
#' A brute-force approach to solve the knapsack problem by evaluating all possible combinations of items.
#'
#' @param x A dataframe with item weights and values.
#' @param W The maximum weight capacity.
#' @param parallel A logical value indicating if the operation should be parallelized.
#' @export
#' @return The best value that can be achieved within the weight limit.
#' @import parallel
#' @import bigmemory
#' @examples
#' items <- data.frame(weight=c(1,2,3), value=c(6,10,12))
#' W <- 5
#' brute_force_knapsack(items, W, parallel=FALSE)
brute_force_knapsack <- function(x, W, parallel=FALSE) {
  start_time <- Sys.time()
  weights <- x[,1]   # weights of the items
  values <- x[,2]    # value of the items
  capacity <- W             # max capacity of the knapsack
  n <- length(weights)  # number of items
  best_value <- 0       # store the max value
  best_combination <- NULL  # store the best combination
  if (parallel) {
    parallel_best_values <- parallel_brute_force_knapsack(weights,values,W,n)
    end_time <- Sys.time()
    execution_time <- end_time - start_time
    print(execution_time)
    return(parallel_best_values)
  }
  # generate all the possibilities (each item can be selected or not)
  for (i in 0:(2^n - 1)) {
    selected <- as.integer(intToBits(i))[1:n]
    # calculate the total weight and value of the selected items
    total_weight <- sum(selected * weights)
    total_value <- sum(selected * values)
    # if the combination is feasible and better than the current best solution
    if (total_weight <= capacity && total_value > best_value) {
      best_value <- total_value
      best_combination <- selected

    }
  }
  cat("best value sum:", best_value, "\n")
  #cat("best combination:", best_combination, "\n")
  end_time <- Sys.time()
  execution_time <- end_time - start_time
  print(execution_time)
  return(values)
}


# update_best_value_optimistic_loceker <- function(x,best_value_matrix,weights,values,W) {
#   repeat{
#     n <- length(weights)
#     selected <- as.integer(intToBits(x))[1:n]
#     selected_total_weight <- sum(selected * weights)
#     selected_total_value <- sum(selected * values)
#     version <- best_value_matrix[1,3]
#     if (selected_total_weight < W && version == best_value_matrix[1,3]) {
#       best_value_matrix[x + 1,1] <- selected_total_value
#       best_value_matrix[x + 1,2] <- selected_total_weight
#       best_value_matrix[x + 1,3] <- version + 1
#       break
#     }
#   }
# }


update_best_value <- function(x,best_value_matrix,weights,values,W) {
  n <- length(weights)
  selected <- as.integer(intToBits(x))[1:n]
  selected_total_weight <- sum(selected * weights)
  selected_total_value <- sum(selected * values)
  if (selected_total_weight < W) {
    best_value_matrix[x + 1,1] <- selected_total_value
  }
}


parallel_brute_force_knapsack <- function(weights,values,W,n) {
  #   n <- length(weights)#
    num_cores <- detectCores()
    best_value_matrix <- big.matrix(nrow = (2^n - 1),ncol = 1,init = 0)
    # best_value_matrix_optimistic <- big.matrix(nrow = 1,ncol = 3,init = 0)
    results <- mclapply(0:(2^n - 1),
                         update_best_value,
                        # update_best_value_optimistic_locker,
                         best_value_matrix = best_value_matrix,
                         #best_value_matrix = best_value_matrix_optimistic,
                         weights = weights,
                         values = values,
                         W = W,
                         mc.cores = 1)
     return(max(best_value_matrix[,1]))
  }
#' Greedy Knapsack Algorithm
#'
#' Solves the knapsack problem using a greedy algorithm that selects items based on the highest value-to-weight ratio.
#'
#' @param x A dataframe with item weights and values.
#' @param W The maximum weight capacity.
#' @export
#' @return The best value that can be achieved within the weight limit.
#' @examples
#' items <- data.frame(weight=c(1,2,3), value=c(6,10,12))
#' W <- 5
#' greedy_knapsack(items, W)
greedy_knapsack <- function(x, W) {
  start_time <- Sys.time()
  x$unit_value <- x[,2] / x[,1]
  sorted_x <- x[order(-x$unit_value), ] # default ascending order#
  selected_best_value <- 0
  selected_best_weight <- 0
  for (i in 1:nrow(sorted_x)) {
    predict_best_value <- selected_best_value + sorted_x[i,2]
    predict_best_weight <- selected_best_weight + sorted_x[i,1]
    if (predict_best_weight < W) {
      selected_best_value <- predict_best_value
      selected_best_weight <- predict_best_weight
    }else if(predict_best_weight > W) {
        end_time <- Sys.time()
        execution_time <- end_time - start_time
        print(execution_time)
        return(selected_best_value)
    }else{
      end_time <- Sys.time()
      execution_time <- end_time - start_time
      print(execution_time)
      return(predict_best_value)
    }
    end_time <- Sys.time()
    execution_time <- end_time - start_time
    cat(execution_time)
  }
}



