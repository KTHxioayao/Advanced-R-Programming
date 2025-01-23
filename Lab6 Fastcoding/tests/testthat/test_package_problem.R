library(testthat)
library(parallel)
library(bigmemory)
library(lab6)



test_that("knapsack_dynamic works as expected", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  test_data <- knapsack_objects[1:8,]
  result <- knapsack_dynamic(test_data, max_capacity)
  expect_type(result, "double")  # 返回值应该是数值型
  expect_true(result >= 0)  # 结果应该是非负数
})


test_that("brute_force_knapsack works as expected (non-parallel)", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  test_data <- knapsack_objects[1:8,]
  result <- brute_force_knapsack(test_data, max_capacity, parallel = FALSE)
  expect_type(result, "double")  # 返回值应该是数值型
  expect_true(sum(result) > 0)  # 价值应该大于0
})

test_that("brute_force_knapsack works as expected (parallel)", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  test_data <- knapsack_objects[1:8,]
  result <- brute_force_knapsack(test_data, max_capacity, parallel = TRUE)
  expect_type(result, "double")  # 返回值应该是数值型
  expect_true(sum(result) > 0)  # 价值应该大于0
})


test_that("greedy_knapsack works as expected", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  test_data <- knapsack_objects[1:8,]
  result <- greedy_knapsack(test_data, max_capacity)
  expect_type(result, "double")  # 返回值应该是数值型
  expect_true(result >= 0)  # 贪心算法结果应该是非负数
})


test_that("knapsack_dynamic handles edge cases", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  empty_data <- data.frame(weight = numeric(0), value = numeric(0))
  result <- knapsack_dynamic(empty_data, max_capacity)
  expect_equal(result, 0)
  large_weights <- knapsack_objects[1:12,]
  result <- knapsack_dynamic(large_weights, max_capacity)
  expect_equal(round(result), 15428)
})

test_that("brute_force_knapsack handles small data sets", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  small_data <- small_data_generator()
  result <- brute_force_knapsack(small_data, 10, parallel = FALSE)
  expect_equal(sum(result), 8500)
})

test_that("parallel_brute_force_knapsack works with larger data sets", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  large_data <-  knapsack_objects[1:12,]
  result <- brute_force_knapsack(large_data, 3500, parallel = TRUE)
  expect_true(sum(result) > 0)
})

test_that("greedy_knapsack returns expected results with sorted input", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  sorted_data <- data.frame(weight = c(1, 2, 3), value = c(10, 20, 30))
  result <- greedy_knapsack(sorted_data, 3)
  expect_equal(result, 30)
})

test_that("greedy_knapsack handles all items under capacity", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  easy_data <-small_data_generator()
  result <- greedy_knapsack(easy_data, 4)
  expect_equal(result, 3500)
})

test_that("knapsack algorithms handle extreme capacities", {
  knapsack_objects <- suppressWarnings(data_generator())
  max_capacity <- 2000
  result <- knapsack_dynamic(test_data, 0)  # 0容量时
  expect_equal(result, 0)
  test_data <- knapsack_objects[1:8,]
  result <- brute_force_knapsack(test_data, 2000, parallel = FALSE)  # 超大容量时
  expect_true(sum(result) > 0)
})
