% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package_problems.R
\name{brute_force_knapsack}
\alias{brute_force_knapsack}
\title{Brute Force Knapsack Solution}
\usage{
brute_force_knapsack(x, W, parallel = FALSE)
}
\arguments{
\item{x}{A dataframe with item weights and values.}

\item{W}{The maximum weight capacity.}

\item{parallel}{A logical value indicating if the operation should be parallelized.}
}
\value{
The best value that can be achieved within the weight limit.
}
\description{
A brute-force approach to solve the knapsack problem by evaluating all possible combinations of items.
}
\examples{
items <- data.frame(weight=c(1,2,3), value=c(6,10,12))
W <- 5
brute_force_knapsack(items, W, parallel=FALSE)
}
