## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(KnapsackPack)

# Example data
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 1e6
knapsack_objects <-
data.frame(
w=sample(1:4000, size = n, replace = TRUE),
v=runif(n = n, 0, 10000)
)


## -----------------------------------------------------------------------------
# Runtime for 16 objects
system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))

## -----------------------------------------------------------------------------
# Runtime for 500 objects
system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 2000))

## -----------------------------------------------------------------------------
# Runtime for 1 000 000 objects
system.time(greedy_knapsack(x = knapsack_objects, W = 2000))

## -----------------------------------------------------------------------------
# Non parallel
system.time(brute_force_knapsack(x = knapsack_objects[1:20,], W = 2000, parallel=FALSE))

# Parallel
system.time(brute_force_knapsack(x = knapsack_objects[1:20,], W = 2000, parallel=TRUE))

