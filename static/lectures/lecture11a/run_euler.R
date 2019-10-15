library(here)
library(microbenchmark)
library(testthat)

source(file.path(here(), "euler001.R"))

# Benchmarking
n = 1e5
mean(1:n); sum(1:n)/length(1:n)
microbenchmark(mean(1:n), sum(1:n)/length(1:n))
microbenchmark(euler001(n), euler001a(n))
# microbenchmark(euler001(n), euler001a(n), euler001b(n))

# Testing 
my_test_file = file.path(here(), "test-euler001.R")
test_file(my_test_file)


# n = 999
# euler001(n)
# euler001a(n)
# euler001b(n)

