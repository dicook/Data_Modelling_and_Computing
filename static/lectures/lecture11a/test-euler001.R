
library(here)
library(microbenchmark)
library(testthat)

source(file.path(here(), "euler001.R"))

test_that("Sum of multiples of 3 and 5 for integers less than 'n'",{
  expect_that(euler001(n = 1), equals(0))
  expect_that(euler001(n = 2), equals(0))
  expect_that(euler001(n = 3), equals(3))
  expect_that(euler001(n = 4), equals(3))
  expect_that(euler001(n = 5), equals(3 + 5))
  expect_that(euler001(n = 6), equals(3 + 5 + 6))
  expect_that(euler001(n = 7), equals(3 + 5 + 6))
  expect_that(euler001(n = 8), equals(3 + 5 + 6))
  expect_that(euler001(n = 9), equals(3 + 5 + 6 + 9))
  expect_that(euler001(n = 10), equals(3 + 5 + 6 + 9 + 10))
  expect_that(euler001(n = 999), equals(233168))
})



# test_that("Multiples of 3 and 5 for '1:n' - Faster Version",{
#   for(i in 1:20) {
#     expect_that(euler001a(i), equals(euler001(i)))
#   }
#   expect_that(euler001a(999), equals(euler001(999)))
  
#   run_times = microbenchmark(euler001(10000), euler001a(10000))
#   # Mean run-times euler001
#   t001 = mean(run_times$time[run_times$expr == 'euler001(10000)'])
#   # Mean run-times euler001a
#   t001a = mean(run_times$time[run_times$expr == 'euler001a(10000)'])
#   expect_that(t001, is_more_than(5*t001a))
# })

# test_that("Multiples of 3 and 5 for 'n' - Even faster Version",{
#   run_times = microbenchmark(euler001(10000), euler001b(10000))
#   t001 = mean(run_times$time[run_times$expr == 'euler001(10000)'])
#   t001b = mean(run_times$time[run_times$expr == 'euler001b(10000)'])
#   expect_that(t001, is_more_than(10*t001b))
  
#   for(i in 1:20) {
#     expect_that(euler001b(i), equals(euler001(i)))
#   }
#   expect_that(euler001b(999), equals(euler001(999)))
# })