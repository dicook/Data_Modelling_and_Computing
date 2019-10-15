

# https://projecteuler.net/problem=1

# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

# Show that the the sum of all the multiples of 3 or 5 below 1000 is 233168

euler001 <- function(n) {
  # Check all numbers divisible by 3 or 5 less than n
  cn = 0
  for(i in 1:n) {
    if((i %% 3 == 0) | (i %% 5 == 0)) {
      cn = cn + i
    }
  }
  return(cn)
}











# euler001a <- function(n) {
#   # Check all numbers divisible by 3 or 5 less than n (vectorised)
#   m3 = which((1:n) %% 3 == 0)
#   m5 = which((1:n) %% 5 == 0)
#   m = unique(c(m3, m5))
#   return(sum(m))
# }

# euler001b <- function(n) {
#   # Generate only the numbers divisible by 3 or 5 less than n
#   if(n < 3) {
#     return(0)
#   } else if(n < 5){
#     return(3)
#   } else{
#     m3 = seq(3, n, by = 3)
#     m5 = seq(5, n, by = 5)
#     m = unique(c(m3, m5))
#     return(sum(m))
#   }
# }