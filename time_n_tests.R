library(ggplot2)
library(mvtnorm)
library(gsubfn)
library(purrr)

source("sgld.R")
source("ksd.R")

generate_sample_data <- function(beta = c(1, 2), N = 100) {
  p <- length(beta)
  X <- matrix(rnorm(N*p), N, p)
  y <- X%*%beta + rnorm(N, 0, 1)
  
  return(list(X, y))
}

beta_true <- c(1, 2)
p <- length(beta_true)
N <- 1000
list[X, y] <- generate_sample_data(beta_true, N)
n_iter <- 2e3 # Throw away the first 1000 as burn in samples

# TEST PARAMS #
test_number <- 20
n_tests <- seq(10, N, length.out = test_number)

# UN-OPTIMISED h #
time_tests <- rep(0, test_number)
for (i in 1:test_number) {
  n <- n_tests[i]
  h <- 3.5e-3
  cat("Time n =", n,"\n")
  start.time <- Sys.time()
  for (j in 1:100) {
    sgld(X, y, n_iter, h, n)
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  time_tests[i] <- as.numeric(time.taken)
}

df_time_tests <- data.frame(n = n_tests, time = time_tests)
saveRDS(df_time_tests, "df_time_tests.rds")
