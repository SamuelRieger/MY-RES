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

n <- 10
# h <- (9.5e-3*sqrt(n))/N
h <- 3.5e-3

# The exact posterior (a rare case where you can find this)
Cov_post <- solve(t(X)%*%X + diag(1,p,p))
mu_post <- Cov_post %*% t(X)%*%y
exact_post <- mvtnorm::rmvnorm(10^4, mean = mu_post, sigma = Cov_post)

list[samples, grad_samples] <- sgld(X, y, n_iter, h, n, TRUE)
imqKSD(samples[1001:n_iter,], grad_samples[1001:n_iter,])

plot(exact_post, col = 2, pch=20, xlab = "", ylab = "", main = sprintf("Step Size = %g", h))
points(samples, pch=".")

# TEST PARAMS #
test_number <- 20
N_tests <- unlist(map(2:10, ~ 10^(.x)))
n_tests <- seq(10, N, length.out = test_number)

# OPTIMISED h #
ksd_tests_optimised <- rep(0, test_number)
for (i in 1:test_number) {
  n <- n_tests[i]
  h <- (9.5e-3*sqrt(n))/N
  average_run_number <- 10
  ksd_total <- 0
  cat("Optimised n =", n,"\n")
  for (j in 1:average_run_number) {
    list[samples_j, grad_samples_j] <- sgld(X, y, n_iter, h, n, TRUE)
    ksd_total <- ksd_total + imqKSD(samples_j[1001:n_iter,], grad_samples_j[1001:n_iter,])
  }
  ksd_tests_optimised[i] <- ksd_total/average_run_number
}

df_optimised_tests <- data.frame(n = n_tests, ksd = ksd_tests_optimised)
saveRDS(df_optimised_tests, "df_optimised_tests.rds")

# UN-OPTIMISED h #
ksd_tests_unoptimised <- rep(0, test_number)
for (i in 1:test_number) {
  n <- n_tests[i]
  h <- 3.5e-3
  average_run_number <- 10
  ksd_total <- 0
  cat("Un-Optimised n =", n,"\n")
  for (j in 1:average_run_number) {
    list[samples_j, grad_samples_j] <- sgld(X, y, n_iter, h, n, TRUE)
    ksd_total <- ksd_total + imqKSD(samples_j[1001:n_iter,], grad_samples_j[1001:n_iter,])
  }
  ksd_tests_unoptimised[i] <- ksd_total/average_run_number
}

df_unoptimised_tests <- data.frame(n = n_tests, ksd = ksd_tests_unoptimised)
saveRDS(df_unoptimised_tests, "df_unoptimised_tests.rds")

plot(df_unoptimised_tests$n, df_unoptimised_tests$ksd, col = "blue")
points(df_optimised_tests$n, df_optimised_tests$ksd, col ="red")
