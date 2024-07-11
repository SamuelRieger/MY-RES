library(MASS)
library(mvtnorm)
library(ggplot2)

set.seed(1)
beta_true <- c(1, 2)
N <- 100
p <- 2 # Length of beta
X <- matrix(rnorm(N*p), N, p)
y <- X%*%beta_true + rnorm(N, 0, 1)

get_grad_U <- function(beta, X, y, N, n) {
  grad_log_prior <- beta
  grad_log_like <- t(X)%*%(y - X%*%beta)
  grad_log_post <- grad_log_prior + N/n * grad_log_like
  return(grad_log_post)
}

sgld <- function(X, y, n_iter, step_size, batch_size) {
  N <- nrow(X)
  n <- batch_size
  p <- ncol(X)
  
  beta <- rep(0, p)
  
  samples <- matrix(0, n_iter, p)
  
  for (t in 1:n_iter) {
    indicies <- sample(1:N, n)
    X_batch <- X[indicies, ]
    y_batch <- y[indicies]
    
    grad_log_post <- get_grad_U(beta, X_batch, y_batch, N, n)
    
    epsilon_t <- step_size
    eta <- rnorm(p, 0, sqrt(epsilon_t))
    
    delta <- epsilon_t/2 * grad_log_post + eta
    
    beta <- beta + delta
    
    samples[t, ] <- beta
  }
  return(samples)
}

ULA <- function(X, y, n_iter, step_size) {
  
  p <- ncol(X)
  
  beta <- rep(0, p)
  
  samples <- matrix(0, n_iter, p)
  
  for (t in 1:n_iter) {
    
    grad_log_post <- get_grad_U(beta, X, y, N, N)
    
    epsilon_t <- step_size
    
    eta <- rnorm(p, 0, sqrt(epsilon_t))
    
    delta <- epsilon_t/2 * grad_log_post + eta
    
    beta <- beta + delta
    
    samples[t, ] <- beta
  }
  return(samples)
}

#__ULA__#
n_iter <- 10^5
step_size_1 <- .003
start.time <- Sys.time()
samples_ULA_1 <- ULA(X, y, n_iter, step_size_1)
step_size_2 <- .00001
samples_ULA_2 <- ULA(X, y, n_iter, step_size_2)
step_size_3 <- 0.000001
samples_ULA_3 <- ULA(X, y, n_iter, step_size_3)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# The exact posterior (a rare case where you can find this)
Cov_post <- solve(t(X)%*%X + diag(1,p,p))
mu_post <- Cov_post %*% t(X)%*%y
exact_post <- mvtnorm::rmvnorm(10^4, mean = mu_post, sigma = Cov_post)

par(mfrow=c(1,3), mar=c(2,2,2,2))
plot(exact_post, col = 2, pch=20, xlab = "", ylab = "", main = sprintf("Step Size = %g", step_size_1))
points(samples_ULA_1, pch=".")
plot(exact_post, col = 2, pch=20, xlab = "", ylab = "", main = sprintf("Step Size = %g", step_size_2))
points(samples_ULA_2, pch=".")
plot(exact_post, col = 2, pch=20, xlab = "", ylab = "", main = sprintf("Step Size = %g", step_size_3))
points(samples_ULA_3, pch=".")

## If you want to see estimates of the parameters take a look at the posterior mean
colMeans(samples_ULA_2) 
beta_true

#__SGLD__#
batch_size <- 10
step_size_1 <- .001
start.time <- Sys.time()
samples_SGLD_1 <- sgld(X, y, n_iter, step_size_1, batch_size)
step_size_2 <- .00001
samples_SGLD_2 <- sgld(X, y, n_iter, step_size_2, batch_size)
step_size_3 <- 0.000001
samples_SGLD_3 <- sgld(X, y, n_iter, step_size_3, batch_size)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

par(mfrow=c(1,3), mar=c(2,2,2,2))
plot(exact_post, col = 2, pch=20, xlab = "", ylab = "", main = sprintf("Step Size = %g", step_size_1))
points(samples_SGLD_1, pch=".")
plot(exact_post, col = 2, pch=20, xlab = "", ylab = "", main = sprintf("Step Size = %g", step_size_2))
points(samples_SGLD_2, pch=".")
plot(exact_post, col = 2, pch=20, xlab = "", ylab = "", main = sprintf("Step Size = %g", step_size_3))
points(samples_SGLD_3, pch=".")

colMeans(samples_SGLD_2) 
beta_true