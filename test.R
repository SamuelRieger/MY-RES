library(MASS)
library(mvtnorm)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data
N <- 100  # number of data points
p <- 2    # number of features
X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))  # design matrix with intercept
beta_true <- c(2, -1.5)  # true coefficients
sigma <- 1  # standard deviation of noise
y <- X %*% beta_true + rnorm(n, mean = 0, sd = sigma)  # response variable

grad_log_posterior <- function(beta) {
  res <- sum(dnorm(y, x%*%beta, 1, log = TRUE))
  res <- sum(res + dnorm(beta, 0, 1, log = TRUE))
}

sgld <- function(X, y, n_iter = 1000, step_size = 1e-4, noise_scale = 1e-2) {
  n <- nrow(X)
  p <- ncol(X)
  
  # Initialize coefficients
  beta <- rep(0, p)
  
  # Store samples
  samples <- matrix(0, nrow = n_iter, ncol = p)
  
  # SGLD iterations
  for (iter in 1:n_iter) {
    # Randomly select a mini-batch
    batch_size <- 10
    indices <- sample(1:n, batch_size)
    X_batch <- X[indices, ]
    y_batch <- y[indices]
    
    # Compute gradient of log-likelihood
    y_pred <- X_batch %*% beta
    grad_log_likelihood <- -t(X_batch) %*% (y_batch - y_pred)
    
    # Compute gradient of log-prior
    grad_log_prior <- beta
    
    # Compute total gradient of log-posterior
    grad_log_posterior <- (n / batch_size) * (grad_log_likelihood + grad_log_prior)
    
    # Update coefficients with SGLD
    noise <- rnorm(p, mean = 0, sd = noise_scale)
    beta <- beta + step_size * grad_log_posterior + noise
    
    # Store the sample
    samples[iter, ] <- beta
  }
  
  return(samples)
}

# Run SGLD
n_iter <- 10000
step_size <- 1e-5
samples <- sgld(X, y, n_iter = n_iter, step_size = step_size)

# Plot the sampled coefficients
plot(samples[, 1], type = "l", col = "blue", ylab = "Coefficient Value", xlab = "Iteration", main = "Trace Plot of SGLD Samples")
lines(samples[, 2], col = "red")
legend("topright", legend = c("Intercept", "Slope"), col = c("blue", "red"), lty = 1)

# Estimate the mean of the sampled coefficients
beta_est <- colMeans(samples[(n_iter/2):n_iter, ])
beta_est

# Compare with true coefficients
beta_true
