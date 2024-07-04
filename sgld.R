library(MASS)
library(mvtnorm)
library(ggplot2)

beta_true <- c(1, 2)
N <- 100
p <- 2 # Length of beta
X <- matrix(rnorm(N*p), N, p)
y <- X%*%beta + rnorm(N, 0, 1)

get_grad_U <- function(beta, X, y, N, n) {
  # res <- sum(dnorm(y, x%*%beta, 1, log = TRUE))
  # res <- sum(res + dnorm(beta, 0, 1, log = TRUE))
  
  grad_log_prior <- beta
  grad_log_like <- t(X)%*%(y - X%*%beta)
  grad_log_post <- grad_log_prior + N/n * grad_log_like
  return(grad_log_post)
}

## For checking derivatives (later)
# (grad_log(c(0,0))-log_pi_dist(c(0.01,0)))/0.01

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
    
    epsilon_t <- step_size / t # Decaying learning rate
    eta <- rnorm(p, 0, sqrt(epsilon_t))
    
    delta <- epsilon_t/2 * grad_log_post + eta
    
    beta <- beta + delta
    
    samples[t, ] <- beta
    
  }
  
  return(samples)
}

n_iter <- 10000
step_size <- 1e-4
batch_size <- 10
samples <- sgld(X, y, n_iter, step_size, batch_size)

samples_df <- as.data.frame(samples)
colnames(samples_df) <- c("Intercept", "Slope")
samples_df$Iteration <- 1:n_iter

trace_plot <- ggplot(samples_df, aes(x = Iteration)) +
  geom_line(aes(y = Intercept, color = "Intercept")) +
  geom_line(aes(y = Slope, color = "Slope")) +
  labs(title = "Trace Plot of SGLD Samples", y = "Coefficient Value", x = "Iteration") +
  scale_color_manual(values = c("Intercept" = "blue", "Slope" = "red")) +
  theme_minimal()

trace_plot

beta_est <- colMeans(samples[(n_iter/2):n_iter, ])
beta_est

# Compare with true coefficients
beta_true

