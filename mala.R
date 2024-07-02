source("animate_mcmc.R")

pi_dist <- function(x, y) {
  exp(-((x-1)^2 + (y-x^2)^2))
}
pi_grad_log_dx <- function(x, y) {
  -2*(x-1)+4*x*(y-x^2)
}
pi_grad_log_dy <- function(x, y) {
  -2*(y-x^2)
}

random_walk_metropolis_hastings <- function(n, start_x, start_y, sigma) {
  x_samples <- numeric(n)
  y_samples <- numeric(n)
  
  x_samples[1] <- start_x
  y_samples[1] <- start_y
  
  for (i in 2:n) {
    # Propose new values
    x_current <- x_samples[i-1]
    y_current <- y_samples[i-1]
    
    grad_log_dx <- pi_grad_log_dx(x_current, y_current)
    grad_log_dy <- pi_grad_log_dy(x_current, y_current)
    
    x_proposed <- x_current + 0.5 * sigma^2 * grad_log_dx + rnorm(1)*sigma
    y_proposed <- y_current + 0.5 * sigma^2 * grad_log_dy + rnorm(1)*sigma
    
    # Calculate acceptance ratio
    pi_current <- pi_dist(x_current, y_current)
    pi_proposed <- pi_dist(x_proposed, y_proposed)
    
    grad_log_dx_proposed <- pi_grad_log_dx(x_proposed, y_proposed)
    grad_log_dy_proposed <- pi_grad_log_dy(x_proposed, y_proposed)
    
    log_g_current_given_proposed <- sum(dnorm(c(x_current, y_current),
      mean = c(x_proposed, y_proposed) +0.5 * sigma^2 * c(grad_log_dx_proposed, grad_log_dy_proposed),
      sd = sigma, log = TRUE))
    log_g_proposed_given_current <- sum(dnorm(c(x_proposed, y_proposed),
      mean = c(x_current, y_current) + 0.5 * sigma^2 * c(grad_log_dx, grad_log_dy),
      sd = sigma, log = TRUE))
    
    acceptance_ratio <- exp(log(pi_proposed) - log(pi_current) + log_g_current_given_proposed - log_g_proposed_given_current)
    
    # Accept or reject the proposed values
    if (runif(1) < acceptance_ratio) {
      x_samples[i] <- x_proposed
      y_samples[i] <- y_proposed
    } else {
      x_samples[i] <- x_samples[i-1]
      y_samples[i] <- y_samples[i-1]
    }
  }
  
  return(list(x = x_samples, y = y_samples))
}

n <- 10000
start_x <- 0
start_y <- 0
sigma <- 0.5

samples <- random_walk_metropolis_hastings(n, start_x, start_y, sigma)

plot(samples$x, samples$y, pch = 20, col = rgb(0, 0, 0, 0.1), main = "Metropolis-Adjusted Langevin Algorithm", xlab = "x", ylab = "y")
animate_mcmc(samples, "Random Walk Metropolis-Hastings Samples")
