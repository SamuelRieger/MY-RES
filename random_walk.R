pi_dist <- function(x, y) {
  exp(-((x-1)^2 + (y-x^2)^2))
}

random_walk_metropolis_hastings <- function(n, start_x, start_y, sigma) {
  x_samples <- numeric(n)
  y_samples <- numeric(n)
  
  x_samples[1] <- start_x
  y_samples[1] <- start_y
  
  for (i in 2:n) {
    # Propose new values from a normal distribution centered at the current values
    # Transition kernel is a normal distribution centered at the previous value with a standard deviation of sigma
    x_proposed <- rnorm(1, mean = x_samples[i-1], sd = sigma)
    y_proposed <- rnorm(1, mean = y_samples[i-1], sd = sigma)
    
    # Calculate acceptance ratio
    pi_current <- pi_dist(x_samples[i-1], y_samples[i-1])
    pi_proposed <- pi_dist(x_proposed, y_proposed)
    acceptance_ratio <- pi_proposed / pi_current
    
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

plot(samples$x, samples$y, pch = 20, col = rgb(0, 0, 0, 0.1), main = "Random Walk Metropolis-Hastings Samples", xlab = "x", ylab = "y")
