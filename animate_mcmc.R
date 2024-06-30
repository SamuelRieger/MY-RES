library("gganimate")
library("ggplot2")
library("dplyr")

set.seed(42)
data <- data.frame(
  x = cumsum(rnorm(100)),
  y = cumsum(rnorm(100)),
  frame = 1:100
)

# Create a df that represents the last 20 points for creation of joining lines
get_last_n_points <- function(df, n = 10) {
  df_result <- data.frame()
  for (i in 1:nrow(df)) {
    if (i > n) {
      temp <- df[(i-n+1):i,]
    } else {
      temp <- df[1:i,]
    }
    temp$group <- i
    temp$frame <- i
    temp$opacity <- seq(0, 1, length.out = nrow(temp))
    df_result <- rbind(df_result, temp)
  }
  return(df_result)
}

# Animation function
animate_mcmc <- function(samples, title) {
  n <- length(samples$x)
  samples$frame <- 1:n
  # samples_lines <- get_last_n_points(samples)
  
  p <- ggplot(data = samples, aes(x = x, y = y, group = frame)) +
    geom_point(colour = "black", size = 2, alpha = 0.1, shape = 20) +
    # geom_path(data = samples_lines, aes(x = x, y = y, group = group, alpha = opacity),
    #           colour = "blue",
    #           linewidth = 1) +
    transition_reveal(along = frame) +
    # shadow_mark(alpha = 0.1, size = 1.5, colour = "black") +
    labs(title = title, subtitle = "n = {frame_along}") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          text = element_text(size = 15)) +
    ease_aes("linear")
    # guides(alpha = "none")
  
  animation_object <- animate(p, nframes = 600, fps = 60, renderer = gifski_renderer())
  
  file_name <- paste(title, ".gif", sep = "")
  
  anim_save(file_name, animation_object)
}