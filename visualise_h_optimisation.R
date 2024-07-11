library(ggplot2)

df_optimised_tests <- readRDS("df_optimised_tests.rds")
df_unoptimised_tests <- readRDS("df_unoptimised_tests.rds")

df_optimised_tests$group <- "(9.5e-3*sqrt(n))/N"
df_unoptimised_tests$group <- "3.5e-3"

df_ksd_combined <- rbind(df_optimised_tests, df_unoptimised_tests)

ksd_comparison <- ggplot(data = df_ksd_combined, aes(x = n, y = ksd, color = group)) +
  geom_line() +
  geom_point() +
  labs(title = "KSD vs Batch Size for Optimised & Un-Optimised h",
       subtitle = "N = 1000",
       x = "Batch Size (n)",
       y = "KSD",
       color = "h =") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(ksd_comparison)

df_time_tests <- readRDS("df_time_tests.rds")

time_comparison <- ggplot(data = df_time_tests, aes(x = n, y = time)) +
  geom_line() +
  geom_point() +
  labs(title = "Time vs Batch Size",
       subtitle = "N = 1000",
       x = "Batch Size (n)",
       y = "Time (s)",
       color = "h =") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(time_comparison)
