#-------------------------------------------------------------------------------
# CALCULATING THE NULL - HW 3

# Import libraries
library("tidyverse")

#-------------------------------------------------------------------------------
# QUESTION 1

# Import data from control condition and clean data
df_raw <- read_csv("data/probowl nfl.csv") # read in data
n <- nrow(df_raw)

# Create a function to shuffle teams, sum, and sort the results
shuffle_and_sum <- function(df) {
  df_shuffled <- df |>
    mutate(team = sample(team)) |>
    group_by(team) |>
    summarise(total_probowls = sum(probowls)) |>
    arrange(total_probowls)
  return(df_shuffled$total_probowls)
}

# Run a bootstrap to get the distribution
set.seed(111)
mat_results <- matrix(0, nrow = 1000, ncol = 31)

for (k in 1:1000) {
  mat_results[k, ] <- shuffle_and_sum(df_raw)
}

# Calculate mean, lower, and upper bound for each column (team)
means <- colMeans(mat_results)

lower <- c()
upper <- c()

for (k in 1:31) {
  lower[k] <- quantile(mat_results[, k], 0.025)
  upper[k] <- quantile(mat_results[, k], 0.975)
}

# Import data from control condition and clean data
actual_performance <- df_raw |>
  group_by(team) |>
  summarise(total_probowls = sum(probowls)) |>
  arrange(total_probowls)

# Convert results to data frame for plotting
df_plot <- data.frame(team = 1:31, mean = means, lower = lower, upper = upper)
df_plot$actual <- actual_performance$total_probowls

# Plotting the mean, confidence interval, and actual performance
ggplot(df_plot) +
  geom_line(aes(x = team, y = mean), color = "blue", linetype = "dashed") +
  geom_ribbon(aes(x = team, ymin = lower, ymax = upper), fill = "gray80", alpha = 0.5) +
  geom_line(aes(x = team, y = actual), color = "red") +
  geom_point(aes(x = team, y = actual), color = "red") +
  labs(
    title = "Mean and 95% CI of Probowls for Each Team vs Actual Performance",
    x = "Team (Ranked from Lowest to Highest)",
    y = "Probowls"
  ) +
  theme_minimal()

ggsave("plot-pro_bowl.png", width = 10, height = 6)
