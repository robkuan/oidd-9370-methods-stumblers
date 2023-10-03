#-------------------------------------------------------------------------------
# POWER ANALYSIS - HW 2

# Import libraries
library("tidyverse")

#-------------------------------------------------------------------------------
# QUESTION 1

# Import data from control condition and clean data
df_raw <- read_csv("data/buder et al.csv") # read in data

df <- df_raw |>
  filter(echo == "balanced") |> # filter for control condition rows
  select(echo, button, value) # select columns

# Set parameter variables for bootstrap
set.seed(111)
n <- 1360
p <- vector("double", length = 10000)

# Run a bootstrap to get the distribution
for (k in 1:10000) {
  value_resample_1 <- sample(df$value, n / 2, replace = TRUE) # control
  value_resample_2 <- sample(df$value, n / 2, replace = TRUE) + 0.25 # treatment
  p[k] <- t.test(value_resample_1, value_resample_2)$p.value
}

# Caculate the power at 5%
mean(p <= 0.05)

#-------------------------------------------------------------------------------

# QUESTION 2

# Set parameter variables for bootstrap
set.seed(111)
n <- 15000
p <- vector("double", length = 10000)

# Run a bootstrap to get the distribution
for (k in 1:10000) {
  v_1 <- sample(df$value, n / 4, replace = TRUE) # control/low moderator
  v_2 <- sample(df$value, n / 4, replace = TRUE) # control/high moderator
  v_3 <- sample(df$value, n / 4, replace = TRUE) + 0.1 # treatment/low moderator
  v_4 <- sample(df$value, n / 4, replace = TRUE) + 0.25 # treatment/high moderator
  p[k] <- t.test(v_3 - v_1, v_4 - v_2)$p.value
}

# Caculate the power at 5%
mean(p <= 0.05)