# If there are no outliers, in the sense that the data all come from the normal 
# distribution, and you remove the single observation with the biggest impact on 
# the results, how often does a p<.01 become p>.01?

# ------------------------------------------------------------------------------
# load libraries
library(tidyverse)

# ------------------------------------------------------------------------------
# create function for running simulations
excl_sim <- function(
    n_sim = 1000,
    n_per_group = 100,
    mean_1 = 0,
    mean_2 = 0,
    sd_1 = 1,
    sd_2 = 1) {
  
  # create vectors for p-values
  p_1 <- vector("double", n_sim) # create vector for p-values w/o exclusions
  p_2 <- vector("double", n_sim) # create vector for p-values w/ exclusions

  for (i in 1:n_sim) {
    # create data from normal distributions
    group_1 <- rnorm(n_per_group, mean = mean_1, sd = sd_1)
    group_2 <- rnorm(n_per_group, mean = mean_2, sd = sd_2)

    # collate data into y and x vector for regression
    dummy_1 <- rep(1, n_per_group) # dummy variables for group 1
    dummy_2 <- rep(0, n_per_group) # dummy variables for group 2
    y <- c(group_1, group_2) # create outcome vector
    x <- c(dummy_1, dummy_2) # create predictor vector

    # run regression for t-test
    model_1 <- lm(y ~ x) # run regression
    p_1[i] <- summary(model_1)$coefficients[2, 4] # extract p-value

    # run regression for t-test after removing outlier using DFBETA
    index <- as.integer(which.max(dfbetas(model_1)[, 2])) # find outlier index
    y_2 <- y[-index] # remove outlier for outcome vector
    x_2 <- x[-index] # remove outlier for predictor vector
    model_2 <- lm(y_2 ~ x_2) # run regression
    p_2[i] <- summary(model_2)$coefficients[2, 4] # extract p-value
  }
  
  # create data frame for p-values
  df <- as_tibble(cbind(p_1, p_2)) 
  df <- df |> mutate(
    p_1_sig = ifelse(p_1 <= 0.01, 1, 0), # p-value < 0.01 for original data
    p_2_sig = ifelse(p_2 <= 0.01, 1, 0), # p-value < 0.01 for data w/o outlier
    s_to_ns = ifelse(p_1_sig == 1 & p_2_sig == 0, 1, 0), # sig to non-sig
    ns_to_s = ifelse(p_1_sig == 0 & p_2_sig == 1, 1, 0) # non-sig to sig
  )

  # return data subset
  n_sig_old <- sum(df$p_1_sig) # number of sig p-values for original data
  n_turned_ns <- sum(df$s_to_ns) # number of sig to non-sig p-values
  percent_ns <- sum(df$s_to_ns) / sum(df$p_1_sig) # % of sig to non-sig p-values

  return(as_tibble(cbind(n_sig_old, n_turned_ns, percent_ns))) # return data
}

# ------------------------------------------------------------------------------

# create sensitivity analysis for different sample sizes and effect sizes
n_sim <- 5000
n_per_group <- c(5, 25, 50, 100, 500) # sample sizes of 5 -> 500 per group
effect_sizes <- c(0, 0.1, 0.2, 0.3, 0.4) # cohen's d of 0 -> 0.40
results <- data.frame() # create empty data frame for results

# run simulations and output values to dataframe
for (n in n_per_group) {
  for (m in effect_sizes) {
    sim_results <- excl_sim(
      n_sim = n_sim,
      n_per_group = n,
      mean_1 = m
    )

    # add extra columns to record the parameters used
    sim_results$n_per_group <- n
    sim_results$effect_sizes <- m

    # bind the results to the results data frame
    results <- rbind(results, sim_results)
  }
}

# convert results to tibble and view
results <- as_tibble(results)

results |>
  select(effect_sizes, n_per_group, n_sig_old, n_turned_ns, percent_ns) |>
  arrange(effect_sizes, n_per_group) |>
  view()


