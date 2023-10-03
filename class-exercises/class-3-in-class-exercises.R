# 1. Define test statistic
# 2. Measure it in the data
# 3. Distribution under the null
# 4. Is the observed extreme under the null? 

library('tidyverse')

x1 <- c(3, 4, 5)
x2 <- c(4, 5, 6)

sim_total <- 1000
dm_boot <- vector("double", sim_total)

for (k in 1:sim_total){
  # Force the Null #1: identitical distribution
  # x1_boot <- sample (c(x1, x2), size = 3, replace = TRUE)
  # x2_boot <- sample (c(x1, x2), size = 3, replace = TRUE)
  # dm_boot[k] = mean(x2_boot) - mean(x1_boot)
  
  # Force the Null #2: force means to be the same (subtract a constant)
  ### For some reason, this doesn't work :( it should match Null #1
  x1_boot <- sample (x1, size = 3, replace = TRUE)
  x2_boot <- sample (x2, size = 3, replace = TRUE) - 1
  dm_boot[k] = mean(x2_boot) - mean(x1_boot)

}

mean(dm_boot>1)
