library(tidyverse)

# create variables for data
p1 <- c()
p2 <- c()
p3 <- c()
ub <- c()
lb <- c()

# run simulation
for (simk in 1:10000) {
  # x1 = rnorm(100, mean = 1)
  # x2 = rnorm(100, mean = 0)
  # x1 = sample(c(1,3,10), size = 12, replace = TRUE)
  # x2 = sample(c(1,3,10), size = 12, replace = TRUE)
  lucky = rnorm(100, mean = 0)
  unlucky = rnorm(100, mean = 0)
  control = rnorm(100, mean = 0)  
  t1 <- t.test(lucky, unlucky)
  t2 <- t.test(lucky, control)
  t3 <- t.test(unlucky, control)
  
  p1[simk] <- t1$p.value
  p2[simk] <- t2$p.value
  p3[simk] <- t3$p.value
  
  # lb[simk] <- t$conf.int[1]
  # ub[simk] <- t$conf.int[2]
  if (simk %% 100 == 0) cat("...", simk)
}

# look at results
mean(p1 < 0.05) 
mean(p2 < 0.05) 
mean(p3 < 0.05)

smallest <-  pmin(p1, p2, p3)

mean(smallest < 0.05)

# mean(p < 0.5)
# mean(lb < 1 & ub > 1)
# hist(p, col = "red4")
