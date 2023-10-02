# 1. Define test statistic
# 2. Measure it in the data
# 3. Distribution under the null
# 4. Is the observed extreme under the null? 

library('tidyverse')

#-------------------------------------------------------------------------------

#1. Define a test statistic
# Option 1 is the range of SDs in a given study
# Option 2 can be mean absolute deviation from the mean SD
# Option 3 SD(SD) --> this one we will use

# 2. Measure it in the data
sd_1 <- 25.09
sd_2 <- 24.58
sd_3 <- 25.65
sds <- c(sd_1, sd_2, sd_3)
sdsd_obs <- sd(sds)

# 3. Distribution under the null

mean_1 <- 39.74
mean_2 <- 85.74
mean_3 <- 65.73

set.seed(2828)

sdsd_sim <- c()

for (k in 1:10000) {
  x_1 <- rnorm(20, mean_1, sd_1)
  x_2 <- rnorm(20, mean_2, sd_2)
  x_3 <- rnorm(20, mean_3, sd_3)
  
  sd1_sim <- sd(x_1)
  sd2_sim <- sd(x_2)
  sd3_sim <- sd(x_3)
  
  sdsd_sim[k] = sd(c(sd1_sim, sd2_sim, sd3_sim))
}

hist(sdsd_sim, breaks = 50)

# 4. Is the observed extreme under the null?


#-------------------------------------------------------------------------------
# POWER ANALYSIS
# Data colada blog post: http://datacolada.org/17

mean_1 <- 5.4
mean_2 <- 4.8
sd_1 <- 1
sd_2 <- 1
n <-  20

p = c()
for (k in 1:10000) {
  x_1 <- rnorm(n, mean_1, sd_1)
  x_2 <- rnorm(n, mean_2, sd_2)
  p[k] = t.test(x_1, x_2)$p.value
}

mean(p <= 0.05)

install.packages('pwr')
library(pwr) # library for power analysis


#-------------------------------------------------------------------------------
# BOOTSTRAPPING
# Does not require normality like t-tests
# With t-test that just employs a stronger assumption than bootstrapping

df <- read_csv('data/buder et al.csv')

mean(df$age)

mean_resample <- c()

for (k in 1:10000) {
  age_resample = sample(df$age, replace = TRUE)
  mean_resample[k] = mean(age_resample)  
}

hist(mean_resample)
se_boot <- sd(mean_resample)
se_math <- sd(df$age)/sqrt(nrow(df))
se_boot
se_math

# 95% confidence interval
quantile(mean_resample, c(0.025, 0.975))

#-------------------------------------------------------------------------------
# MEDIATION ANALYSIS WITH BOOTSTRAPPING

# original psych package used is mediate
library('mediation')
library('psych')

df <- read_csv('data/buder et al.csv')

#Code posted with paper
#Recoode the button variables
df$button_0_1 <- NA
df[df$button == "like",]$button_0_1 <- 0
df[df$button == "dislike",]$button_0_1 <- 1

lm_a <- lm(button_0_1 ~ conf, data = df)
lm_b <- lm(value ~ conf + button_0_1, data = df)

#Key analysis in the paper 
results <- psych::mediate(y = "value", x = "conf", m = "button_0_1", data = dat)

# Own analysis with mediation package
results <- mediation::mediate(lm_a, lm_b, boot = TRUE, boot.ci.type = 'bca', treat = "conf", mediator = "button_0_1", robustSE = TRUE, sims = 10000)  
summary(results)

# do it by hand, assuming each observation is independent
n = nrow(df)
set.seed(111)
a_boot <- vector("double", length = 1000)
b_boot <- vector("double", length = 1000)
c_boot <- vector("double", length = 1000)
cp_boot <- vector("double", length = 1000)

# you can use table(table(subk)) to look at your distributions

for (bk in 1:1000) {
  # which rows to select?
  subk <- sample(n, replace = TRUE)
  
  # get a subsample
  df_k <- df[subk,]
  
  lm_a_boot <- lm(button_0_1 ~ conf, data = df_k)
  lm_b_boot <- lm(value ~ conf + button_0_1, data = df_k)
  lm_c_boot <- lm(value ~ conf, data = df_k)
  a_boot[bk] <- coef(lm_a_boot)[2]
  b_boot[bk] <- coef(lm_b_boot)[3]
  c_boot[bk] <- coef(lm_c_boot)[2]
  cp_boot[bk] <- coef(lm_b_boot)[2]  
}

ab_boot = a_boot*b_boot
cc_boot = c_boot - cp_boot 

hist(ab_boot)
ci = quantile (ab_boot, c(0.025, 0.975))
ci

# do it by hand, assuming each observation is clustered under a participant
# THIS IS WRONG

n = nrow(df)
set.seed(111)
id_unique <- unique(df$id)

# which rows to select?
subk <- sample(n, replace = TRUE)

# get a subsample
df_k <- df[subk,]

# draw participants
id_sub <- sample(id_unique, replace = TRUE)
df_split <- split(df, df$id)

df_k <- df

# Assign vectors
a_boot <- vector("double", length = 1000)
b_boot <- vector("double", length = 1000)
c_boot <- vector("double", length = 1000)
cp_boot <- vector("double", length = 1000)


for (k in 1:106) { # THIS CODE CHUNK DOESN'T WORK - VERIFY
  df_k[(1+12*(k-1)):(12*k),] <- df[df$id == id_sub[k],]

  # table(table(df_k$id)) # every participant has multiples of 12 observations

  for (bk in 1:1000) {
    # which rows to select?
    subk <- sample(n, replace = TRUE)
    
    # get a subsample
    df_bk <- df_k[subk,]
    
    lm_a_boot <- lm(button_0_1 ~ conf, data = df_bk)
    lm_b_boot <- lm(value ~ conf + button_0_1, data = df_bk)
    lm_c_boot <- lm(value ~ conf, data = df_bk)
    a_boot[bk] <- coef(lm_a_boot)[2]
    b_boot[bk] <- coef(lm_b_boot)[3]
    c_boot[bk] <- coef(lm_c_boot)[2]
    cp_boot[bk] <- coef(lm_b_boot)[2]  
  }
}

ab_boot = a_boot*b_boot
cc_boot = c_boot - cp_boot 
hist(ab_boot)
ci = quantile (ab_boot, c(0.025, 0.975))
ci
