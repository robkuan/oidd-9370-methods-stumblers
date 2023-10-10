#------------------------------------------------------------
# Intuition for why smaller p-values are more likely if a true effect exists

# If there is a true effect, a smaller p-value is ALWAYS more likely!
d <- 0.1
p <- c()

for (k in 1:100000) {
  x1 <- rnorm(100)
  x2 <- rnorm(100) + d
  p[k] <- t.test(x1, x2)$p.value
}

hist(p[p < .05])

# T-distribution with non-centrality parameter
# gives a random t value with 20 per cell
hist(rt(n = 10000, df = 38), col = adjustcolor("red4", .4))
hist(rt(n = 10000, df = 38, ncp = 2), col = adjustcolor("dodgerblue", .4), add = TRUE)
t2 <- rt(n = 10000, df = 38, ncp = 2)
mean(t2 > 2.1)


2 * (1 - pt(1.96, df = 100))


#------------------------------------------------------------
# Intuition why regression is just the weighted average of slopes, 
# Weighted by the square distance between pairs of points of x

x <- c(1, 2, 3)
y <- x^2
df <- data.frame(x, y)
df
# Regression is equal to the slopes between every point, weighted
# by the square of the distance between the values of x
(3 * 1 + 4 * 4 + 5 * 1) / (1 + 4 + 1)
lm(y ~ x)


#------------------------------------------------------------
# Adding controls to reduce noise in regression

# This simulates an experiment where 500 customers get a promotion that ended up on average increasing their purchases by $10
# this benefit is observable only after controlling for past purchases

rm(list = ls())

set.seed(124)
n <- 500

# 1. Random assignment
x <- rep(c(1, 0), each = 500)

# 2. Latent variable, how much people spend for christmas varies across people in a stable way
christmas.budget <- rnorm(2 * n, mean = 500, sd = 150)

# 3. We don't observe that, we do observe last year
e1 <- rnorm(2 * n, sd = 15) # (last year)
e2 <- rnorm(2 * n, sd = 15) # (this year)

# 4 So purchases for each year
last.year <- christmas.budget + e1 # People deviate at random fromm their budget each year
# 5And then this year
this.year <- christmas.budget + 10 * x + e2

# 6 Simplify variable names
y <- this.year
y0 <- last.year

# 7 Analyze data
# 7.1 Experiment
t.test(y ~ x) # p=.3472

# 7.2 Same with regression
summary(lm(y ~ x))

# 7.3 But, we can control for last year
summary(lm(y ~ x + y0)) # p<<.0001

# 8 Let's see it with graphs
# 8.1 raw data histograms
dev.off()
par(mfrow = c(2, 1))
h1 <- hist(y[x == 1], col = "red", breaks = 20, xlim = c(100, 950))
h0 <- hist(y[x == 0], col = "blue", breaks = 20, xlim = c(100, 950))
# hard to spot $10

dev.off()
plot(density(y[x == 1]), lty = 1, col = "red")
points(density(y[x == 0]), lty = 1, col = "blue")

# 9. get residuals of expenses this year, controlling for last year's
lm1 <- lm(y ~ y0)
res1 <- resid(lm1)

# 9.1 raw data
dev.off()
plot(density(y[x == 0]), ylim = c(0, .003), col = "blue", main = "distribution of expenses")
points(density(y[x == 1]), col = "red", type = "l")
abline(v = mean(y[x == 1]), col = "red")
abline(v = mean(y[x == 0]), col = "blue")

# 9.2 Residuals
plot(density(res1[x == 0]), col = "blue", main = "distribution of residual of expenses this year, controlling for last year")
points(density(res1[x == 1]), col = "red", type = "l")
abline(v = mean(res1[x == 1]), col = "red")
abline(v = mean(res1[x == 0]), col = "blue")

# 9.3 Shift residuals so they have the same mean and plot side by side
offset1 <- mean(y[x == 1]) - mean(res1[x == 1])
offset0 <- mean(y[x == 0]) - mean(res1[x == 0])

# 9.4 one graph
plot(density(y[x == 0]), col = "blue", main = "distribution of expenses", ylim = c(0, .02))
points(density(y[x == 1]), col = "red", type = "l")
points(density(res1[x == 0] + offset0), col = "blue", lty = 2, type = "l")
points(density(res1[x == 1] + offset1), col = "red", type = "l", lty = 2)

abline(v = mean(res1[x == 1]) + offset1, col = "red")
abline(v = mean(res1[x == 0]) + offset1, col = "blue")
