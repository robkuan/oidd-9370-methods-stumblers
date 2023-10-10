# If there is a true effect, a smaller p-value is ALWAYS more likely! 
d <- 0.1
p <- c()

for (k in 1:100000) {
  x1 <- rnorm(100)
  x2 <- rnorm(100) + d
  p[k] <- t.test(x1, x2)$p.value
}

hist(p[p<.05])

# T-distribution with non-centrality parameter
# gives a random t value with 20 per cell
hist(rt(n = 10000, df = 38), col = adjustcolor('red4',.4)) 
hist(rt(n = 10000, df = 38, ncp = 2), col = adjustcolor('dodgerblue',.4), add=TRUE)
t2 = rt(n=10000, df = 38, ncp = 2)
mean(t2>2.1)


2 * (1-pt(1.96, df = 100))
