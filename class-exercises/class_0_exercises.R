# Compute if the sum of two cards in a deck is equal to 12
count <- 0

for (i in 1:1000) {
  # 1 Simulate Data

  full_deck <- rep(1:13, 4)
  cards_drawn <- sample(full.deck, 2, replace = FALSE)

  # 2 Compute the sum of the two cards

  sum_cards[i] <- sum(cards_drawn)

  # 3 Check if the sum is equal to 12
  equals_12[i] <- (sum_cards[i] == 12)
}

#-------------------------------------------------------------------------------

# Birthday problem
birthday_match <- c()
n <- 23 # size of room

set.seed(50)

for (i in 1:1000) {
  # 1 Simulate Data

  birthdays <- sample(1:365, n, replace = TRUE)

  # 2 Compute the unique birthdays

  unique_birthdays <- length(unique(birthdays))

  # 3 Check if there is a birthday match
  birthday_match[i] <- (unique_birthdays) < n
}

# Compute the probability of a birthday match
mean(birthday_match)

#-------------------------------------------------------------------------------

# simulate two normally distributed variables
grp_1 <- rnorm(100, mean = 100, sd = 10)
grp_2 <- rnorm(100, mean = 100, sd = 10)

# compute a t-test
t <- t.test(grp_1, grp_2,
  alternative = "two.sided",
  mu = 0,
  paired = FALSE,
  var.equal = TRUE
)

# create tidy.t function
tidy.t <- function(t, n_decimals = 3) {
  # store the mean estimates
  m1 <- t$estimate[1]
  m2 <- t$estimate[2]

  # compute the difference in means
  diff <- m1 - m2

  # extract the p-value and confidence interval
  p <- t$p.value
  ci <- t$conf.int

  # aggregate results into a data.frame with 1 row
  row <- data.frame(
    mean1 = m1,
    mean2 = m2,
    m1_m2 = diff,
    ci_L = ci[1],
    ci_H = ci[2]
  )

  # round the rows
  row <- round(row, n_decimals)
  print(" Modified t-test is stored")
  invisible(row)
}

