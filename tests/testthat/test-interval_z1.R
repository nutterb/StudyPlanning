context("interval_z1.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "When n = NULL, return the value of n that satisfies the other arguments",
  {
    expect_equal(
      interval_z1(sigma = 10, E = 2, alpha = 0.10)$n_est,
      67.63856613173926746185
    )
  }
)

test_that(
  "When n = NULL, return the value of n that satisfies the other arguments",
  {
    expect_equal(
      interval_z1(sigma = 10, E = 2, alpha = 0.10)$n,
      68
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When sigma = NULL, return the value of sigma that satisfies the other arguments",
  {
    expect_equal(
      interval_z1(n = 68, E = 2, alpha = 0.10)$sigma,
      10.0266809351524
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "When E = NULL, return the value of E that satisfies the other arguments",
  {
    expect_equal(
      interval_z1(n = 68, s = 10, alpha = 0.10)$E,
      1.99467801302671
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "When alpha = NULL, return the value of alpha that satisfies the other arguments",
  {
    expect_equal(
      interval_z1(n = 68, sigma = 10, E = 2, alpha = NULL)$alpha,
      0.09909341903706406218
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if the number of n, sigma, E, and alpha that are NULL is 
   not exactly one.",
  {
    expect_error(interval_z1(E = 2))
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if n is not integerish on the interval [2, Inf)",
  {
    expect_error(interval_z1(n = 1, E = 2))
  }
)

test_that(
  "Cast an error if n is not integerish on the interval [2, Inf)",
  {
    expect_error(interval_z1(n = 2.3, E = 2))
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if sigma is not numeric on the interval (0, Inf)",
  {
    expect_error(interval_z1(E = 2, s = 0))
  }
)

test_that(
  "Cast an error if sigma is not numeric on the interval (0, Inf)",
  {
    expect_error(interval_z1(E = 2, s = -1))
  }
)

test_that(
  "Cast an error if sigma is not numeric on the interval (0, Inf)",
  {
    expect_error(interval_z1(E = 2, s = "ten"))
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if E is not numeric",
  {
    expect_error(interval_z1(s = 2, E = "ten"))
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if alpha is not numeric on the interval (0, 1)",
  {
    expect_error(interval_z1(s = 2, E = 10, alpha = -1))
  }
)

# Functional Requirement 10------------------------------------------

test_that(
  "Cast an error if tail is not a subset of c('both', 'left', 'right')",
  {
    expect_error(interval_z1(s = 2, E = 10, tail = "all"))
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(interval_z1(s = 2, E = 10, interval_min = c(3, 4)))
  }
)

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(interval_z1(s = 2, E = 10, interval_min = c("three")))
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(interval_z1(s = 2, E = 10, interval_max = c(3, 4)))
  }
)

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(interval_z1(s = 2, E = 10, interval_max = c("three")))
  }
)