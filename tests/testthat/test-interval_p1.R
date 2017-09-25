context("interval_p1.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "When n = NULL, return the value of n that satisfies the other arguments",
  {
    expect_equal(
      interval_p1(p0 = 0.72, E = 0.01, alpha = 0.05)$n_est,
      7744.38098251934
    )
  }
)

test_that(
  "When n = NULL, return the value of n that satisfies the other arguments",
  {
    expect_equal(
      interval_p1(p0 = 0.72, E = 0.01, alpha = 0.05)$n,
      7745
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When p0 = NULL, return the value of p0 that satisfies the other arguments",
  {
    expect_equal(
      interval_p1(n = 7745, p0 = NULL, E = 0.01, alpha = 0.05,
                  upper = TRUE)$p0,
      0.719961725922044
    )
  }
)

test_that(
  "When p0 = NULL, return the value of p0 that satisfies the other arguments",
  {
    expect_equal(
      interval_p1(n = 7745, p0 = NULL, E = 0.01, alpha = 0.05,
                  upper = FALSE)$p0,
      1 - 0.719961725922044
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "When E = NULL, return the value of E that satisfies the other arguments",
  {
    expect_equal(
      interval_p1(n = 7745, p0 = .72)$E,
      0.0099996003680
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "When alpha = NULL, return the value of alpha that satisfies the other arguments",
  {
    expect_equal(
      interval_p1(n = 7745, p0 = .72, E = 0.01, alpha = NULL)$alpha,
      0.049961560867
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if the number of n, p0, E, and alpha that are NULL is 
   not exactly one.",
  {
    expect_error(interval_p1(E = NULL, n = NULL))
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if n is not integerish on the interval [2, Inf)",
  {
    expect_error(interval_p1(n = 1))
  }
)

test_that(
  "Cast an error if n is not integerish on the interval [2, Inf)",
  {
    expect_error(interval_p1(n = 2.3))
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if p0 is not numeric on the interval (0, 1)",
  {
    expect_error(interval_p1(n = 7745, p0 = "one"))
  }
)

test_that(
  "Cast an error if sigma is not numeric on the interval (0, 1)",
  {
    expect_error(interval_p1(n = 7745, p0 = 1.25))
  }
)

test_that(
  "Cast an error if sigma is not numeric on the interval (0, 1)",
  {
    expect_error(interval_p1(n = 7745, p0 = -1))
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if E is not numeric",
  {
    expect_error(interval_p1(E = "ten"))
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if alpha is not numeric on the interval (0, 1)",
  {
    expect_error(interval_p1(E = 0.01, alpha = -1))
  }
)

# Functional Requirement 10------------------------------------------

test_that(
  "Cast an error if tail is not a subset of c('both', 'left', 'right')",
  {
    expect_error(interval_p1(E = 0.01, tail = "all"))
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(interval_p1(E = 0.01, interval_min = c(3, 4)))
  }
)

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(interval_p1(E = 0.01, interval_min = c("three")))
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(interval_p1(E = 0.01, interval_max = c(3, 4)))
  }
)

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(interval_p1(E = 0.01, interval_max = c("three")))
  }
)

# Functional Requirement 13 -----------------------------------------

test_that(
  "Cast an error if upper is not logical(1)",
  {
    expect_error(interval_p1(E = 0.01, upper = c(FALSE, TRUE)))
  }
)

test_that(
  "Cast an error if upper is not logical(1)",
  {
    expect_error(interval_p1(E = 0.01, upper = "TRUE"))
  }
)