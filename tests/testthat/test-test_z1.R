context("test_z1.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "When delta = NULL, calculate the value of delta that satisfies the
   other requirements",
  {
    expect_equal(
      round(test_z1(sigma = 15, n = 20, alpha = .05, power = 0.438749,
              tail = "right")$delta),
      5
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When n = NULL, calculate the value of n that satisfies the
  other requirements",
  {
    expect_equal(
      round(test_z1(sigma = 15, delta = 5, alpha = .05, power = 0.438749,
                    tail = "right")$n_est),
      20
    )
  }
)

test_that(
  "When n = NULL, calculate the value of n that satisfies the
  other requirements",
  {
    expect_equal(
      test_z1(sigma = 15, delta = 5, alpha = .05, power = 0.438749,
                    tail = "right")$n,
      20
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "When sigma = NULL, calculate the value of sigma that satisfies the
  other requirements",
  {
    expect_equal(
      round(test_z1(n = 20, delta = 5, alpha = .05, power = 0.438749,
                    tail = "right")$sigma),
      15
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "When n = NULL, calculate the value of n that satisfies the
  other requirements",
  {
    expect_equal(
      round(test_z1(sigma = 15, delta = 5, n = 20, power = 0.438749,
                    alpha = NULL, tail = "right")$alpha, 2),
      0.05
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "When n = NULL, calculate the value of n that satisfies the
  other requirements",
  {
    expect_equal(
      round(test_z1(sigma = 15, delta = 5, n = 20, alpha = 0.05,
                    power = NULL, tail = "right")$power, 6),
      0.438749
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error when the number of delta, n, sigma, alpha, and power 
   is not exactly one",
  {
    expect_error(
      test_z1(n = NULL, delta = NULL)
    )
  }
)

test_that(
  "Cast an error when the number of delta, n, sigma, alpha, and power 
  is not exactly one",
  {
    expect_error(
      test_z1(n = 20, delta = 5, sigma = 15)
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Calculate delta from mu_null and mu_alt",
  {
    expect_equal(
      test_z1(mu_null = 100, mu_alt = c(105, 106), sigma = 15, 
              n = 20, power = NULL)$delta,
      c(5, 6)
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Ignore mu_null and mu_alt with a warning when delta is not NULL",
  {
    expect_warning(
      test_z1(delta = 5, sigma = 15, n = 20, power = NULL,
              mu_null = 100, mu_alt = 105),
      "`delta` is non-null"
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error when delta = NULL and mu_null and mu_alt are not both given",
  {
    expect_error(
      test_z1(delta = NULL, sigma = 15, n = 20, power = NULL, alpha = 0.05)
    )
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error when delta is not numeric",
  {
    expect_error(test_z1(delta = "ten", sigma = 15, n = 20, 
                         power = NULL, alpha = 0.05))
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error when n is not numeric",
  {
    expect_error(test_z1(delta = 5, sigma = 15, n = "twenty",
                         power = NULL, alpha = 0.05))
  }
)

test_that(
  "Cast an error when n is not numeric",
  {
    expect_error(test_z1(delta = 5, sigma = 15, n = 20.5,
                         power = NULL, alpha = 0.05))
  }
)

test_that(
  "Cast an error when n is not numeric",
  {
    expect_error(test_z1(delta = 5, sigma = 15, n = 0,
                         power = NULL, alpha = 0.05))
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error when sigma is not numeric on the interval (0, Inf)",
  expect_error(
    test_z1(delta = 5, sigma = -1, n= 20, power = NULL)
  )
)

test_that(
  "Cast an error when sigma is not numeric on the interval (0, Inf)",
  expect_error(
    test_z1(delta = 5, sigma = "two", n= 20, power = NULL)
  )
)

# Functional Requirement 13 -----------------------------------------

test_that(
  "Cast an error when alpha is not numeric on the interval (0, 1)",
  expect_error(
    test_z1(delta = 5, sigma = 15, n = 20, power = NULL,
            alpha = "0.05")
  )
)

test_that(
  "Cast an error when alpha is not numeric on the interval (0, 1)",
  expect_error(
    test_z1(delta = 5, sigma = 15, n = 20, power = NULL,
            alpha = -0.05)
  )
)

# Functional Requirement 14 -----------------------------------------

test_that(
  "Cast an error when power is not numeric on the interval (0, 1)",
  expect_error(
    test_z1(delta = 5, sigma = 15, n = 20, power = "0.4",
            alpha = NULL)
  )
)

test_that(
  "Cast an error when power is not numeric on the interval (0, 1)",
  expect_error(
    test_z1(delta = 5, sigma = 15, n = 20, power = -0.4,
            alpha = NULL)
  )
)

# Functional Requirement 15 -----------------------------------------

test_that(
  "Cast an error when tail is not a subset of c(both, left, right)",
  {
    expect_error(
      test_z1(delta = 5, sigma = 15, n = 20, power = NULL,
              tail = "not an option")
    )
  }
)

# Functional Requirement 16 -----------------------------------------

test_that(
  "Cast an error when mu_null is not numeric",
  {
    expect_error(
      test_z1(delta = 5, sigma = 15, n = 20, power = NULL,
              mu_null = "one")
    )
  }
)

# Functional Requirement 17 -----------------------------------------

test_that(
  "Cast an error when mu_alt is not numeric",
  {
    expect_error(
      test_z1(delta = 5, sigma = 15, n = 20, power = NULL,
              mu_alt = "one")
    )
  }
)

# Functional Requirement 18 -----------------------------------------

test_that(
  "Cast an error when interval_min is not numeric(1)",
  {
    expect_error(
      test_z1(delta = 5, sigma = 15, n = 20, power = NULL,
              interval_min = "one")
    )
  }
)

# Functional Requirement 19 -----------------------------------------

test_that(
  "Cast an error when interval_max is not numeric(1)",
  {
    expect_error(
      test_z1(delta = 5, sigma = 15, n = 20, power = NULL,
              interval_max = "one")
    )
  }
)