context("test_binomial.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "When n = NULL, correctly calculate the estimated sample size",
  {
    expect_equal(
      test_binomial(n=NULL, p0=.5, p1=.8, alpha=.05, power = 0.80,
                    tail='right')[["n"]],
      18
    )
  }
)

test_that(
  "When n = NULL, correctly calculate the estimated sample size",
  {
    expect_equal(
      test_binomial(n=NULL, p0=.5, p1=.8, alpha=.05, power = 0.80,
                    tail='right', conservative = TRUE)[["n"]],
      19
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When p0 = NULL, correctly calculate the null proportion",
  {
    expect_equal(
      test_binomial(n=20, p0=NULL, p1=.8, alpha=.05, power = 0.80,
                    tail='right')[["p0"]],
      0.54439872543131162530
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "When p1 = NULL, correctly calculate the alternative proportion",
  {
    expect_equal(
      test_binomial(n=20, p0=.50, p1=NULL, alpha=.05, power = 0.80,
                    tail='right')[["p1"]],
      0.79873432880075501128
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "When power = NULL, correctly calculate the power",
  {
    expect_equal(
      test_binomial(n=20, p0=.50, p1=.80, alpha=.05, power = NULL,
                    tail='right')[["power"]],
      0.80420778545954951788
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "When alpha = NULL, correctly calculate the significance",
  {
    expect_equal(
      test_binomial(n=20, p0=.50, p1=.80, alpha=NULL, power = 0.80,
                    tail='right')[["alpha"]],
      0.02071053263635511121
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "When n = NULL, correctly calculate the estimated sample size",
  {
    expect_equal(
      test_binomial(n=NULL, p0=.5, p1=.8, alpha=.05, power = 0.8,
                    tail=c('right', 'both', 'left'))[["n"]],
      c(18, 23, NA)
    )
  }
)

# Functional Requirement 7 ------------------------------------------
# Functional Requirement 8 ------------------------------------------
# Functional Requirement 9 ------------------------------------------
# Functional Requirement 10 -----------------------------------------
# Functional Requirement 11 -----------------------------------------
# Functional Requirement 12 -----------------------------------------
# Functional Requirement 13 -----------------------------------------
# Functional Requirement 14 -----------------------------------------
# Functional Requirement 15 -----------------------------------------
# Functional Requirement 16 -----------------------------------------
# Functional Requirement 17 -----------------------------------------
# Functional Requirement 18 -----------------------------------------