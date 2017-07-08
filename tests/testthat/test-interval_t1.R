context("interval_t1")

# Parameters from Student's Classical Example
# Student's sleep data

student_E <- 
  (sleep$extra[sleep$group == 1] %>%
     t.test %$%
     conf.int %>%
     diff) / 2
student_s <- sd(sleep$extra[sleep$group == 1])


student_E_onetail  <- 
  qt(0.05, lower.tail = FALSE, 9) * student_s / sqrt(10)

student_s_onetail <-
  student_E * sqrt(10) / qt(0.05, lower.tail = FALSE, 9)

# Functional Requirement 1 ------------------------------------------

test_that(
  "When E = NULL, correctly calculate the value that satisfies the other arguments",
  {
    expect_equal(
      interval_t1(E = NULL,
                  s = student_s,
                  n = 10,
                  alpha = 0.05,
                  tail = c("both", "left"))[["E"]],
      c(student_E, student_E_onetail)
    )
  }
)
# Functional Requirement 2 ------------------------------------------

test_that(
  "When n = NULL, correctly calculate the value that satisfies the other
   argument",
  {
    expect_equal(
      interval_t1(E = student_E,
                  s = student_s,
                  alpha = 0.05,
                  tail = "both")[["n"]],
      11
    )
  }
)
# Functional Requirement 3 ------------------------------------------

test_that(
  "When s = NULL, correctly calculate the standard deviation that satisfies
    the other arguments",
  {
    expect_equal(
      interval_t1(E = student_E,
                  s = NULL,
                  n = 10,
                  alpha = 0.05,
                  tail = c("both", "right"))[["s"]],
      c(student_s, student_s_onetail)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "When alpha = NULL, correctly calculate the significance level that 
   satisfies the other arguments",
  {
    expect_equal(
      interval_t1(E = student_E,
                   s = student_s,
                   n = 10,
                   alpha = NULL,
                   tail = c("both", "left"))[["alpha"]],
      c(0.0499414102928245, 0.0249707051464123)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if the number of NULL arguments among n, E, s, and alpha
   is not 1",
  {
    expect_error(
      interval_t1(E = NULL,
                  n = NULL),
      "Exactly one of"
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error when E is not numeric",
  {
    expect_error(
        interval_t1(E = "11",
                    s = student_s,
                    alpha = 0.05,
                    tail = "both"),
        "Must be of type 'numeric'"
    )
  }
)

test_that(
  "Cast an error when E is not numeric on the interval (0, Inf)",
  {
    expect_error(
      interval_t1(E = -3,
                  s = student_s,
                  alpha = 0.05,
                  tail = "both"),
      "All elements must be [>][=] 0"
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if n is not numeric",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = student_s,
                  n = "10",
                  alpha = 0.05,
                  tail = c("both", "left")),
      "Must be of type 'integerish'"
    )
  }
)

test_that(
  "Cast an error if n is not numeric on the interval [2, Inf)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = student_s,
                  n = 1,
                  alpha = 0.05,
                  tail = c("both", "left")),
      "All elements must be [>][=] 2"
    )
  }
)

test_that(
  "Cast an error if n is not integerish on the interval [2, Inf)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = student_s,
                  n = 4.5,
                  alpha = 0.05,
                  tail = c("both", "left")),
      "Must be of type 'integerish'"
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if s is not numeric on the interval (0, Inf)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = "12",
                  n = 10,
                  alpha = 0.05,
                  tail = c("both", "left")),
      "Must be of type 'numeric'"
    )
  }
)

test_that(
  "Cast an error if s is not numeric on the interval (0, Inf)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = -3,
                  n = 10,
                  alpha = 0.05,
                  tail = c("both", "left")),
      "All elements must be [>][=] 0"
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if alpha is not numeric",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha = "0.05",
                  tail = c("both", "left")),
      "Must be of type 'numeric'"
    )
  }
)

test_that(
  "Cast an error if alpha is not numeric on the interval (0, 1)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha = -.05,
                  tail = c("both", "left")),
      "All elements must be [>][=] 0"
    )
  }
)

test_that(
  "Cast an error if alpha is not numeric on the interval (0, 1)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha = 1.5,
                  tail = c("both", "left")),
      "All elements must be [<][=] 1"
    )
  }
)

test_that(
  "Cast a message when 0 or 1 is removed from alpha",
  {
    expect_message(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha = c(0, 0.05, 1),
                  tail = c("both", "left"))
    )
  }
)

test_that(
  "Cast an error when 0 or 1 is removed from alpha and no other values are
   given",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha = c(0, 1),
                  tail = c("both", "left"))
    )
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if tail is not a subset of (both, left, right)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha = .05,
                  tail = c("not_valid"))
    )
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha =.05,
                  tail = c("both", "left"),
                  interval_min = "2")
    )
  }
)

test_that(
  "Cast an error if interval_min is not numeric(1)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha =.05,
                  tail = c("both", "left"),
                  interval_min = c(2, 3))
    )
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if interval_max is not numeric(1)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha =.05,
                  tail = c("both", "left"),
                  interval_max = "2")
    )
  }
)

test_that(
  "Cast an error if interval_max is not numeric(1)",
  {
    expect_error(
      interval_t1(E = NULL,
                  s = 6,
                  n = 10,
                  alpha =.05,
                  tail = c("both", "left"),
                  interval_max = c(2, 3))
    )
  }
)

#* Clean up
rm(list = c("student_E", "student_E_onetail", "student_s", "student_s_onetail"))
