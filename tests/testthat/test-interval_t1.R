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

test_that(
  "Req 1. Accepts values of `E` on the interval (0, Inf)",
  {
    expect_silent({
      interval_t1(E = student_E,
                  s = student_s,
                  alpha = 0.05,
                  two_tail = TRUE) 
    })
  }
)

test_that(
  "Req 1. Cast an error if `E` has values outside (0, Inf).",
  {
    expect_error({
      interval_t1(E = c(-2, student_E),
                  s = student_s,
                  alpha = 0.05,
                  two_tail = TRUE)
    })
  }
)


test_that(
  "Req 2. Accepts values of `s` on the interval (0, Inf)",
  {
    interval_t1(E = student_E,
                s = student_s,
                alpha = 0.05,
                two_tail = TRUE) %>%
      expect_silent()
  }
)

test_that(
  "Req 2. Cast an error if `s` has values outside (0, Inf).",
  {
    expect_error({
      interval_t1(E = student_E,
                  s = c(-10, student_s),
                  alpha = 0.05,
                  two_tail = TRUE)
    })
  }
)



test_that(
  "Req 3. Accepts integerish values of `n` on the interval [2, Inf)",
  {
    expect_silent({
      interval_t1(E = student_E,
                  s = NULL,
                  n = 10,
                  alpha = 0.05,
                  two_tail = TRUE)
    })
  }
)

test_that(
  "Req 3. Cast an error if `n` has values outside [2, Inf) or non-integerish values.",
  {
    expect_error({
      interval_t1(E = student_E,
                  s = NULL,
                  n = c(1, 10),
                  alpha = 0.05,
                  two_tail = TRUE)
    })
  }
)



test_that(
  "Req 4. Accepts values for `alpha` on the interval (0, Inf)",
  {
    expect_silent({
      interval_t1(E = student_E,
                  s = NULL,
                  n = 10,
                  alpha = 0.05,
                  two_tail = TRUE)
    })
  }
)

test_that(
  "Req 4. Cast an error if `alpha` has values outside (0, Inf).",
  {
    expect_error({
      interval_t1(E = student_E,
                  s = NULL,
                  n = 10,
                  alpha = c(-2, 0.05),
                  two_tail = TRUE)
    })
  }
)


test_that(
  "Req 5. Permit one and two tailed calculations",
  {
    expect_silent({
      interval_t1(E = NULL,
                  s = student_s,
                  n = 10,
                  alpha = 0.05,
                  two_tail = c(TRUE, FALSE))
    })
  }
)


#**************************************************
#* Now let's make sure the calculations are correct

test_that(
  "Req. 6: Calculation of E is correct",
  {
    expect_equal(
      {interval_t1(E = NULL,
                  s = student_s,
                  n = 10,
                  alpha = 0.05,
                  two_tail = c(TRUE, FALSE)) %$%
        E},
      c(student_E, student_E_onetail)
    )
  }
)


test_that(
  "Req. 6: Calculation of s is correct",
  {
    expect_equal(
      {interval_t1(E = student_E,
                   s = NULL,
                   n = 10,
                   alpha = 0.05,
                   two_tail = c(TRUE, FALSE)) %$%
          s},
      c(student_s, student_s_onetail)
    )
  }
)


test_that(
  "Req. 6: Calculation of n is correct",
  {
    expect_equal(
      {interval_t1(E = student_E,
                   s = student_s,
                   n = NULL,
                   alpha = 0.05,
                   two_tail = c(TRUE, FALSE)) %$%
          n_est},
      c(10.0000197573725, 7.26720870767518)
    )
  }
)


test_that(
  "Req. 6: Calculation of alpha is correct",
  {
    expect_equal(
      {interval_t1(E = student_E,
                   s = student_s,
                   n = 10,
                   alpha = NULL,
                   two_tail = c(TRUE, FALSE)) %$%
          alpha},
      c(0.0499414102928245, 0.0249707051464123)
    )
  }
)

#* Clean up
rm(list = c("student_E", "student_E_onetail", "student_s", "student_s_onetail"))
