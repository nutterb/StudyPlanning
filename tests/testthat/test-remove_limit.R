context("remove_limit.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a vector of any elements that are not equal to lower or upper",
  {
    expect_equal(
      suppressMessages(remove_limit(seq(0, 1, by = .1))),
      seq(0.1, 0.9, by = .1)
    )
  }
)

test_that(
  "Return a vector of any elements that are not equal to lower or upper",
  {
    expect_equal(
      suppressMessages(remove_limit(seq(.1, 1, by = .1))),
      seq(0.1, 0.9, by = .1)
    )
  }
)

test_that(
  "Return a vector of any elements that are not equal to lower or upper",
  {
    expect_equal(
      suppressMessages(remove_limit(1:10, lower = 1, upper = 10)),
      2:9
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast a message if any elements of x are removed",
  {
    expect_message(
      remove_limit(seq(0, 1, by = .1))
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "If error_none_left = TRUE, cast an error if there are no values in 
   x after the endpoints are removed.",
  {
    expect_error(
      remove_limit(0:1),
      "has length 0"
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If error_none_left = FALSE, return an empty vector if there are no 
   values in x after the endpoints are removed",
  {
    expect_equal(
      remove_limit(0:1, error_none_left = FALSE),
      numeric(0)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "If assert_between = TRUE, cast an error if any values in x are less than
   lower or greater than upper",
  {
    expect_error(
      remove_limit(-1:2)
    )
  }
)

test_that(
  "If assert_between = TRUE, cast an error if any values in x are less than
  lower or greater than upper",
  {
    expect_error(
      remove_limit(0:2)
    )
  }
)

test_that(
  "If assert_between = TRUE, cast an error if any values in x are less than
  lower or greater than upper",
  {
    expect_error(
      remove_limit(-1:1)
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "If assert_between = FALSE, return x after the endpoints are removed",
  {
    expect_equal(
      remove_limit(-1:2, assert_between = FALSE),
      c(-1, 2)
    )
  }
)
# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if x is not numeric",
  {
    expect_error(
      remove_limit(letters),
      "Must be of type 'numeric'"
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if lower is not numeric(1)",
  {
    expect_error(
      remove_limit(runif(10), lower = "0"),
      "Must be of type 'numeric'"
    )
  }
)

test_that(
  "Cast an error if lower is not numeric(1)",
  {
    expect_error(
      remove_limit(runif(10), lower = c(-1, 0)),
      "Must have length 1"
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if upper is not numeric(1)",
  {
    expect_error(
      remove_limit(runif(10), upper = "1"),
      "Must be of type 'numeric'"
    )
  }
)

test_that(
  "Cast an error if upper is not numeric(1)",
  {
    expect_error(
      remove_limit(runif(10), upper = c(1, 2)),
      "Must have length 1"
    )
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if coll is not an AssertCollection",
  {
    expect_error(
      remove_limit(runif(10), coll = "not assert"),
      "Must have class 'AssertCollection'"
    )
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if error_none_left is not logical(1)",
  {
    expect_error(
      remove_limit(runif(10), error_none_left = "TRUE"),
      "Must be of type 'logical'"
    )
  }
)

test_that(
  "Cast an error if error_none_left is not logical(1)",
  {
    expect_error(
      remove_limit(runif(10), error_none_left = c(TRUE, FALSE)),
      "Must have length 1"
    )
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if assert_between is not logical(1)",
  {
    expect_error(
      remove_limit(runif(10), assert_between = "TRUE"),
      "Must be of type 'logical'"
    )
  }
)

test_that(
  "Cast an error if assert_between is not logical(1)",
  {
    expect_error(
      remove_limit(runif(10), assert_between = c(TRUE, FALSE)),
      "Must have length 1"
    )
  }
)

# Functional Requirement 13 -----------------------------------------

test_that(
  "Cast an error if .var.name is not character(1)",
  expect_error(
    remove_limit(runif(10), .var.name = 3),
    "Must be of type 'character'"
  )
)

test_that(
  "Cast an error if .var.name is not character(1)",
  expect_error(
    remove_limit(runif(10), .var.name = letters),
    "Must have length 1"
  )
)
