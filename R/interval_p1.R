#' @name interval_p1
#' @title Study Parameters for One Sample Z-Test of Proportions
#' 
#' @description The one sample z-test of proportions may be used to design
#' interval estimation studies based on a normal approximation to the 
#' Binomial distribution.  Study parameters include the sample size, margin
#' of error, hypothesized proportion, and significance level.
#' 
#' @param n integerish value, the sample size. Must be on the interval 
#'   [2, Inf)
#' @param E numeric, the margin of error. Must be on the interval (-1, 1)
#' @param p numeric, the hypothesize proportion. Must be on the interval (0, 1)
#' @param alpha numeric, the significance level. Must be on the interval (0, 1)
#' @param tail character, determines the direction of the confidence interval.
#'   Must be a subset of \code{c("both", "left", "right")}
#' @param upper code{logical(1)}. When \code{FALSE}, the default interval when 
#'   solving for \code{p} is \code{c(0, 0.5)}, otherwise, it is 
#'   \code{c(0.5, 1.0)}.
#' @param interval_min numeric, the minimum value of the search interval.
#' @param interval_max numeric, the maximum value of the search interval.
#' @param ... additional arguments to pass to \code{uniroot}
#'
#' @details Exactly one of \code{n}, \code{p}, \code{E}, and 
#'  \code{alpha} must be \code{NULL}.  The solution is returned for the 
#'  \code{NULL} value that satisfies the other arguments.
#'  
#' @section Default Interval Limits:
#' \tabular{lll}{
#'   Study Parameter \tab Minimum \tab Maximum \cr
#'   n               \tab 2       \tab 1e7     \cr
#'   p              \tab 0       \tab 1     \cr
#'   E               \tab -1      \tab 1     \cr
#'   alpha           \tab 0       \tab 1       
#' }
#' 
#' @return 
#' Returns a data frame with the following columns:
#' \itemize{
#'  \item{\code{n_est} } Estimated sample size.
#'  \item{\code{n }} Sample size rounded up to the next largest integer.
#'  \item{\code{p }} Hypothesized proportion
#'  \item{\code{E }} Margin of error
#'  \item{\code{alpha }} Significance level
#'  \item{\code{tail }} Character value indicating if the test is two-tailed,
#'    left-tailed, or right-tailed.
#'  \item{\code{np }} The product of \code{n} and \code{p}. 
#'  \item{\code{nq }} The product of \code{n} and \code{1 - p}.
#' }
#' 
#' \code{np} and \code{nq} are provided to assist in evaluating if the study
#' parameters may satisfy the assumptions of the Normal approximation to the
#' Binomial distribution.  Typically, these values must both be greater than
#' five or ten.
#' 
#' @author Benjamin Nutter
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item When \code{n = NULL}, return the value of \code{n} that satisfies 
#'    the other arguments.
#'  \item When \code{p = NULL}, return the value of \code{p} that 
#'    satisfies the other arguments.
#'  \item When \code{E = NULL}, return the value of \code{E} that satisfies
#'    the other arguments.
#'  \item When \code{alpha = NULL}, return the value of \code{alpha} that
#'    satisfies the other arguments.
#'  \item Cast an error if the number of \code{n}, \code{p}, \code{E},
#'    and \code{alpha} that are \code{NULL} is not exactly one.
#'  \item Cast an error if \code{n} is not integerish on the interval [2, Inf).
#'  \item Cast an error if \code{p} is not numeric on the interval 
#'    (-1, 1)
#'  \item Cast an error if \code{E} is not numeric.
#'  \item Cast an error if \code{alpha} is not numeric on the interval (0, 1)
#'  \item Cast an error if \code{tail} is not a subset of 
#'    \code{c("both", "left", "right")}
#'  \item Cast an error if \code{interval_min} is not \code{numeric(1)}
#'  \item Cast an error if \code{interval_max} is not \code{numeric(1)}
#'  \item Cast en error if \code{upper} is not \code{logical(1)}
#' }
#' 
#' @examples 
#' # Example from https://onlinecourses.science.psu.edu/stat500/node/31
#' interval_p1(p = 0.72, E = 0.01, alpha = 0.05)
#'
#' @export
interval_p1 <- function(n = NULL, E = NULL, p = 0.50, alpha = 0.05,
                        tail = "both", upper = FALSE, 
                        interval_min = NULL,
                        interval_max = NULL, ...)
{
  #******************************************************************
  # Argument Validation
  #******************************************************************
  
  coll <- checkmate::makeAssertCollection()
  
  massert(~ p + E + interval_min + interval_max,
          checkmate::assert_numeric,
          lower = list(p = 0, E = -1),
          upper = list(p = 1, E = 1),
          len = list(interval_min = 1, interval_max = 1),
          fixed = list(add = coll, 
                       null.ok = TRUE))
  
  alpha <- remove_limit(x = alpha,
                        null.ok = TRUE,
                        coll = coll,
                        .var.name = "alpha")
  
  checkmate::assert_integerish(x = n,
                               lower = 2,
                               null.ok = TRUE, 
                               add = coll)
  
  checkmate::assert_subset(x = tail,
                           choices = c("both", "left", "right"),
                           add = coll)
  
  checkmate::assert_logical(x = upper,
                            len = 1,
                            add = coll)
  
  plan_args <- list(n_est = n,
                    p = p,
                    E = E,
                    alpha = alpha)
  
  which_null <- vapply(X = plan_args,
                       FUN = is.null,
                       FUN.VALUE = logical(1))
  
  if (sum(which_null) != 1)
  {
    coll$push("Exactly one of `n`, `p`, `E`, and `alpha` must be NULL")
  }
  
  checkmate::reportAssertions(coll)
  
  #******************************************************************
  # Planning function
  #******************************************************************
  
  plan_fn <- function()
  {
    E - qnorm(alpha / ((tail == "both") + 1),
              lower.tail = FALSE) * sqrt((p * (1 - p)) / n_est)
  }
  
  formals(plan_fn) <- c(plan_args[which_null],
                        plan_args[!which_null],
                        list(tail = tail))
  
  #******************************************************************
  # Default Interval Limits
  #******************************************************************
  
  if (is.null(interval_min))
  {
    interval_min <- 
      switch(
        EXPR = names(plan_args)[which_null],
        "n_est" = 2,
        "E" = -1,
        "p" = 0 + 0.5 * upper,
        0  # alpha
      )
  }
  
  if (is.null(interval_max))
  {
    interval_max <- 
      switch(
        EXPR = names(plan_args[which_null]),
        "n_est" = 1e7, # n
        "p" = 0.5 + 0.5 * upper,
        1          # E, alpha,
      )
  }

  #******************************************************************
  # Parameters Data Frame
  #******************************************************************
  
  .params <- 
    expand.grid(
      n_est = if (is.null(n)) NA else n,
      n = NA,
      p = if (is.null(p)) NA else p,
      E = if (is.null(E)) NA else E,
      alpha = if (is.null(alpha)) NA else alpha,
      tail = tail,
      stringsAsFactors = FALSE
    )

  #******************************************************************
  # Solve for the missing parameter
  #******************************************************************

  .params[[names(plan_args)[which_null]]] <-
    do.call("mapply",
            args = c(.params[names(plan_args)[!which_null]],
                     .params["tail"],
                     list(FUN = try_uniroot,
                          MoreArgs = list(f = plan_fn,
                                          interval = c(interval_min, interval_max)),
                          SIMPLIFY = FALSE))) %>%
    vapply(FUN = function(x) x[["root"]],
           FUN.VALUE = numeric(1))

  .params[["n"]] <- ceiling(.params[["n_est"]])
  .params[["np"]] <- with(.params, n * p)
  .params[["nq"]] <- with(.params, n * (1 - p))
  
  .params
}
