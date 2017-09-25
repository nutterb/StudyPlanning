#' @name interval_z1
#' @title Study Parameters for Normal-Distribution based Confidence Intervals
#' 
#' @description The standard normal distribution can be transformed to produce
#'   confidence intervals. These intervals assume the population standard 
#'   deviation is known.  In practice, it is considered better to use the
#'   t-based confidence intervals.  This function permits the user to solve
#'   for the sample size, margin of error, standard deviation, or 
#'   significance level.
#'   
#' @param n integerish value on the interval (2, Inf), the sample size.
#' @param sigma \code{numeric} on the interval (0, Inf), population standard 
#'  deviation
#' @param E \code{numeric} on the interval (-Inf, Inf), margin of error
#' @param alpha \code{numeric} on the interval (0, 1), significance level.
#' @param tail \code{character}, a subset of \code{c("both", "left", "right")}
#' @param interval_min \code{numeric(1)}, the minimum value for the search
#'   interval.  
#' @param interval_max \code{numeric(1)}, the maximum value for the search
#'   interval.
#' @param ... Additional arguments to pass to \code{uniroot}.
#' 
#' @details Exactly one of \code{n}, \code{sigma}, \code{E}, and 
#'  \code{alpha} must be \code{NULL}.  The solution is returned for the 
#'  \code{NULL} value that satisfies the other arguments.
#'  
#' @section Default Interval Limits:
#' \tabular{lll}{
#'   Study Parameter \tab Minimum \tab Maximum \cr
#'   n               \tab 2       \tab 1e7     \cr
#'   sigma           \tab 0       \tab 1e7     \cr
#'   E               \tab -1e7    \tab 1e7     \cr
#'   alpha           \tab 0       \tab 1       
#' }
#' 
#' @return 
#' Returns a data frame with the following columns:
#' \itemize{
#'  \item{\code{n_est} } Estimated sample size.
#'  \item{\code{n }} Sample size rounded up to the next largest integer.
#'  \item{\code{sigma }} Population standard deviation
#'  \item{\code{E }} Margin of error
#'  \item{\code{alpha }} Significance level
#'  \item{\code{tail }} Character value indicating if the test is two-tailed,
#'    left-tailed, or right-tailed.
#' }
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item When \code{n = NULL}, return the value of \code{n} that satisfies 
#'    the other arguments.
#'  \item When \code{sigma = NULL}, return the value of \code{sigma} that 
#'    satisfies the other arguments.
#'  \item When \code{E = NULL}, return the value of \code{E} that satisfies
#'    the other arguments.
#'  \item When \code{alpha = NULL}, return the value of \code{alpha} that
#'    satisfies the other arguments.
#'  \item Cast an error if the number of \code{n}, \code{sigma}, \code{E},
#'    and \code{alpha} that are \code{NULL} is not exactly one.
#'  \item Cast an error if \code{n} is not integerish on the interval [2, Inf).
#'  \item Cast an error if \code{sigma} is not numeric on the interval 
#'    (0, Inf)
#'  \item Cast an error if \code{E} is not numeric.
#'  \item Cast an error if \code{alpha} is not numeric on the interval (0, 1)
#'  \item Cast an error if \code{tail} is not a subset of 
#'    \code{c("both", "left", "right")}
#'  \item Cast an error if \code{interval_min} is not \code{numeric(1)}
#'  \item Cast an error if \code{interval_max} is not \code{numeric(1)}
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' # Example from https://onlinecourses.science.psu.edu/stat200/node/256
#' interval_z1(sigma = 10, E = 2, alpha = 0.10)
#' 
#' # Example from https://www.isixsigma.com/tools-templates/sampling-data/how-determine-sample-size-determining-sample-size/
#' interval_z1(sigma = 6.95, E = 1, alpha = 0.05)
#' 
#' @export

interval_z1 <- function(n = NULL, sigma = NULL, E = NULL, alpha = 0.05,
                        tail = "both", interval_min = NULL,
                        interval_max = NULL, ...)
{
  #******************************************************************
  # Argument Validation
  #******************************************************************
  
  coll <- checkmate::makeAssertCollection()
  
  massert(~ sigma + E + interval_min + interval_max,
          checkmate::assert_numeric,
          lower = list(sigma = 0),
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
  
  plan_args <- list(n_est = n,
                    sigma = sigma,
                    E = E,
                    alpha = alpha)
  
  which_null <- vapply(X = plan_args,
                       FUN = is.null,
                       FUN.VALUE = logical(1))
  
  if (sum(which_null) != 1)
  {
    coll$push("Exactly one of `n`, `sigma`, `E`, and `alpha` must be NULL")
  }
  
  checkmate::reportAssertions(coll)
  
  #******************************************************************
  # Planning function
  #******************************************************************
  
  plan_fn <- function()
  {
    E - qnorm(alpha / ((tail == "both") + 1),
              lower.tail = FALSE) * sigma / sqrt(n_est)
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
        "n" = 2,
        "E" = -1e7,
        0  # alpha and sigma
      )
  }
  
  if (is.null(interval_max))
  {
    interval_max <- 
      switch(
        EXPR = names(plan_args[which_null]),
        "alpha" = 1,
        1e7 # n, E, and sigma
      )
  }
  
  #******************************************************************
  # Parameters Data Frame
  #******************************************************************
  
  .params <- 
    expand.grid(
      n_est = if (is.null(n)) NA else n,
      n = NA,
      sigma = if (is.null(sigma)) NA else sigma,
      E = if (is.null(E)) NA else E,
      alpha = if (is.null(alpha)) NA else alpha,
      tail = tail,
      stringsAsFactors = FALSE
    )

  #******************************************************************
  # Solve for the missing parameter
  #******************************************************************
  
  .params[[names(plan_args)[which_null]]] <- 
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
  
  .params
}