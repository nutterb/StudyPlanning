#' @name test_z1
#' @title One Sample Z Test of a Sample Mean
#' 
#' @description The one sample Z test of a sample mean may be used to test the
#'   hypothesis that an observed sample mean differs from a predetermined
#'   constant.  The test assumes that the data are approximately normally 
#'   distributed with a known population variance.
#'   
#' @param delta \code{numeric}. The magnitude of the difference between
#'   the values of the mean under the null and alternative hypotheses.  
#'   This may alternatively be specified by providing \code{mu_null} and 
#'   \code{mu_alt}.
#' @param n \code{numeric} on the interval [2, Inf). The sample size.
#' @param sigma \code{numeric} on the interval (0, Inf). The population
#'   standard deviation.
#' @param alpha \code{numeric} on the interval (0, 1). The significance
#'   level of the test.
#' @param power \code{numeric} on the interval (0, 1). The power of the
#'   test.
#' @param tail \code{character} specifying the direction of the test.
#'   must be one of \code{c("both", "left", "right")}
#' @param mu_null \code{numeric} the value of the mean under the null
#'   hypothesis. See Details.
#' @param mu_alt \code{numeric} the value of the mean under the alternative
#'   hypothesis. See Details.
#' @param ... Additional arguments to pass to \code{uniroot}
#'   
#' @details Exactly one of \code{delta}, \code{n}, \code{sigma}, 
#'   \code{alpha}, and \code{power} must be \code{NULL}.  \code{delta}
#'   may be a second \code{NULL} onl if \code{mu_null} and \code{mu_alt}
#'   are both provided. In this case, \code{delta} is calculated as 
#'   \code{mu_alt} - \code{mu_null}.
#'   
#' @section Default Interval Limits:
#' \tabular{lll}{
#'   Study Parameter \tab Minimum \tab Maximum \cr
#'   delta           \tab -1e7    \tab 1e7     \cr
#'   n               \tab 2       \tab 1e7     \cr
#'   sigma           \tab 0       \tab 1e7     \cr
#'   alpha           \tab 0       \tab 1       \cr
#'   power           \tab 0       \tab 1 
#' }
#' 
#' @return 
#' Returns a data frame with the following columns:
#' \itemize{
#'  \item{\code{delta }} Estimated difference.
#'  \item{\code{n_est} } Estimated sample size.
#'  \item{\code{n }} Sample size rounded up to the next largest integer.
#'  \item{\code{sigma }} Population standard deviation
#'  \item{\code{alpha }} Significance level
#'  \item{\code{power }} Power of the test 
#'  \item{\code{tail }} Character value indicating if the test is two-tailed,
#'    left-tailed, or right-tailed.
#' }
#' 
#' If \code{mu_null} and \code{mu_alt} are used to calculate \code{delta}, 
#' the data frame will also contain columns for these values.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item When \code{delta = NULL}, calculate the value of \code{delta} 
#'    that satisfies the other requirements.
#'  \item When \code{n = NULL}, calculate the value of \code{n} that 
#'    satisfies the other requirements
#'  \item When \code{sigma = NULL}, calculate the value of \code{sigma}
#'    that satisfies the other requirements.
#'  \item When \code{alpha = NULL}, calculate the value of \code{alpha}
#'    that satisfies the other requirements.
#'  \item When \code{power = NULL}, calculate the value of \code{power}
#'    that satisfies the other requirements.
#'  \item Cast an error when the number of \code{delta}, \code{n},
#'    \code{sigma}, \code{alpha}, \code{power} is not exactly one.
#'  \item Calculate \code{delta} from \code{mu_null} and \code{mu_alt}
#'  \item Ignore \code{mu_null} and \code{mu_alt} with a warning when 
#'    \code{delta} is not \code{NULL}
#'  \item Cast an error when \code{delta = NULL} and \code{mu_null} and 
#'    \code{mu_alt} are not both given (if \code{delta} is the only 
#'    \code{NULL} study parameter).
#'  \item Cast an error when \code{delta} is not numeric.
#'  \item Cast an error when \code{n} is not numeric on the interval [2, Inf)
#'  \item Cast an error when \code{sigma} is not numeric on the interval 
#'    (0, Inf)
#'  \item Cast an error when \code{alpha} is not numeric on the interval 
#'    (0, 1)
#'  \item Cast an error when \code{power} is not numeric on the interval 
#'    (0, 1)
#'  \item Cast an error when \code{tail} is not a subset of 
#'    \code{c("both", "left", "right")}
#'  \item Cast an error when \code{mu_null} is not numeric.
#'  \item Cast an error when \code{mu_alt} is not numeric.
#'  \item Cast an error when \code{interval_min} is not \code{numeric(1)}.
#'  \item Cast an error when \code{interval_max} is not \code{numeric(1)}.
#' }
#' 
#' @examples 
#' test_z1(delta = 5, sigma = 15, n = 20, alpha = 0.05, power = NULL)
#' 
#' @export


test_z1 <- function(delta = NULL, n = NULL, sigma = NULL, alpha = 0.05,
                    power = 0.80, tail = "both",
                    mu_null = NULL, mu_alt = NULL, 
                    interval_min= NULL,
                    interval_max = NULL, ...)
{
  coll <- checkmate::makeAssertCollection()
  
  massert(~ delta + sigma + mu_null + mu_alt + interval_min + interval_max,
          checkmate::assert_numeric,
          len = list(interval_min = 1, interval_max = 1),
          lower = list(sigma = 0),
          fixed = list(null.ok = TRUE,
                       add = coll))
  
  checkmate::assert_integerish(x = n,
                               lower = 2,
                               null.ok = TRUE,
                               add = coll)
  
  alpha <- remove_limit(x = alpha, 
                        coll = coll,
                        .var.name = "alpha",
                        null.ok = TRUE)
  
  power <- remove_limit(x = power,
                        coll = coll,
                        .var.name = "power",
                        null.ok = TRUE)
  
  checkmate::assert_subset(x = tail,
                           choices = c("both", "left", "right"),
                           add = coll)
  
  plan_args <- list(n_est = n,
                    sigma = sigma,
                    delta = delta,
                    alpha = alpha,
                    power = power)
  
  which_null <- vapply(X = plan_args,
                       FUN = is.null,
                       FUN.VALUE = logical(1))
  
  # delta is calculated when both `mu_null` and `mu_alt` are not NULL
  if (is.null(delta))
  {
    which_null["delta"] <- 
      as.logical(which_null["delta"] - (!is.null(mu_null) && !is.null(mu_alt)))
  }
  
  if (sum(which_null) != 1)
  {
    coll$push("Exactly one of `n`, `sigma`, `delta`, `alpha`, and `power` must be NULL")
  }
  
  checkmate::reportAssertions(coll)
  
  if (!is.null(delta) && !is.null(mu_null) && !is.null(mu_alt))
  {
    warning("`delta` is non-null; `mu_null` and `mu_alt` will be ignored")
    mu_null <- NULL
    mu_alt <- NULL
  }
  
  #******************************************************************
  #* Planning Function 
  #******************************************************************
  
  plan_fn <- function()
  {
    if (tail == "both")
    {
      power - 
        (pnorm(qnorm(alpha/2, lower.tail = TRUE) + 
                 sqrt(n_est) * -delta / sigma, 
               lower.tail = TRUE) + 
           pnorm(qnorm(alpha/2, lower.tail = FALSE) + 
                   sqrt(n_est) * -delta / sigma,
                 lower.tail = FALSE))
    }
    else if (tail == "left")
    {
      power - 
        pnorm(qnorm(alpha, lower.tail = TRUE) + 
                sqrt(n_est) * -delta / sigma, 
              lower.tail = TRUE)
    }
    else
    {
      power - pnorm(qnorm(alpha, lower.tail = FALSE) + 
              sqrt(n_est) * -delta / sigma,
            lower.tail = FALSE) 
    }
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
        "delta" = -1e7,
        0 # sigma, alpha, power 
      )
  }
  
  if (is.null(interval_max))
  {
    interval_max <- 
      switch(
        EXPR = names(plan_args)[which_null],
        "alpha" = 1,
        "power" = 1,
        1e7 #delta, sigma, n_est
      )
  }
  
  #******************************************************************
  # Parameters Data Frame
  #******************************************************************
  
  .params <- 
    expand.grid(
      n_est = if (is.null(n)) NA else n,
      n = NA,
      delta = if (is.null(delta)) NA else delta,
      sigma = if (is.null(sigma)) NA else sigma,
      alpha = if (is.null(alpha)) NA else alpha,
      power = if (is.null(power)) NA else power,
      tail = tail,
      stringsAsFactors = FALSE
    )
  
  if (is.null(delta) && !is.null(mu_null) && !is.null(mu_alt))
  {
    delta_frame <- expand.grid(mu_null = mu_null,
                               mu_alt = mu_alt)
    delta_frame$delta <- with(delta_frame, mu_alt - mu_null)
    .params$delta <- NULL
    
    .params <- expand.grid.df(.params, delta_frame)
  }
  
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