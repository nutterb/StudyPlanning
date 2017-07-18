#' @name test_binomial
#' @export test_binomial
#'
#' @title Power and Sample Size Analysis for a Binomial Test
#' @description Determines the sample size, power, null proportion,
#'   alternative proportion, or significance level for a binomial test.
#'   The results also return the actual power and significance.
#'
#' @param n The sample size, or number of trials
#' @param p0 The value of the probability of a success under the null hypothesis
#' @param p1 The value of the probability of a success under the alternative hypothesis
#' @param power The power of the test
#' @param alpha Significance level for the test
#' @param alternative A character vector giving the alternative to the test.
#'   Multiple values may be given, but the values must be \code{"two.tailed"},
#'   \code{"left.tailed"}, or \code{"right.tailed"}.
#' @param conservative A logical vector.  This determines if the sample
#'   size selected is conservative (larger).  This decision is required
#'   because the power as a function of sample size is non-montonic in
#'   the binomial distribution.  In practice, it is usually better to look
#'   at both and select the sample size where \code{alpha_actual} is closest
#'   to \code{alpha}.
#' @param n_limits The limits of the search for when \code{n=NULL}.  The
#'   sample size is determined in a manner similar to \code{uniroot}, but
#'   \code{uniroot} doesn't handle discrete values.
#'
#' @details Exactly one of the parameters \code{n}, \code{p0},
#'   \code{p1}, \code{alpha}, and \code{power} must be passed as \code{NULL}.  The only
#'   exception is that \code{delta} may be passed as a second \code{NULL} when
#'   \code{mu0} and \code{mu1} are specified.
#'
#'   The parameters are combined via \code{expand.grid}, so all combinations
#'   of the inputs are evaluated.
#'
#' @section Default Interval Limits:
#' \tabular{ccc}{
#'   Study Parameter \tab Lower Limit \tab Upper Limit \cr
#'   \code{n}        \tab 2           \tab 1e7         \cr
#'   \code{p0}       \tab 0           \tab 1           \cr
#'   \code{p1}       \tab 0           \tab 1           \cr
#'   \code{power}    \tab 0           \tab 1           \cr
#'   \code{alpha}    \tab 0           \tab 1
#' }
#' 
#' @return Returns a data frame with the following columns.
#' \itemize{
#'  \item{\code{p0} } The population proportion under the null hypothesis.
#'  \item{\code{p1} } The population proportion under the alternative
#'    hypothesis.
#'  \item{\code{alpha} } The nominal, or desired, significance level.
#'  \item{\code{alpha_actual} } The actual significance level.
#'  \item{\code{power} } The nominal, or desired, power.
#'  \item{\code{power_actual} } The actual power.
#'  \item{\code{n} } Sample size
#'  \item{\code{tail} } Character string indicating if the test was left
#'    tailed, right tailed, or two tailed.
#'  \item{\code{conservative} } Logical value indicating if the the 
#'    sample size reflects a more conservative estimate. See the 
#'    \code{conservative} argument.
#' }
#'
#' @author Benjamin Nutter
#'
#' @source 
#' O'Brien R, Castelloe J, "Sample-Size Analysis in Study Planning,"
#'   American Statistical Association Continuing Education Program: Section on
#'   Teaching Statistics in the Health Sciences, Joint Statistical Meetings,
#'   San Francisco, CA; 5 August 2003 (Short Course Manual)
#'
#' Some design choices were obtained from the r-help question at:
#' \url{http://r.789695.n4.nabble.com/Sample-size-calculations-for-one-sided-binomial-exact-test-td3964313.html}
#'
#' Formal theoretical considerations are described at \url{http://nutterb.github.io/ItCanBeShown/binomial-test.html}
#'
#' @section Functional Requirements:
#' \enumerate{
#'   \item When \code{n = NULL} correctly calculate the estimated sample size
#'     that satisfies the other arguments.
#'   \item When \code{p0 = NULL}, correctly calculate the value of \code{p0}
#'     that satisfies the other arguments.
#'   \item When \code{p1 = NULL}, correctly calculate the value of \code{p1}
#'     that satisfies the other arguments.
#'   \item When \code{power = NULL}, correctly calculate the value of \code{power}
#'     that satisfies the other arguments.
#'   \item When \code{alpha = NULL}, correctly calculate the value of \code{alpha}
#'     that satisfies the other arguments.
#'   \item Correctly adjust values for the chosen value of \code{alternative}
#'   \item Correctly adjust the values returned for the chosen value of
#'     \code{conservative}
#'   \item Cast an error if the number of \code{NULL} arguments among \code{n},
#'     \code{p0}, \code{p1}, \code{power}, and \code{alpha} is not 1.
#'   \item Cast an error if \code{n} is not integerish.
#'   \item Cast an error if \code{p0} is not numeric on the interval (0, 1)
#'   \item Cast an error if \code{p1} is not numeric on the interval (0, 1)
#'   \item Cast an error if \code{power} is not numeric on the interval (0, 1)
#'   \item Cast an error if \code{alpha} is not numeric on the interval (0, 1)
#'   \item Cast an error if \code{tail} is not one of \code{c("both", "left", "right")}
#'   \item Cast an error if \code{conservative} is not logical.
#'   \item Retain only unique values of \code{conservative}.
#'   \item Cast an error if \code{interval_min} is not \code{numeric(1)}.
#'   \item Cast an error if \code{interval_max} is not \code{numeric(1)}.
#' }
#'
#' @examples
#' #* Julia Chill's Frozen Sensations Example from O'Brien and Castelloe
#' test_binomial(n=c(20, 40), p0=.5, p1=.8, alpha=c(.01, .05),
#'               tail='right')
#'
#' #* Plot the sample size for a range of n
#' library(ggplot2)
#' Chill <- test_binomial(n=20:40, p0=.5, p1=.8, alpha=c(.01, .05),
#'               tail='right.tailed')
#'
#' ggplot(Chill, aes(x=n, y=power, colour=factor(alpha))) + geom_line()
#'

test_binomial <- function(n = NULL, p0 = NULL, p1 = NULL,
                          power = NULL, alpha=.05,
                          tail = "both",
                          conservative = FALSE,
                          interval_min = NULL,
                          interval_max = NULL,
                          ...){
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = n,
                               null.ok = TRUE,
                               add = coll)

  for (arg in c("p0", "p1", "alpha", "power"))
  {
    assign(arg, 
           remove_limit(x = get(arg),
                        coll = coll,
                        .var.name = arg,
                        null.ok = TRUE))
  }
    
  massert(~ interval_min + interval_max,
          checkmate::assert_numeric,
          fixed = list(null.ok = TRUE,
                       add = coll))
  
  checkmate::assert_subset(x = tail,
                           choices = c("both", "left", "right"),
                           add = coll)
  
  checkmate::assert_logical(x = conservative,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  tail <- unique(tail)
  
  conservative <- unique(conservative)
  
  # Secondary argument check, make sure exactly one of
  # n, p0, p1, power, or alpha is NULL
  
  plan_args <- list(n = n,
                    p0 = p0,
                    p1 = p1,
                    power = power,
                    alpha = alpha,
                    tail = character(0))
  
  which_null <-
    vapply(plan_args,
           is.null,
           logical(1))
  
  if (sum(which_null) != 1)
  {
    coll$push("Exactly one of n, p0, p1, power, and alpha may be NULL")
  }
  
  checkmate::reportAssertions(coll)
  
  # Default interval values
  if (is.null(interval_min))
  {
    interval_min <-
      switch(
        names(plan_args)[which_null],
        "n" = 2,
        0 # Default value
      )
  }
  
  if (is.null(interval_max))
  {
    interval_max <-
      switch(
        names(plan_args)[which_null],
        "n" = 1e7,
        1 # default value
      )
  }
  
  # Power Function
  
  plan_fn <- function()
  {
    if (tail == "both")
    {
      power -
        (pbinom(q = qbinom(p = alpha / 2,
                           size = n,
                           prob = p0),
                size = n,
                prob = p1) +
           pbinom(q = qbinom(p = alpha/2,
                             size = n,
                             prob = p0,
                             lower.tail=FALSE),
                  size = n,
                  prob = p1,
                  lower.tail=FALSE))
    }
    else if (tail == "left")
    {
      power -
        pbinom(q = qbinom(p = alpha,
                          size = n,
                          prob = p0),
               size = n,
               prob = p1)
    }
    else
    {
      power -
        pbinom(q = qbinom(p = alpha,
                          size = n,
                          prob = p0,
                          lower.tail=FALSE),
               size = n,
               prob = p1,
               lower.tail=FALSE)
    }
  }
  
  # Set the NULL argument to the first argument in plan_fn
  # This sets it to be the argument for which uniroot will solve
  formals(plan_fn) <- c(plan_args[which_null],
                        plan_args[!which_null])
  
  # Output Data Frame
  .params <- expand.grid(p0 = if (is.null(p0)) NA else p0,
                         p1 = if (is.null(p1)) NA else p1,
                         alpha = if (is.null(alpha)) NA else alpha,
                         alpha_actual = NA,
                         power = if (is.null(power)) NA else power,
                         power_actual = NA,
                         n = if (is.null(n)) NA else n,
                         tail = tail,
                         conservative = conservative,
                         stringsAsFactors = FALSE)

  # Calculate the NULL parameter
  .params[[names(plan_args)[which_null]]] <-
    vapply(
      do.call("mapply",
              args = c(.params[names(plan_args)[!which_null]],
                       list(FUN = try_uniroot,
                            MoreArgs = c(list(f = plan_fn,
                                              interval = c(interval_min, interval_max),
                                              integer = is.null(n)),
                                         list(...)),
                            SIMPLIFY = FALSE))),
      FUN = function(x) x[["root"]],
      FUN.VALUE = numeric(1)
    )
  
  .params[["n"]][.params[["conservative"]]] <- 
    .params[["n"]][.params[["conservative"]]] + 1
  
  if (!is.null(power))
  {
    .params[["alpha_actual"]] <- 
      (mapply(
        FUN = plan_fn,
        n = .params[["n"]],
        p0 = .params[["p0"]],
        p1 = .params[["p0"]],
        alpha = .params[["alpha"]],
        power = .params[["power"]],
        tail = .params[["tail"]]
      ) - power) * -1
    
    .params[["power_actual"]] <- 
      (mapply(
        FUN = plan_fn,
        n = .params[["n"]],
        p0 = .params[["p0"]],
        p1 = .params[["p1"]],
        alpha = .params[["alpha"]],
        power = .params[["power"]],
        tail = .params[["tail"]]
      ) - power) * -1
  }
  .params
}
