#' @name interval_t1
#' @title Estimate Study Parameters for a One Sample t-Interval
#'
#' @description Estimate a missing study parameter for a one sample t-based 
#'     confidence interval for the population mean, given the remaining 
#'     study parameters are known. The study parameters for the one sample 
#'     t-based interval are \code{E} (margin of error), \code{n} (sample size),
#'     \code{s} (standard deviation) and \code{alpha} (significance level). 
#'     
#'     The t-based confidence interval is commonly used in estimating
#'     population mean using sample data.  It is based on Student's t 
#'     distribution, an adaptation of the standard normal distribution for
#'     cases when the population standard deviation is not known.  
#' 
#' @param E Margin of error (\code{E} > 0)
#' @param n Sample size (\code{n > 1})
#' @param s Sample standard deviation (\code{s > 0})
#' @param alpha Significance level (or 1-confidence; \code{0 < alpha < 1}). For 
#'   convenience, it is permitted to pass values such as \code{seq(0, 1, by = 0.1)}.
#'   Strictly speaking, \code{0} and \code{1} are not on the interval (0, 1), but 
#'   they are removed with a message prior to the argument validation. (Note: this 
#'   could potentially result in an error on the assertion that \code{alpha} have
#'   at length >= 1, if, for example, \code{alpha = c(0, 1)}) 
#' @param two_tail Logical values indicating if the interval is one or two tailed.
#' @param interval_min The minimum value for the search interval in 
#'   \code{uniroot}.  If \code{NULL}, a default value will be chosen based on the 
#'   NULL study parameter.  See Default Interval Limits.
#' @param interval_max The maximum value for the search interval in 
#'   \code{uniroot}.  If \code{NULL}, a default value will be chosen based on the
#'   NULL study parameter.  See Default Interval Limits.
#' 
#' @details Exactly one of the parameters \code{n}, \code{E}, \code{s} and 
#' \code{alpha} must be passed as \code{NULL}, and that parameter will be 
#' calculated from the others.  Notice that \code{alpha} has a 
#' non-\code{NULL} default, so \code{NULL} must be explicitly passed 
#' if you want it computed.
#' 
#' The formula used in parameter calculation is:
#' 
#' \deqn{E = t(alpha, n-1) * s / \sqrt{n}}
#' 
#' @section Default Interval Limits:
#' 
#' \tabular{ccc}{
#'   Study Parameter \tab Lower Limit \tab Upper Limit \cr
#'   \code{E}        \tab \code{0}    \tab \code{1e8}  \cr
#'   \code{n}        \tab \code{2}    \tab \code{1e8}  \cr
#'   \code{s}        \tab \code{0}    \tab \code{1e8}  \cr 
#'   \code{alpha}    \tab \code{0}    \tab \code{1}
#' }
#' 
#' The need for using limits other than the default ought to be rare.  The only
#' purpose for doing so would be to reduce the computational time required
#' to find a solution via \code{uniroot}. But \code{uniroot} is already quite 
#' efficient. Be warned that no validations are performed on the user supplied 
#' interval limits to confirm that the limits are within the domains of each 
#' study parameter.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item{When \code{E = NULL}, correctly calculate the margin of error that
#'     satisfies the other arguments.}
#'   \item{When \code{n = NULL}, correctly calculate the sample size that
#'     satisifies the other arguments.}
#'   \item{When \code{s = NULL}, correctly calculate the standard deviation 
#'     that satisfies the other arguments.}
#'   \item{When \code{alpha = NULL}, correctly calculate the significance 
#'     level that satisfies the other arguments.}
#'   \item Cast an error if the number of \code{NULL} arguments among \code{n},
#'     \code{E}, \code{s}, and \code{alpha} is not 1.
#'   \item{Cast an error when \code{E} is not numeric on the interval (0, Inf)}
#'   \item{Cast an error if \code{n} is not integerish on the interval [2, Inf)}
#'   \item{Cast an error if \code{s} is not numeric on the interval(0, Inf)}
#'   \item{Cast an error if \code{alpha} is not numeric on the interval (0, 1)}
#'   \item{Cast an error if \code{two_tail} is not logical.}
#'   \item{Cast an error if \code{interval_min} is not \code{numeric(1)}}
#'   \item{Cast an error if \code{interval_max} is not \code{numeric(1)}}
#' }
#' 
#' @return A data frame with six columns
#'  \itemize{
#'    \item \code{n_est}: Exact sample size.  When \code{n} is not 
#'        \code{NULL}, this will be identical to \code{n}.
#'    \item \code{n}: Estimated sample size; the next largest integer 
#'        after \code{n_est}.
#'    \item \code{E}: Margin of error
#'    \item \code{s}: Sample Standard Deviation
#'    \item \code{alpha}: Desired significance level.
#'    \item \code{two_tail}: Logical value indicating if the test was two tailed.
#'  }
#'  
#' @source 
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' 
#' # Use Student's sleep data, group 1
#' data(sleep)
#' 
#' student_E <- 
#'   (sleep$extra[sleep$group == 1] %>%
#'   t.test %$%
#'   conf.int %>%
#'   diff) / 2
#' student_s <- sd(sleep$extra[sleep$group == 1])
#' 
#' # Calculate the margin of error 
#' 
#' interval_t1(E = NULL,
#'             s = student_s,
#'             n = 10,
#'             alpha = 0.05,
#'             two_tail = TRUE)
#'             
#' # Calculate sample size for two different significance levels
#' interval_t1(E = student_E,
#'             s = student_s,
#'             alpha = c(0.05, 0.10),
#'             two_tail = TRUE)
#'             
#' # Calculate sample size over a range of margins of error
#' # and with two significance levels
#' (SampleSize <- 
#'   interval_t1(E = seq(1.0, 2.0, by = 0.01),
#'               s = student_s,
#'               alpha = c(0.05, 0.10),
#'               two_tail = TRUE))
#'             
#' library(ggplot2)
#' ggplot(data = SampleSize,
#'        mapping = aes(x = E,
#'                      y = n_est,
#'                      colour = factor(alpha))) + 
#'   geom_line()
#' 
#' @export 

interval_t1 <- function(E=NULL, s=NULL, n=NULL, alpha=.05,
                        two_tail = TRUE,
                        interval_min = NULL,
                        interval_max = NULL)
{
  
  #* The names assigned to this list will be useful later 
  #* when the parameters are estimated.  They are 
  #* assigned here rather than renaming them later.
  plan_args <- list(E = E, 
                    s = s, 
                    n_est = n,
                    alpha_calc = alpha)
  
  #* Provide a logical vector indicating which of the study parameters are NULL.
  #* In the argument checks, this helps cast the error if the sum is not TRUE.
  #* It is used later to determine the formal arguments to the function passed
  #* to uniroot
  which_null <-
    vapply(X = plan_args,
           FUN = is.null,
           FUN.VALUE = logical(1))
  
  #*******************************************************
  #* Argument checks
  #* 1. only one of E, n, s, alpha may be NULL
  #* 2. E must be on the interval (0, Inf)
  #* 3. n must be integerish on the interval [2, Inf)
  #* 4. s must be on the interval (0, Inf)
  #* 5. alpha must be on the interval (0, 1)
  #* 6.  two_tail must be logical
  #*******************************************************
  
  coll <- checkmate::makeAssertCollection()
  
  #* 1.  only one of E, n, s, alpha may be NULL (error)
  if (sum(which_null) != 1)
  {
    coll$add("Exactly one of `E`, `n`, `s`, and `alpha` may be NULL")
  }
  
  #* 2. E must be on the interval (0, Inf)
  if (!is.null(E))
  {
    checkmate::assert_numeric(x = E,
                              lower = 0,
                              min.len = 1,
                              add = coll)
  }
  
  #* 3. s must be on the interval (0, Inf)
  if (!is.null(s))
  {
    checkmate::assert_numeric(x = s,
                              lower = 0,
                              min.len = 1,
                              add = coll)
  }
  
  #* 4. n must be integerish on the interval [2, Inf)
  if (!is.null(n))
  {
    checkmate::assert_integerish(x = n,
                                 lower = 2,
                                 min.len = 1,
                                 add = coll)
  }
  
  #* 5. alpha must be on the interval (0, 1)
  if (!is.null(alpha))
  {
    if (any(alpha %in% c(0, 1)))
    {
      alpha <- alpha[!alpha %in% c(0, 1)]
      message("`alpha` only accepts values on the interval (0, 1). ",
              "Values equal to 0 or 1 have been removed. ",
              "(This could affect the argument validation)")
    }
    checkmate::assert_numeric(x = alpha,
                              lower = 0,
                              upper = 1,
                              min.len = 1,
                              add = coll)
  }
  
  #* 6 two_tail must be logical
  checkmate::assert_logical(x = two_tail,
                            min.len = 1,
                            add = coll)
  
  #* Print errors
  checkmate::reportAssertions(coll)
  
  #******************************************************
  #* Assign additional defaults
  #* 1. Default interval minimum
  #* 2. Default interval maximum
  #******************************************************
  
  #* Default interval minimum
  if (is.null(interval_min))
  {
    interval_min <- 
      switch(
        EXPR = names(plan_args)[which_null],
        n_est = 2,
        0    # default value for everything other than n_est
      )
  }
  
  #* Default interval maximum
  if (is.null(interval_max))
  {
    interval_max <- 
      switch(
        EXPR = names(plan_args)[which_null],
        alpha_calc = 1,
        1e8    # default value for everything other than alpha_calc
      )
  }
  
  #******************************************************
  #* Study Parameter Estimating Function
  #* 1. Estimating Function
  #* 2. Assign formal arguments
  #******************************************************
  
  #* 1. Estimating Function
  plan_fn <- function()
  {
    E - qt(alpha_calc, n_est-1, lower.tail = FALSE) * s / sqrt(n_est)
  }
  
  #* 2. Assign formala arguments. 
  #*    The NULL study parameter has to be first, and then the remaining 
  #*    parameters are assumed to come in the order listed in the arguments
  #*    of interval_t1.
  formals(plan_fn) <-  c(plan_args[which_null], plan_args[!which_null])
  
  #******************************************************
  #* Create the data frame for storing the results
  #******************************************************
  
  .params <- 
    expand.grid(
      n_est = if (is.null(n)) NA else n, #* integerish n is calculated later.
      E = if (is.null(E)) NA else E,
      s = if (is.null(s)) NA else s,
      alpha = if (is.null(alpha)) NA else alpha,
      two_tail = two_tail
    ) %>%
    dplyr::mutate(alpha_calc = alpha / (two_tail + 1))
  
  #******************************************************
  #* Estimate the missing parameter
  #* By using `do.call`, I can pass the list of non-null study parameters
  #* as a single object.  If I were to use `mapply` directly, I would
  #* have to write a single line for each parameter. 
  #******************************************************
  
  .params[[names(plan_args)[which_null]]] <-
    do.call("mapply",
            args = c(.params[names(plan_args)[!which_null]],
                     list(FUN = try_uniroot,
                          MoreArgs = list(f = plan_fn,
                                          interval = c(interval_min, interval_max)),
                          SIMPLIFY = FALSE))) %>%
    vapply(FUN = function(x) x[["root"]],
           FUN.VALUE = numeric(1))
  
  #******************************************************
  #* Final preparations for the results
  #* 1. Calculate the total alpha.
  #* 2. Calculate integerish n
  #* 3. Return results
  #******************************************************
  
  #* 1. Calculate the total alpha
  #* uniroot calculates alpha/2 for two_tail tests.  
  #* If alpha was the missing parameter, it needs to be transformed
  #* to the complete value of alpha
  if (is.null(alpha))
  {
    .params[["alpha"]] <- .params[["alpha_calc"]] * (.params[["two_tail"]] + 1)
  }
  
  #* 2. Calculate integerish n
  #* If n was the missing parameter, it is the ceiling of n_est
  #* otherwise it is the same as n_est
  if (is.null(n))
  {
    .params[["n"]] <- ceiling(.params[["n_est"]])
  }
  else
  {
    .params[["n"]] <- .params[["n_est"]]
  }
  
  #* 3. Return the results
  .params[c("n_est", "n", "E", "s", "alpha", "two_tail")]
}  

utils::globalVariables(c("alpha_calc", "n_est"))
