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
#'     distribution, an adaptation of the standard normal distribution.  
#' 
#' @param E Margin of error (\code{E} > 0)
#' @param n Sample size (\code{n > 1})
#' @param s Sample standard deviation (\code{s > 0})
#' @param alpha Significance level (or 1-confidence; \code{0 < alpha < 1})
#' @param two_tail Logical values indicating if the interval is one or two tailed.
#' @param interval_min The minimum value for the search interval in 
#'   \code{uniroot}.  If \code{NULL}, a default value will be chosen based on the 
#'   NULL study parameter.  See Default Interval Limits.
#' @param interval_max The maximum value for the search interval in 
#'   \code{uniroot}.  If \code{NULL}, a default value will be chosen based on the
#'   NULL study parameter.  See Default Interval Limits.
#' @param cl A parallel computing cluster object that inherits class \code{cluster}.
#'   When \code{NULL}, estimation is done using \code{mapply}, otherwise 
#'   \code{clusterMap} is used. 
#' @param ncores An integerish value designating the number of cores to use in a 
#'   parallel cluster.  Ignored if \code{cl} is not \code{NULL}.
#' 
#' @details Exactly one of the parameters \code{n}, \code{E}, \code{s} and 
#' \code{alpha} must be passed as \code{NULL}, and that parameter will be 
#' calculated from the others.  Notice that \code{alpha} has a 
#' non-\code{NULL} default, so \code{NULL} must be explicitly passed 
#' if you want it computed.
#' 
#' \deqn{E = t(alpha, n-1) * s / \sqrt{n}}
#' 
#' @section Default Interval Limits:
#' \tabular{ccc}{
#'   Study Parameter \tab Lower Limit \tab Upper Limit \cr
#'   \code{E}        \tab 0           \tab 1e8         \cr
#'   \code{n}        \tab 2           \tab 1e8         \cr
#'   \code{s}        \tab 0           \tab 1e8         \cr 
#'   \code{alpha}    \tab 0           \tab 1
#' }
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item{Accepts values of \code{E} greater than 0}
#'   \item{Remove non-positive values from \code{E} with a warning}
#'   \item{Cast an error if \code{E} has no positive values.}
#'   \item{Accepts integerish values of \code{n} greater than 1}
#'   \item{Remove values less than 2 from \code{n} with a warning}
#'   \item{Cast an error if \code{n} has no suitable values.}
#'   \item{Accepts values of \code{s} greater than 0}
#'   \item{Remove non-positive values from \code{s} with a warning}
#'   \item{Cast an error if \code{s} has no positive values.}
#'   \item{Accepts values of \code{alpha} between 0 and 1}
#'   \item{Remove values less than 0 or greater than 1 from 
#'              \code{alpha} with a warning}
#'   \item{Cast an error if \code{alpha} has no suitable values.}
#'   \item{Permit one and two tail interval calculations}
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
#' @export 

interval_t1 <- function(E=NULL, n=NULL, s=NULL, alpha=.05,
                        two_tail = TRUE,
                        interval_min = NULL,
                        interval_max = NULL,
                        cl = NULL, ncores = NULL){
  
  coll <- checkmate::makeAssertCollection()
  
  #* The names assigned to this list will be useful later 
  #* when the parameters are estimated.  They are 
  #* assigned here rather than renaming them later.
  plan_args <- list(E = E, 
                    n_est = n, 
                    s = s, 
                    alpha_calc = alpha)
  
  which_null <-
    vapply(X = plan_args,
           FUN = is.null,
           FUN.VALUE = logical(1))
  
  #*******************************************************
  #* Parameter checks
  #* 1.  only one of E, n, s, alpha may be NULL (error)
  #* 2a. for alpha, 0 < alpha < 1 (warning)
  #* 2b. Must be at least one valid value of alpha
  #* 3a. s must be a positive number
  #* 3b. Must be at least one valid value of s
  #* 4a. E must be a positive number
  #* 4b. Must be at least one valid value for E
  #* 5.  two_tail must be logical
  #*******************************************************
  
  #* 1.  only one of E, n, s, alpha may be NULL (error)
  
  if (sum(which_null) != 1)
  {
    coll$add("Exactly one of `E`, `n`, `s`, and `alpha` may be NULL")
  }

  #* 2a. for alpha, 0 < alpha < 1 (warning)
  if (any(alpha <= 0 | alpha >=1))
  {
    warning("`alpha` must be between 0 and 1, exclusive. Invalid values were removed")
    
    alpha <- alpha[alpha > 0 & alpha < 1]
    
    #* 2b. Must be at least one valid value of alpha 
    #*     (only applies when alpha is not NULL)
    checkmate::assert_numeric(x = alpha,
                              lower = 0,
                              upper = 1,
                              min.len = 1,
                              add = coll)
  }

  #* 3a. s must be a positive number
  if (any(s <= 0))
  {
    warning("`s` must be a positive number.  Non positive numbers were removed")
    s <- s[s > 0]
    
    #* 3b. Must be at least one valid value of s
    checkmate::assert_numeric(x = s,
                              lower = 0,
                              min.len = 1,
                              add = coll)
  }

  #* 4a. E must be a positive number
  if (any(E <= 0))
  {
    warning("`E` must be a positive number.  Non positive numbers were removed")
    
    E <- E[E > 0]
    
    #* 4b. Must be at least one valid value for E
    checkmate::assert_numeric(x = E,
                              lower = 0,
                              min.len = 1,
                              add = coll)
  }
  
  #* 5. two_tail must be logical
  checkmate::assert_logical(x = two_tail,
                            add = coll)
  
  #* 6. n must be greater than 1
  if (any(n < 2))
  {
    warning("`n` must be greater than or equal to 2. Smaller values were removed")
    n <- n[n < 2]
    
    checkmate::assert_integerish(x = n,
                                 lower = 2,
                                 min.len = 1,
                                 add = coll)
  }
  
  #* 7. cl inherits class cluster
  if (!is.null(cl))
  {
    checkmate::assert_class(x = cl,
                            classes = "cluster",
                            add = coll)
  }
  
  #* 8. ncores is integerish
  if (!is.null(ncores))
  {
    checkmate::assert_integerish(x = ncores,
                                 len = 1,
                                 lower = 1,
                                 upper = parallel::detectCores(),
                                 add = coll)
  }
  
  #* Print errors
  checkmate::reportAssertions(coll)
  
  if (is.null(cl))
  {
    if (!is.null(ncores))
    {
      cl <- parallel::makeCluster(ncores)
    }
  }
  
  #* Make the function for use in uniroot
  plan_fn <- function()
  {
    E - qt(alpha_calc, n_est-1, lower.tail = FALSE) * s / sqrt(n_est)
  }
  
  #* Assigning the formals in such a way that the missing (NULL) argument is first.
  formals(plan_fn) <-  c(plan_args[which_null], plan_args[!which_null])
  
  #* Create the data frame for storing the results
  .params <- 
    expand.grid(
      n_est = if (is.null(n)) NA else n,
      E = if (is.null(E)) NA else E,
      s = if (is.null(s)) NA else s,
      alpha = if (is.null(alpha)) NA else alpha,
      two_tail = two_tail
    ) %>%
    dplyr::mutate(alpha_calc = alpha / (two_tail + 1))
  
  #* Set default limits.  
  #* For minimum, lower limit is 2 for n_est, 0 otherwise
  if (is.null(interval_min))
  {
    interval_min <- 
      switch(
        names(plan_args)[which_null],
        "n_est" = 2,
        0 #default value
      )
  }
  
  #* For maximum, upper limit is 1 for alpha_calc, 1e8 otherwise
  if (is.null(interval_max))
  {
    interval_max <- 
      switch(
        names(plan_args)[which_null],
        "alpha_calc" = 1,
        1e8
      )
  }
  
  #* Estimate the missing parameter
  if (is.null(cl))
  {
    .params[[names(plan_args)[which_null]]] <-
      do.call("mapply",
              args = c(.params[names(plan_args)[!which_null]],
                       list(FUN = uniroot,
                            MoreArgs = list(f = plan_fn,
                                            interval = c(interval_min, interval_max)),
                            SIMPLIFY = FALSE))) %>%
      vapply(FUN = function(x) x[["root"]],
             FUN.VALUE = numeric(1))
  }
  else
  {
    .params[[names(plan_args)[which_null]]] <- 
      do.call(parallel::clusterMap,
              args = c(.params[names(plan_args)[!which_null]],
                       list(cl = cl,
                            fun = uniroot,
                            MoreArgs = list(f = plan_fn,
                                            interval = c(interval_min, interval_max)),
                            SIMPLIFY = FALSE))) %>%
      vapply(FUN = function(x) x[["root"]],
             FUN.VALUE = numeric(1))
    
    parallel::stopCluster(cl)
  }
  
  #* uniroot calculates alpha/2 for two_tail tests.  
  #* If alpha was the missing parameter, it needs to be transformed
  #* to the complete value of alpha
  if (is.null(alpha))
  {
    .params[["alpha"]] <- .params[["alpha_calc"]] * (.params[["two_tail"]] + 1)
  }
  
  #* If n was the missing parameter, it is the floor of n_est
  #* otherwise it is the same as n_est
  if (is.null(n))
  {
    .params[["n"]] <- floor(.params[["n_est"]])
  }
  else
  {
    .params[["n"]] <- .params[["n_est"]]
  }
  
  .params[c("n_est", "n", "E", "s", "alpha", "two_tail")]
}  

utils::globalVariables(c("alpha_calc", "n_est"))
