#' @name interval_t1
#' @export interval_t1
#'
#' @title Sample Size for One Sample T Confidence Intervals
#'
#' @description The $t$-based confidence interval is commonly used in estimating
#'     population mean using sample data.  It is based on Student's $t$ 
#'     distribution, an adaptation of the standard normal distribution.  
#'     \code{n_t1samp_interval} provides tools for calculating and illustrating
#'     the sample size requirement and other design factors associated with 
#'     these intervals.
#' 
#' @param E Margin of error
#' @param n Sample size
#' @param s Sample standard deviation
#' @param alpha Significance level (or 1-confidence)
#' @param optim.max Maximum value for \code{optimze} when solving for \code{n}.  
#'     The default setting is 1,000,000,000, which is probably higher than
#'     anyone will ever need.  I may change this in the future for the sake
#'     of improving speed.
#' 
#' @details Exactly one of the parameters \code{n}, \code{E}, \code{s} and 
#' \code{alpha} must be passed as \code{NULL}, and that parameter will be 
#' calculated from the others.  Notice that \code{alpha} has a 
#' non-\code{NULL} default, so \code{NULL} must be explicitly passed 
#' if you want it computed.
#' 
#' @return Returns an object of class \code{HazPwr} with subclasses \code{t1samp}
#'  and \code{est} (actual appearance: \code{t1samp_est_HazPwr}.  The object
#'  contains a data frame of the parameters passed to the function
#'  (expanded using \code{expand.grid}) and the corresponding sample size 
#'  estimates.  
#'  
#'  \enumerate{
#'  \item \code{n_est}: Exact sample size.  When \code{n} is not 
#'      \code{NULL}, this will be identical to \code{n}.
#'  \item \code{n}: Estimated sample size; the next largest integer 
#'      after \code{n_est}.
#'  \item \code{E}: Margin of error
#'  \item \code{s}: Sample Standard Deviation
#'  \item \code{alpha}: Desired significance level.
#'  }
#' 
#' @author Benjamin Nutter
#' 
#' 



interval_t1 <- function(E=NULL, n=NULL, s=NULL, alpha=.05,
                        two_tail = TRUE,
                        interval_min = NULL,
                        interval_max = NULL){
  
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
    if (!length(alpha))
    {
      coll$add("No valid values for `alpha` were given")
    }
  }

  #* 3a. s must be a positive number
  if (any(s <= 0))
  {
    warning("`s` must be a positive number.  Non positive numbers were removed")
    s <- s[s > 0]
    
    #* 3b. Must be at least one valid value of s
    if (!length(s))
    {
      coll$add("No valid values for `s` were given")
    }
  }

  #* 4a. E must be a positive number
  if (any(E <= 0))
  {
    warning("`E` must be a positive number.  Non positive numbers were removed")
    
    E <- E[E > 0]
    
    #* 4b. Must be at least one valid value for E
    if (length(E) == 0)
    {
      coll$add("No valid values for E were given.")
    }
  }
  
  #* 5. two_tail must be logical
  checkmate::assert_logical(x = two_tail,
                            add = coll)
  
  #* Print errors
  checkmate::reportAssertions(coll)
  
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
  .params[[names(plan_args)[which_null]]] <- 
    mapply(
      uniroot,
      .params[[names(plan_args)[!which_null][1]]],
      .params[[names(plan_args)[!which_null][2]]],
      .params[[names(plan_args)[!which_null][3]]],
      MoreArgs = list(f = plan_fn,
                      interval = c(interval_min, interval_max)),
      SIMPLIFY = FALSE
    ) %>%
    vapply(FUN = function(x) x[["root"]],
           FUN.VALUE = numeric(1))
  
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

interval_t1(n = c(800, 900),
            E = c(.02616808, .03), 
            alpha = NULL, # alpha = 0.05, 
            s = c(0.4, 0.5, 0.6))
