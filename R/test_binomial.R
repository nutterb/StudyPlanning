#' @name test_binomial
#' @export test_binomial
#' @import ParameterCheck
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
#' @author Benjamin Nutter
#' 
#' @references
#' O'Brien R, Castelloe J, "Sample-Size Analysis in Study Planning,"
#'   American Statistical Association Continuing Education Program: Section on
#'   Teaching Statistics in the Health Sciences, Joint Statistical Meetings, 
#'   San Francisco, CA; 5 August 2003 (Short Course Manual)
#' 
#' Some design choices were obtained from the r-help question at:
#' \url{http://r.789695.n4.nabble.com/Sample-size-calculations-for-one-sided-binomial-exact-test-td3964313.html}
#' 
#' @examples
#' #* Julia Chill's Frozen Sensations Example from O'Brien and Castelloe
#' test_binomial(n=c(20, 40), p0=.5, p1=.8, alpha=c(.01, .05), 
#'               alternative='right.tailed')
#'               
#' #* Plot the sample size for a range of n
#' library(ggplot2)
#' Chill <- test_binomial(n=20:40, p0=.5, p1=.8, alpha=c(.01, .05), 
#'               alternative='right.tailed')
#'               
#' ggplot(Chill, aes(x=n, y=power, colour=factor(alpha))) + geom_line()
#' 

test_binomial <- function(n = NULL, p0 = NULL, p1 = NULL, 
                            power = NULL, alpha=.05,
                            alternative = "two.tailed",
                            conservative = FALSE,
                            n_limits=c(2L, 200L)){
    
  #******************************************
  #* Parameter Checking
  #* 1. Exactly one of n, p0, p1, power, and alpha may be NULL
  #* 2. alpha must be between 0 and 1, exclusive
  #* 3. power must be between 0 and 1, exclusive
  #* 4. p0 must be between 0 and 1, exclusive
  #* 5. p1 must be between 0 and 1, exclusive
  #* 6. n must greater than 1
  #* 7. alternative must be in c('two.sided', 'left.tailed', 'right.tailed')
  #* 8. coerce n_limits to integer
  #* 9. lower limit of n_limits must be greater than 1
  #******************************************
    
  Check <- ParameterCheck::newParamCheck()

  #* 1. Exactly one of n, p0, p1, power, and alpha may be NULL
  sum_null <- sum(sapply(list(n, p0, p1, power, alpha), is.null))
  Check <- ParameterCheck::addError(sum_null != 1,
                    "Exactly 1 of n, p0, p1, alpha, or power must be NULL",
                    Check)
  
  #* 2. alpha must be between 0 and 1, exclusive
  if (!is.null(alpha)){
    Check <- ParameterCheck::addWarning(any(alpha <= 0) | any(alpha >= 1),
                        "'alpha' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                        Check)
    alpha <- alpha[alpha > 0 & alpha < 1]
    Check <- ParameterCheck::addError(length(alpha) == 0,
                      "No valid values were given for 'alpha'",
                      Check)
  }
  
  #* 3. power must be between 0 and 1, exclusive
  if (!is.null(power)){
    Check <- ParameterCheck::addWarning(any(power <= 0) | any(power >= 1),
                        "'power' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                        Check)
    power <- power[power > 0 & power < 1]
    Check <- ParameterCheck::addError(length(power) == 0,
                      "No valid values were given for 'power'",
                      Check)
  }
  
  #* 4. p0 must be between 0 and 1, exclusive
  if (!is.null(p0)){
    Check <- ParameterCheck::addWarning(any(p0 <= 0) | any(p0 >= 1),
                        "'p0' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                        Check)
    p0 <- p0[p0 > 0 & p0 < 1]
    Check <- ParameterCheck::addError(length(p0) == 0,
                      "No valid values were given for 'p0'",
                      Check)
  }
  
  #* 5. p1 must be between 0 and 1, exclusive
  if (!is.null(p1)){
    Check <- ParameterCheck::addWarning(any(p1 <= 0) | any(p1 >= 1),
                        "'p1' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                        Check)
    p1 <- p1[p1 > 0 & p1 < 1]
    Check <- ParameterCheck::addError(length(p1) == 0,
                      "No valid values were given for 'p1'",
                      Check)
  }
  
  #* 6. n must greater than 1
  if (!is.null(n)){
    Check <- ParameterCheck::addWarning(any(n <= 0),
                        "'n' must be larger than 1.  Invalid values have been removed.",
                        Check)
    n <- n[n > 1]
    Check <- ParameterCheck::addError(length(n) == 0,
                      "No valid values were given for 'n'",
                      Check)
  }
  
  #* 7. alternative must be in c('two.tailed', 'left.tailed', 'right.tailed')
  
  Check <- ParameterCheck::addError(!all(alternative %in% c("two.tailed", "left.tailed", 
                                            "right.tailed")),
                    paste0("Values in 'alternative' must be ",
                           "'two.tailed', 'left.tailed', or 'right.tailed'"),
                    Check)
  
  #* 8. coerce n_limits to integer
  Check <- ParameterCheck::addWarning(!is.integer(n_limits),
                      "n_limits has been coerced to an integer vector",
                      Check)
  
  #* 9. lower limit of n_limits must be greater than 1
  n_limits <- sort(n_limits)
  Check <- ParameterCheck::addWarning(n_limits[1] <= 1,
                      "The minimum for n_limits has been increased to 2",
                      Check)
  if (n_limits[1] <= 1) n_limits[1] <- 2
  n_limits <- n_limits[1]:n_limits[2]
  
  #* Pass errors and warnings
  ParameterCheck::finishParamCheck(Check)

  
  #*****************************************
  #* Output Data Frame
  .params <- expand.grid(p0 = if (is.null(p0)) NA else p0,
                         p1 = if (is.null(p1)) NA else p1,
                         alpha = if (is.null(alpha)) NA else alpha,
                         alpha_actual = NA,
                         power = if (is.null(power)) NA else power,
                         power_actual = NA,
                         n = if (is.null(n)) NA else n,
                         alternative = alternative,
                         conservative = conservative,
                         stringsAsFactors=FALSE)
  
  #* Power Equation Selection Function
  power.eqn <- function(alt){
    switch(alt,
           "two.tailed" = quote({pbinom(qbinom(alpha/2, n, p0), n, p1) +
                                  pbinom(qbinom(alpha/2, n, p0, 
                                                lower.tail=FALSE), 
                                         n, p1, lower.tail=FALSE)}),
           "left.tailed" = quote({pbinom(qbinom(alpha, n, p0), n, p1)}),
           "right.tailed" = quote({pbinom(qbinom(alpha, n, p0,
                                                 lower.tail=FALSE), 
                                          n, p1, lower.tail=FALSE)}))
  }
  
  sig.eqn <- function(alt){
    switch(alt,
           "two.tailed" = quote({pbinom(qbinom(alpha/2, n, p0) - 1, n, p0) + 
                                 pbinom(qbinom(alpha/2, n, p0, lower.tail=FALSE) + 1,
                                        n, p0, lower.tail=FALSE)}),
           "left.tailed" = quote({pbinom(qbinom(alpha, n, p0) - 1, n, p0)}),
           "right.tailed" = quote({pbinom(qbinom(alpha, n, p0, lower.tail=FALSE) + 1,
                                          n, p0, lower.tail=FALSE)}))
  }
        
  
  #* Calculate Power
  if (is.null(power)){
    for(i in 1:nrow(.params)){
      .params$power[i] <- with(.params[i, ], eval(power.eqn(alternative)))
      .params$alpha_actual[i] <- with(.params[i, ], eval(sig.eqn(alternative)))
    }
    .params$power_actual <- .params$power
    
  }
  
  #* Calculate Sample Size
  #* Sample Size is calculated by calculating the difference between
  #* the stated power and the power calculate for each value in n_limits.
  #* The value of n that gives the smallest positive difference is the
  #* chosen sample size.
  if (is.null(n)){
    for(i in 1:nrow(.params)){
      .params$n[i] <- 
        with(.params[i, ],
             {eqn <- function(n){eval(power.eqn(alternative)) - power}
              if (conservative) n_limits[max(which(eqn(n_limits) <= 0)) + 1]
              else n_limits[min(which(eqn(n_limits) >= 0))]
             })
      .params$alpha_actual[i] <- with(.params[i, ], eval(sig.eqn(alternative)))
      .params$power_actual[i] <- with(.params[i, ], eval(power.eqn(alternative)))
      #* This block is in place to help me observe fluctuations in power
      #* based on sample size.  Particularly when increasing sample size 
      #* decreases power
      #* with(.params[i, ],
      #*      {eqn <- function(n){eval(power.eqn(alternative)) - power}
      #*       pwr <- function(n){eval(power.eqn(alternative))}
      #*             data.frame(n=n_limits,
      #*                        power = pwr(n_limits),
      #*                        diff = eqn(n_limits))
      #*      })
    }
  }
  
  #* Solve for p0
  if (is.null(p0)){
    for(i in 1:nrow(.params)){
      .params$p0[i] <- 
        with(.params[i, ],
             uniroot(function(p0) eval(power.eqn(alternative)) - power,
                     c(0, 1))$root)
      .params$alpha_actual[i] <- with(.params[i, ], eval(sig.eqn(alternative)))
      .params$power_actual[i] <- with(.params[i, ], eval(power.eqn(alternative)))
    }
  }
  
  #* Solve for p1
  if (is.null(p1)){
    for(i in 1:nrow(.params)){
      .params$p1[i] <- 
        with(.params[i, ],
             uniroot(function(p1) eval(power.eqn(alternative)) - power,
                     c(0, 1))$root)
      .params$alpha_actual[i] <- with(.params[i, ], eval(sig.eqn(alternative)))
      .params$power_actual[i] <- with(.params[i, ], eval(power.eqn(alternative)))
    }
  }
  
  #* Solve for significance
  if (is.null(alpha)){
    for(i in 1:nrow(.params)){
      .params$alpha[i] <- 
        with(.params[i, ],
             uniroot(function(alpha) eval(power.eqn(alternative)) - power,
                     c(0, 1))$root)
      .params$power_actual[i] <- with(.params[i, ], eval(power.eqn(alternative)))
    }
    .params$alpha_actual <- .params$alpha
  }
  
  return(.params)
}
  
                            