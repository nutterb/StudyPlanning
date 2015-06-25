#' @name test_t1
#' @export test_t1
#' @import ParameterCheck
#' 
#' @title One Sample T-Test Study Planning
#' @description Calculates the power, sample size, difference of means, 
#'   standard deviation, or significance level of a one sample t-test with
#'   options for one and two sided testing.
#'   
#' @param delta The difference of means to be tested. This may also be 
#'   specified using \code{mu0} and \code{mu1}, if desired.  If all three 
#'   of these arguments are specified, \code{delta} overrides the other two.
#' @param n The sample size
#' @param sd The standard deviation of the difference
#' @param alpha The significance level
#' @param power The power of the test
#' @param two.tail Logical.  Determines if the calculations assume one or two
#'   sided testing.  This may be a vector (ie, \code{c(TRUE, FALSE)})
#' @param mu0,mu1 Group means for calculating \code{delta}.  The difference
#'   is always calculated as \code{mu1 - mu0}.  These may be specified in place
#'   of \code{delta} and are used to calculate \code{delta} before verifiying
#'   that only one of the arguments in \code{NULL}.  Thus, \code{delta} and one
#'   other argument may be \code{NULL} so long as these arguments are specified.
#'   Recycling is used if they are not the same length.
#'   
#' @details Exactly one of the parameters \code{delta}, \code{n}, \code{sd},
#'   \code{alpha}, and \code{power} must be passed as \code{NULL}.  The only 
#'   exception is that \code{delta} may be passed as a second \code{NULL} when
#'   \code{mu0} and \code{mu1} are specified.
#'   
#'   The parameters are combined via \code{expand.grid}, so all combinations
#'   of the inputs are evaluated.
#'   
#' @return Returns a data frame with the columns listed below.  
#' \enumerate{
#' \item mu0 The hypothesized mean (only if \code{mu0} is not \code{NA})
#' \item mu1 The alternative mean (only if \code{mu1} is not \code{NA})
#' \item delta The difference of means
#' \item sd The standard deviation of the difference of means
#' \item alpha The significance level of the test.
#' \item power The power of the test
#' \item two.tail a logical indicating if the test assumes one or two tails
#' \item n_est The exact estimated sample size.  This will equal \code{n} 
#'   whenever \code{n} is not \code{NULL}
#' \item n The integer sample size obtained by \code{ceiling(n_est)}.
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @seealso \code{power.t.test}, \code{optimize}, \code{uniroot},\cr
#'   \code{vignette("TTestOneSample", package="StudyPlanning")}
#'   
#' @references
#' Hogg RV, McKean JW, Craig AT, \emph{Introduction to Mathematical Statistics},
#' Pearson Prentice Hall 6th ed., 2005. ISBN: 0-13-008507-3
#' 
#' @examples
#' test_t1(delta=1, n=20, sd=1, two.tail=TRUE)
#' 
#' #* Compare to one-sample version of example from the \code{stats} package
#' power.t.test(n = 20, delta = 1, type="one.sample")
#'
#' 
#' 
#' #* Illustrate the multiple inputs
#' test_t1(delta=1, sd=1, power=0.90, two.tail=c(TRUE, FALSE))
#' 
#' #* Compare with the examples from stats package 
#' power.t.test(power = .90, delta = 1, type="one.sample")
#' power.t.test(power = .90, delta = 1, type="one.sample", alternative = "one.sided")
#' 

test_t1 <- function(delta = NULL, n=NULL, sd=NULL,
                    alpha = 0.05, power=NULL,
                    two.tail=TRUE,
                    mu0=NA, mu1=NA){
  #******************************************
  #* Parameter Checking
  #* 1. If delta=NULL and mu0 and mu1 are given, delta = mu1-mu0
  #* 2. If delta != NULL and mu0 and mu1 are given, ignore mu0 and mu1
  #* 3. Exactly one of delta, n, sd, alpha, and power may be NULL
  #* 4. alpha must be between 0 and 1, exclusive
  #* 5. power must be between 0 and 1, exclusive
  #* 6. sd must be positive
  #* 7. n must greater than 1
  #******************************************
  
  Check <- ParameterCheck::newParamCheck()
  
  #* 1. If delta=NULL and mu0 and mu1 are given, delta = mu1-mu0
  if (is.null(delta) & (!is.na(mu0) & !is.na(mu1))){
    Check <- addWarning(length(mu0) != length(mu1),
                        "mu0 and mu1 have different lengths.  Recycling will occur",
                        Check)
    delta <- mu1 - mu0
  }
  
  #* 2. If delta != NULL and mu0 and mu1 are given, ignore mu0 and mu1
  Check <- addWarning(!is.null(delta) & (!is.na(mu0) & !is.na(mu1)),
                      "'delta' was explicitly provided; 'mu0' and 'mu1' are ignored",
                      Check)
  
  #* 3. Exactly one of delta, n, sd, alpha, and power may be NULL
  sum_null <- sum(sapply(list(delta, n, sd, alpha, power), is.null))
  Check <- addError(sum_null != 1,
                    "Exactly 1 of delta (or mu1 - mu0), n, sd, alpha, or power must be NULL",
                    Check)
  
  #* 4. alpha must be between 0 and 1, exclusive
  if (!is.null(alpha)){
    Check <- addWarning(any(alpha <= 0) | any(alpha >= 1),
                        "'alpha' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                        Check)
    alpha <- alpha[alpha > 0 & alpha < 1]
    Check <- addError(length(alpha) == 0,
                      "No valid values were given for 'alpha'",
                      Check)
  }
  
  #* 5. power must be between 0 and 1, exclusive
  if (!is.null(power)){
    Check <- addWarning(any(power <= 0) | any(power >= 1),
                        "'power' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                        Check)
    power <- power[power > 0 & power < 1]
    Check <- addError(length(power) == 0,
                      "No valid values were given for 'power'",
                      Check)
  }
  
  #* 6. sd must be positive
  if (!is.null(sd)){
    Check <- addWarning(any(sd <= 0),
                        "'sd' must be positive.  Invalid values have been removed.",
                        Check)
    sd <- sd[sd > 0]
    Check <- addError(length(sd) == 0,
                      "No valid values were given for 'sd'",
                      Check)
  }
  
  #* 7. n must greater than 1
  if (!is.null(n)){
    Check <- addWarning(any(n <= 0),
                        "'n' must be larger than 1.  Invalid values have been removed.",
                        Check)
    n <- n[n > 1]
    Check <- addError(length(n) == 0,
                      "No valid values were given for 'n'",
                      Check)
  }

  ParameterCheck::finishParamCheck(Check)
  
  #******************************************
  #* Output DataFrame
  
  .params <- expand.grid(mu0 = mu0,
                         mu1 = mu1,
                         delta = if(is.null(delta)) NA else delta,
                         sd = if (is.null(sd)) NA else sd,
                         alpha = if (is.null(alpha)) NA else alpha,
                         power = if (is.null(power)) NA else power,
                         two.tail = two.tail,
                         n_est = NA,
                         n = if (is.null(n)) NA else n)
  
  if (!is.null(n)) .params$n_est <- .params$n
  if (is.na(mu0)) .params$mu0 <- NULL
  if (is.na(mu1)) .params$mu1 <- NULL
  
  #* Calculation of Power for one and two sided tests
  p.calc1 <- quote({pt(qt(alpha, n-1, lower.tail=FALSE), 
                       n-1, ncp=sqrt(n) * delta / sd, lower.tail=FALSE)})
  p.calc2 <- quote({pt(-qt(alpha, n-1, lower.tail=FALSE),
                       n-1, ncp=sqrt(n) * delta / sd, lower.tail=TRUE) + 
                    pt(qt(alpha/2, n-1, lower.tail=FALSE),
                       n-1, ncp=sqrt(n) * delta / sd, lower.tail=FALSE)})
  
  #* Determine delta
  if (is.null(delta)){
    for (i in 1:nrow(.params)){
      .params$delta[i] <- 
        with(.params[i, ],
             if (two.tail) uniroot(function(delta) eval(p.calc2) - power, 
                                   c(-1e+7, 1e+7))$root
             else uniroot(function(delta) eval(p.calc1) - power,
                          c(-1e+7, 1e+7))$root)
    }
  }
  
  #* Determine n
  if (is.null(n)){
    for (i in 1:nrow(.params)){
      .params$n_est[i] <- 
        with(.params[i, ],
             if (two.tail) uniroot(function(n) eval(p.calc2) - power, 
                                   c(2, 1e+7))$root
             else uniroot(function(n) eval(p.calc1) - power,
                          c(2, 1e+7))$root)
    }
    .params$n <- ceiling(.params$n_est)
  }
  
  #* Determine sd
  if (is.null(sd)){
    for (i in 1:nrow(.params)){
      .params$sd[i] <- 
        with(.params[i, ],
             if (two.tail) uniroot(function(sd) eval(p.calc2) - power, 
                                   c(1e-7, 1e+7))$root
             else uniroot(function(sd) eval(p.calc1) - power,
                          c(1e-7, 1e+7))$root)
    }
  }
  
  #* Determine alpha
  if (is.null(alpha)){
    for (i in 1:nrow(.params)){
      .params$alpha[i] <- 
        with(.params[i, ],
             if (two.tail) optimize(function(alpha) (eval(p.calc2) - power)^2, 
                                   c(1e-7, 1-1e-7))$minimum
             else optimize(function(alpha) (eval(p.calc1) - power)^2,
                          c(1e-7, 1-1e-7))$minimum)
    }
  }
  
  #* Determine power
  if (is.null(power)){
    for (i in 1:nrow(.params)){
      .params$power[i] <- 
        with(.params[i, ],
             if (two.tail) eval(p.calc2)
             else eval(p.calc1))
    }
  }
                          
                         
                         
  return(.params)
  
}
