#' @name test_p2
#' @export test_p2
#' 
#' @title Power for a Two Sample Test of Proportions Using the Normal Approximation
#' @description The two sample test of proportions based on the normal approximation 
#'   is a method commonly taught in introductory statistics tests.
#'   
#' @param delta The difference to be observed between the proportions of two groups.
#'   This may also be interpreted as the difference under the alternative hypothesis.
#' @param n The total sample size between the two groups.  Unequal sample sizes
#'   may be accommodated using the \code{weights} argument.
#' @param sd The standard error of the difference of proportions.  This is the
#'   value represented by s* in the references.
#' @param alpha The significance level for the test.
#' @param power The power of the test.
#' @param delta0 The difference of proportions under the null hypothesis.  Typically,
#'   this is set to 0 (equal proportions), but can be altered if desired.
#' @param weights A list of weights for the two sample sizes.
#' @param two_tail Will this be a two tailed test?  A vector may be given.
#' @param p1_null,p2_null The proportions of p1 and p2 under the null hypothesis.  
#'   If both of these values are given, their difference will replace the 
#'   default value of \code{delta0}.
#' @param p1_alt,p2_alt The proportions of p1 and p2 under the alternative
#'   hypothesis.  If these are both provided and \code{delta = NULL}, the 
#'   difference of these vectors will be used as \code{delta}.
#' @param x1,x2,n1,n2 Values of the frequencies and samples sizes from 
#'   preliminary data to be used to calculate \code{sd}.  If All four of these
#'   values are given and \code{pbar=NULL}, \code{pbar} will be calculated from 
#'   the preliminary values.  The arguments \code{n1} and \code{n2} have no
#'   influence over the sample size values in the resulting data frame.  Note: 
#'   these values are assumed to be integers and are quietly coerced to
#'   integers.
#'   
#'

test_p2 <- function(delta=NULL, n=NULL, pbar=NULL, alpha=.05,
                    power=NULL, delta0=0, weights=list(c(1,1)),
                    two_tail=TRUE,
                    p1_null, p2_null,
                    p1_alt, p2_alt,
                    x1, n1, x2, n2){
  
  #*************************************************************************
  #* Parameter checks
  #* 1. If delta=NULL; and p1_alt and p2_alt are both missing, set delta_check=NA
  #* 2. If delta0=NULL; and p1_null and p2_null are both missing, set delta0_check=NA
  #* 3. If pbar=NULL; and x1, n1, x2, n2 are all missing, set pbar_check=NA
  #* 4. Exactly one of delta_check, n, pbar_check, alpha, power, and delta0_check must be NULL
  #* 5. alpha , power, p1_null, p2_null, p1_alt, p2_alt must be between 0 and 1, exclusive
  #* 6. x1, n1, x2, n2 must be positive integers
  
  Check <- ParameterCheck::newParamCheck()
  
  #* 1. If delta=NULL; and p1_alt and p2_alt are both missing, set delta_check=NA
  #*    delta_check will be used to determine if the check in #4 is valid
  if (is.null(delta) & all(c(missing(p1_alt), missing(p2_alt)))) delta_check <- NULL
  else delta_check <- NA
  
  #* 2. If delta0=NULL; and p1_null and p2_null are both missing, set delta0_check=NA
  #*    delta0_check will be used to determine if the check in #4 is valid
  if (is.null(delta0) & all(c(missing(p1_null), missing(p2_null)))) delta0_check <- NULL
  else delta0_check <- NA
  
  #* 3. If pbar=NULL; and x1, n1, x2, n2 are all missing, set delta0_check=NA
  #*    pbar_check will be used to determine if the check in #4 is valid
  if (is.null(pbar) & all(c(missing(x1), missing(n1), missing(x2), missing(n2)))) pbar_check <- NULL
  else pbar_check <- NA
  
  #* 4. Exactly one of delta_check, n, pbar_check, alpha, power, and delta0_check must be NULL
  sum_null <- sum(sapply(list(delta_check, n, pbar_check, alpha, power, delta0_check), is.null))
  Check <- ParameterCheck::addError(sum_null != 1,
                                    paste0("Exactly 1 of delta*, n, pbar*, alpha, power, or delta0* must be NULL\n",
                                           "    *(or an equivalent)"),
                                    Check)
  
  #* 5. alpha , power, p1_null, p2_null, p1_alt, p2_alt must be between 0 and 1, exclusive
  if (!is.null(alpha)){
    Check <- ParameterCheck::addWarning(any(alpha <= 0) | any(alpha >= 1),
                                        "'alpha' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                                        Check)
    alpha <- alpha[alpha > 0 & alpha < 1]
    Check <- ParameterCheck::addError(length(alpha) == 0,
                                      "No valid values were given for 'alpha'",
                                      Check)
  }

  if (!is.null(power)){
    Check <- ParameterCheck::addWarning(any(power <= 0) | any(power >= 1),
                                        "'power' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                                        Check)
    power <- power[power > 0 & power < 1]
    Check <- ParameterCheck::addError(length(power) == 0,
                                      "No valid values were given for 'power'",
                                      Check)
  }

  if (!missing(p1_null)){
    Check <- ParameterCheck::addWarning(any(p1_null <= 0) | any(p1_null >= 1),
                                        "'p1_null' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                                        Check)
    p1_null <- p1_null[p1_null > 0 & p1_null < 1]
    Check <- ParameterCheck::addError(length(p1_null) == 0,
                                      "No valid values were given for 'p1_null'",
                                      Check)
  }

  if (!missing(p1_alt)){
    Check <- ParameterCheck::addWarning(any(p1_alt <= 0) | any(p1_alt >= 1),
                                        "'p1_alt' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                                        Check)
    p1_alt <- p1_alt[p1_alt > 0 & p1_alt < 1]
    Check <- ParameterCheck::addError(length(p1_alt) == 0,
                                      "No valid values were given for 'p1_alt'",
                                      Check)
  }

  if (!missing(p2_null)){
    Check <- ParameterCheck::addWarning(any(p2_null <= 0) | any(p2_null >= 1),
                                        "'p2_null' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                                        Check)
    p2_null <- p2_null[p2_null > 0 & p2_null < 1]
    Check <- ParameterCheck::addError(length(p2_null) == 0,
                                      "No valid values were given for 'p2_null'",
                                      Check)
  }
  
  if (!missing(p2_alt)){
    Check <- ParameterCheck::addWarning(any(p2_alt <= 0) | any(p2_alt >= 1),
                                        "'p2_alt' must be between 0 and 1, exclusive.  Invalid values have been removed.",
                                        Check)
    p2_alt <- p2_alt[p2_alt > 0 & p2_alt < 1]
    Check <- ParameterCheck::addError(length(p2_alt) == 0,
                                      "No valid values were given for 'p2_alt'",
                                      Check)
  }
    
  
  #* 6. x1, n1, x2, n2 must be positive integers
  if (!missing(x1)){
    if (!is.integer(x1)) x1 <- as.integer(x1)
    Check <- ParameterCheck::addWarning(any(x1 <= 0),
                                        "'x1' must be a positive integer.",
                                        Check)
    x1 <- x1[x1 > 0]
    Check <- ParameterCheck::addError(length(x1) == 0,
                                      "No valid values were given for 'x1'",
                                      Check)
  }
  
  if (!missing(n1)){
    if (!is.integer(n1)) n1 <- as.integer(n1)
    Check <- ParameterCheck::addWarning(any(n1 <= 0),
                                        "'n1' must be a positive integer.",
                                        Check)
    n1 <- n1[n1 > 0]
    Check <- ParameterCheck::addError(length(n1) == 0,
                                      "No valid values were given for 'n1'",
                                      Check)
  }
  
  if (!missing(x2)){
    if (!is.integer(x2)) x2 <- as.integer(x2)
    Check <- ParameterCheck::addWarning(any(x2 <= 0),
                                        "'x2' must be a positive integer.",
                                        Check)
    x2 <- x2[x2 > 0]
    Check <- ParameterCheck::addError(length(x2) == 0,
                                      "No valid values were given for 'x2'",
                                      Check)
  }
  
  if (!missing(n2)){
    if (!is.integer(n2)) n2 <- as.integer(n2)
    Check <- ParameterCheck::addWarning(any(n2 <= 0),
                                        "'n2' must be a positive integer.",
                                        Check)
    n2 <- n2[n2 > 0]
    Check <- ParameterCheck::addError(length(n2) == 0,
                                      "No valid values were given for 'n2'",
                                      Check)
  }
  
  #* Pass errors and warnings
  ParameterCheck::finishParamCheck(Check)  
  
  #* Create estimates of null difference from preliminary values
  prelim_delta0 <- all(!c(missing(p1_null), missing(p2_null)))
  if (prelim_delta0){
    Delta0 <- data.frame(p1_null = p1_null,
                         p2_null = p2_null)
    Delta0$delta0 <- with(Delta0, p2_null - p1_null)
  }
  else Delta0 <- NULL
  
  #* Create estimates of alternative differences from preliminary values
  prelim_delta1 <- all(!c(missing(p1_alt), missing(p2_alt)))
  if (prelim_delta1){
    Delta1 <- data.frame(p1_alt = p1_alt,
                         p2_alt = p2_alt)
    Delta1$delta <- with(Delta1, p2_alt - p1_alt)
  }
  else Delta1 <- NULL
  
  #* Create an estimate of pbar from preliminary data
  prelim_p <- all(!c(missing(x1), missing(n1), missing(x2), missing(n2)))
  if (prelim_p){
    Prelim <- data.frame(x1 = x1, 
                         n1_prelim = n1,
                         x2 = x2,
                         n2_prelim = n2)
    Prelim$pbar = with(Prelim, (x2 + x1) / (n2 + n1))
  }
  else Prelim <- NULL
  
  #* Proportion of n that belongs to n1
  k <- sapply(weights, function(w) w[1] / sum(w))
         
  #* Base parameters
  .params <- expand.grid(delta = if (is.null(delta)) NA else delta,
                         delta0 = delta0,
                         pbar = if (is.null(pbar)) NA else pbar,
                         alpha = if (is.null(alpha)) NA else alpha,
                         power = if (is.null(power)) NA else power,
                         n1_est = NA,
                         n2_est = NA,
                         n_est = NA,
                         n1 = NA,
                         n2 = NA,
                         n = if (is.null(n)) NA else n,
                         two_tail = two_tail,
                         k = k)
  
  #* Calculate group sizes if 'n' is given
  if (!is.null(n)){
    .params <- transform(.params,
                         n1_est = n * k,
                         n2_est = n * (1-k),
                         n_est = (n*k) + (n * (1-k)),
                         n1 = ceiling(n*k),
                         n2 = ceiling(n * (1-k)),
                         n = ceiling(n*k) + ceiling(n * (1-k)))
  }
  
  #* if delta is not provided, add it from the prelim data
  if (all(is.na(.params$delta)) & !is.null(Delta1)){
    .params$delta <- NULL
    .params <- expand.grid.df(.params, Delta1)
    delta <- NA
  }
  
  #* if delta0 is not provided, add it from the prelim data
  if (!is.null(Delta0)){
    .params$delta0 <- NULL
    .params <- expand.grid.df(.params, Delta0)
    delta0 <- NA
  }
  
  #* if pbar is not provided, add it from the prelim data
  if (all(is.na(.params$pbar)) & !is.null(Prelim)){
    .params$pbar <- NULL
    .params <- expand.grid.df(.params, Prelim)
    pbar <- NA
  }
  
  #* My preferred order of variables
  #* .params is saved as an object with the variables in this order
  param_order <- c("p1_alt", "p2_alt", "delta", "p1_null", "p2_null", "delta0",
                   "x1", "n1_prelim", "x2", "n2_prelim", "pbar",
                   "alpha", "power", "k", "two_tail",
                   "n1_est", "n2_est", "n_est",
                   "n1", "n2", "n")
  .params <- .params[, param_order[param_order %in% names(.params), drop=FALSE]]
     
  #* Power Equation
  power.eqn <- function(tail){
    switch(tail+1,
           quote({pnorm(qnorm(1-alpha), mean=(delta0 - delta) / sqrt(pbar * (1-pbar) * (1/ (k / (1-k) * n2) + 1/n2)), lower.tail=FALSE)}),
           quote({pnorm(qnorm(alpha/2), mean=(delta0 - delta) / sqrt(pbar * (1-pbar) * (1/(k / (1-k) * n2) + 1/n2))) + 
                           pnorm(qnorm(1-alpha/2), mean=(delta0 - delta) / sqrt(pbar * (1-pbar) * (1/(k / (1-k) * n2) + 1/n2)), lower.tail=FALSE)}))
  }

  #* Calculate Power
  if (is.null(power)){
    for (i in 1:nrow(.params)){
      .params$power[i] <- with(.params[i, , drop=FALSE],
                               eval(power.eqn(two_tail)))
    }
  }
  
  #* Calculate delta
  if (is.null(delta)){
    for (i in 1:nrow(.params)){
      .params$delta[i] <- with(.params[i, , drop=FALSE],
                               uniroot(function(delta){ eval(power.eqn(two_tail)) - power},
                                       c(-1, 1))$root)
    }
  }
  
  #* Calculate sample size
  if (is.null(n)){
    for (i in 1:nrow(.params)){
      .params$n2_est[i] <- with(.params[i, , drop=FALSE],
                                uniroot(function(n2) eval(power.eqn(two_tail)) - power,
                                        c(2, 1e+7))$root)
    }
    .params$n1_est <- with(.params, (k / (1-k)) * n2_est)
    .params <- transform(.params,
                         n_est = n1_est + n2_est,
                         n1 = ceiling(n1_est),
                         n2 = ceiling(n2_est),
                         n = ceiling(n1_est) + ceiling(n2_est))
  }
  
  #* Calculate p-bar
  if (is.null(pbar)){
    for (i in 1:nrow(.params)){
      .params$pbar[i] <- with(.params[i, , drop=FALSE],
                               uniroot(function(pbar){ eval(power.eqn(two_tail)) - power},
                                       c(0, 1))$root)
    }
  }
  
  #* Calculate significance level
  if (is.null(alpha)){
    for (i in 1:nrow(.params)){
      .params$alpha[i] <- with(.params[i, , drop=FALSE],
                              uniroot(function(alpha){ eval(power.eqn(two_tail)) - power},
                                      c(0, 1))$root)
    }
  }
  
  #* Calculate null difference.  For the life of me, I don't know why you would
  #* want to do this, but it was easy to program.
  if (is.null(delta0)){
    for (i in 1:nrow(.params)){
      .params$delta0[i] <- with(.params[i, , drop=FALSE],
                               uniroot(function(delta0){ eval(power.eqn(two_tail)) - power},
                                       c(-1, 1))$root)
    }
  }
  
  return(.params)
  
}
