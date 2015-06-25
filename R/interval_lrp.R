#' @name interval_lrp
#' @export interval_lrp
#'
#' @title Sample Size for Likelihood Ratio Positive Confidence Intervals
#'
#' @description The likelihood ratio positive \eqn{LR^+} is a measure comparing positive results from a 
#' diagnostic test.  Sample size requirements may be generated given an estimate of the sensitivity, 
#' specificity, and desired lower (or upper) limit of the confidence interval.  Unbalanced designs between 
#' diseased and non-diseased groups may be accommodated.
#' 
#' @param lr The lower (or upper) limit of the likelihood ratio positive desired for the study.
#' @param n The total sample size for the study.  Refer to the Details for strategies
#'     for calculating \eqn{n} from the disease or non-disease group size.
#' @param sens The estimated or expected sensitivity of the diagnostic test.  
#' @param spec The estimated or expected specificity of the diagnostic test.
#' @param weights A list of vectors, each of length 2, providing the weights for 
#'     the diease and non-disease groups.  By default, it is 
#'     assumed that there are as many patients with the disease as there are without.  
#'     The values in each vector will be normalized (summed and divided by one), 
#'     so they do not necessarily need to sum to 1.0.  Note that while 
#'     decreasing the weight of the disease group may reduce the sample size requirement, 
#'     it will increase the width of the confidence interval around the likelihood ratio negative.
#' @param alpha Significance level (or 1-confidence)
#' 
#' @details Exactly one of the parameters \code{lr}, \code{n}, and \code{alpha} must
#' be passed as \code{NULL}, and that parameter will be calculated from the others.
#' Notice that \code{alpha} has a non-\code{NULL} default, so \code{NULL} must be 
#' explicitly passed if you want it computed.
#' 
#' Let \eqn{D} indicate a patient's disease status, with \eqn{D^+} indicating the patient has the 
#' disease and \eqn{D^-} indicating no disease.  Let \eqn{T} represent a diagnostic test where \eqn{T^+} 
#' indicates a positive test and \eqn{T^-} indicates a negative test.  Then the sensitivity, \eqn{s_e}, 
#' is defined by \eqn{s_e = P(D^+|T^+)} and specificity, \eqn{s_p} is defined by \eqn{P(D^-|T^-)}.  
#' The likelihood ratio positive can be defined as \eqn{s_e / (1-s_p)}.  
#' But who are we kidding, this is nearly impossible to read in this medium.  
#' Further details about the definitions and derivations for this function 
#' can be reviewed by running \code{vignette('LikelihoodRatioPositive')}.
#' 
#' In cases where the total sample size is not known, the total sample size may 
#' be calculated using either \eqn{n = n_h + (r/(1-r)) * n_h} or 
#' \eqn{n_d + (1-r)/r * n_d}, where \eqn{n_h} is the size of the 
#' non-disease group, \eqn{n_d} is the size of the disease group, 
#' and \eqn{r} is the disease rate in the total group.  The vignette given above
#' justifies this approach in Section 5b.  An example is given in Section 4e.
#' 
#' @return Returns an object of class \code{HazPwr} with subclasses \code{lrp}
#'  and \code{est} (actual appearance: \code{lrp_est_HazPwr}.  The object
#'  contains a data frame of the parameters passed to the function
#'  (expanded using \code{expand.grid}) and the corresponding sample size 
#'  estimates.  Sample size estimates are provided for the diseased group, the
#'  non-diseased group, and the total sample size.  Specific fields are:
#'  \enumerate{
#'  \item \code{lr}: Lower limit of the confidence interval for the likelihood
#'    ratio positive.
#'  \item \code{sens}: Sensitivity of the proposed test.
#'  \item \code{spec}: Specificity of the proposed test.
#'  \item \code{disease.rate}: Rate of disease in the study population.
#'  \item \code{alpha}: Desired significance level.
#'  \item \code{n1_est}: The estimated (decimal) sample size for the diseased group.
#'  \item \code{n2_est}: The estimated (decimal) sample size for the 
#'    non-diseased group.
#'  \item \code{n_est}: The total estimated (decimal) sample size.  This is 
#'    the sum of \code{n1_est} and \code{n2_est}.
#'  \item \code{n1}: The actual (integer) sample size for the diseased group.
#'  \item \code{n2}: The acutal (integer) sample size for the non-diseased
#'    group:
#'  \item \code{n}: The actual total (integer) sample size.  This is the sum
#'    of \code{n1} and \code{n2}.
#'  }
#' 
#' @author Benjamin Nutter
#' 
#' @references David L. Simel, Gregory P. Samsa, and David B. Matchar, ``Likelihood Ratios with confidence: sample size estimation
#' for diagnostic test studies," \emph{Journal of Clinical Epidemiology}, Vol 44, No 8, pp 763-770, 1991.
#' 
#' @examples
#' 
#' # Problem 1 From Simel Article
#' interval_lrp(lr=2.0, n=NULL, sens=.80, spec=.73)
#' 
#' # Problem 3 From Simel Article
#' interval_lrp(lr=2.0, n=NULL, sens=.80, spec=.73, weights=list(c(1, 5)))
#' 

interval_lrp <- function(lr=NULL, n=NULL, sens, spec, 
                           weights = list(c(.5, .5)), alpha=.05){
  err.flag <- 0
  err.msg <- ""
  
  warn.flag <- 0
  warn.msg <- ""
  
  #***********************************************************
  #* Parameter checks
  #* 1. Remove values of sens that are <= 0 or >= 1 (warning)
  #* 2. Remove values of spec that are <= 0 or >= 1 (warning)
  #* 3. Actually remove the values
  #* 4. Confirm at least one valid value of sens
  #* 5. Confirm at least one valid value of spec
  #* 6. Each vectors in 'weights' must have length 2.
  #* 7. alpha must be between 0 and 1
  #* 8. Exactly one of lr, n, and alpha is NULL
  #***********************************************************
  
  #* 1. Remove values of sens that are <= 0 or >= 1 (warning)
  if (any(sens <= 0) | any(sens >= 1)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg, 
                  paste("Values where sens <= 0 and where sens >= 1 have been removed."))
  }
  
  #* 2. Remove values of spec that are <= 0 or >= 1 (warning)
  if (any(spec <= 0) | any(sens >= 1)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste("Values where sens <= 0 and where sens >= 1 have been removed."))
  }
  
  #* 3. Actually remove the values
  #* Remove values of sens and spec that are less than 0 or greater than 1
  #* These would cause the function to fail, and since
  #* I can see myself doing something like sens=seq(0, 1, by=.1)
  #* I may as well do this for my own protection.
  sens <- sens[sens > 0 & sens < 1]
  spec <- spec[spec > 0 & spec < 1]
  
  #* 4. Confirm at least one valid value of sens
  if (length(sens) == 0){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, 
                 paste(err.flag, ": No valid values were given for sens; 0 < sens < 1"))
  }
  
  #* 5. Confirm at least one valid value of spec
  if (length(spec) == 0){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, 
                 paste(err.flag, ": No valid values were given for spec; 0 < spec < 1"))
  }
  
  #* 6. Each vectors in 'weights' must have length 2.
  if (any(sapply(weights, length) != 2)){
    w <- which(sapply(weights, length) != 2)
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, paste(err.flag, ": elements",
                                paste(w, collapse=","), "in 'weights' do not have length 2"))
  }
  
  #* 7. alpha must be between 0 and 1
  if (any(alpha <= 0) | any(alpha >= 1)) {
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, paste(err.flag, ": alpha must be a proportion, ie 0 < sens < 1"))
  }
  
  #* 8. Exactly one of lr, n, and alpha is NULL
  if (sum(sapply(list(lr, n, alpha), is.null)) != 1){
    err.flag <- err.flag + 1
    err.msg <- c(err.msg, paste(err.flag, ": exactly one of 'lr', 'n', and 'alpha' must be NULL"))
  }
  
  if (err.flag) stop(paste(err.msg, collapse="\n"))
  if (warn.flag) warning(paste(warn.msg, collapse="\n"))

  #* Calculate sample sizes
  if (is.null(n)){
    .param <- expand.grid(lr = lr,
                          sens = sens,
                          spec = spec,
                          disease.rate = sapply(weights, function(x) x[1]/sum(x)),
                          alpha=alpha)
    .param$n <- .param$nh <- .param$nd <-
      .param$n_est <- .param$nh_est <- .param$nd_est <- NA
  
    .param$nh_est <- with(.param, (qnorm(1-alpha/2)^2 * 
                                     ((1-sens)/(sens*(disease.rate/(1-disease.rate))) + 
                                        spec/(1-spec))) /
                            (log(sens/(1-spec)) - log(lr))^2)
  
    .param$nd_est <- .param$disease.rate/(1-.param$disease.rate) * .param$nh_est
    .param$n_est <- .param$nd_est + .param$nh_est
  
    .param$nd <- ceiling(.param$nd_est)
    .param$nh <- ceiling(.param$nh_est)
    .param$n <- .param$nd + .param$nh

    return(.param)
  }
  
  if (is.null(lr)){
    .param <- expand.grid(lr = NA,
                          sens = sens,
                          spec = spec, 
                          disease.rate = sapply(weights, function(x) x[1]/sum(x)),
                          alpha = alpha,
                          n = n)
    .param$nd_est <- .param$n * .param$disease.rate
    .param$nh_est <- .param$n * (1-.param$disease.rate)
    .param$n_est <- .param$nd_est + .param$nh_est
    .param$nd <- ceiling(.param$nd_est)
    .param$nh <- ceiling(.param$nh_est)
    .param$lr <- with(.param,
                      exp(log(sens/(1-spec)) - qnorm(1-alpha/2) * 
                            sqrt((1-sens)/(sens*nd) + spec/((1-spec) * nh))))
    .param <- .param[, c("lr", "sens", "spec", "disease.rate", "alpha",
                         "nd_est", "nh_est", "n_est", 
                         "nd", "nh", "n")]
    return(.param)
  }
  
  if (is.null(alpha)){
    .param <- expand.grid(lr = lr,
                          sens = sens,
                          spec = spec, 
                          disease.rate = sapply(weights, function(x) x[1]/sum(x)),
                          alpha = NA,
                          n = n)
    .param$nd_est <- .param$n * .param$disease.rate
    .param$nh_est <- .param$n * (1-.param$disease.rate)
    .param$n_est <- .param$nd_est + .param$nh_est
    .param$nd <- ceiling(.param$nd_est)
    .param$nh <- ceiling(.param$nh_est)
    .param$alpha <- with(.param,
                      2 * pnorm((log(lr) - log(sens/(1-spec))) / 
                                sqrt((1-sens)/(sens * nd_est) + 
                                     (spec / ((1-spec) * nh_est)))))
    .param <- .param[, c("lr", "sens", "spec", "disease.rate", "alpha",
                         "nd_est", "nh_est", "n_est", 
                         "nd", "nh", "n")]
    return(.param)
  }
} 

