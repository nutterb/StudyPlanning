#' @name interval_t2
#' @title Estimate Study Parameters for a Two Sample T-Interval
#' 
#' @description Estimate a missing study parameter for a two sample t-based 
#'     confidence interval for the difference of population means, 
#'     given the remaining study parameters are known. 
#'     The study parameters for the two sample 
#'     t-based interval are \code{E} (margin of error), \code{n1} 
#'     (sample size of group 1), \code{n2} (sample size of group2), 
#'     \code{s} (standard deviation) and \code{alpha} (significance level). 
#'     
#'     The t-based confidence interval is commonly used in estimating
#'     population mean using sample data.  It is based on Student's t 
#'     distribution, an adaptation of the standard normal distribution for
#'     cases when the population standard deviation is not known.