#' @name sim_wilcoxon
#' @export sim_wilcoxon
#' 
#' @title Simulate the Power of a Wilcoxon Rank Sum Test
#' @description Generates random samples from any two specified distributions
#'   and compares the samples by a Wilcoxon rank sum test.  Power is calculated
#'   as the proportion of tests that correctly reject the null hypothesis.
#'   
#' @param n Total sample size
#' @param ... two distribution functions should be entered.  See 'Details'.
#' @param weights a list of vectors giving the proposed weights for the two
#'   groups.  Each vector will be normalized.
#' @param alpha Significance level for the test.
#' @param nsim Number of simulations to run per set of conditions
#' @param seed The value of the random number generator seed (for reproducibility)
#' @param ncores The number of cores to use in parallel processing.
#' 
#' @details Distribution functions should be entered as a valid random variable
#'   generating function, but excluding the first argument.  For example, to 
#'   sample from a Poisson distribution with a mean of 3, one would enter
#'   \code{rpois(lambda=3)}.  Multiple values may be given to an argument to 
#'   generate power under multiple conditions, such as
#'   \code{rpois(lambda=c(3, 5, 7))}.
#' 
#' @return Returns a data frame with the following fields:
#' \enumerate{
#'   \item \code{n_total} Total sample size
#'   \item \code{n1} Group 1 sample size
#'   \item \code{n2} Group 2 sample size
#'   \item \code{k} The proportion of the total sample size allotted to Group 1
#'   \item \code{alpha} Significance level
#'   \item \code{power} Estimate of simulated power
#'   \item \code{nsim} Number of simulations performed
#'   \item \code{pop1_param} Parameters for the random sampling of group 1
#'   \item \code{pop2_param} Parameters for the random sampling of group 2
#'   \item \code{pop1_dist} Random sampling function for group 1
#'   \item \code{pop2_dist} Random sampling function for group 2
#' }
#'       
#' @author Benjamin Nutter
#' @examples
#' sim_wilcoxon(n=30, 
#'              weights=list(c(1, 1), c(1, 3)),
#'              rpois(lambda=c(2.1, 3.1, 4.1)),
#'              rpois(lambda=3.53),
#'              nsim=100)
#' 
sim_wilcoxon <- function(n, 
                                  ...,
                                  weights=list(c(1, 1)),
                                  alpha=0.05,
                                  nsim = 10000, 
                                  seed = NULL,
                                  ncores = 1){
  
  if (ncores > 1) require(parallel)
  if (ncores > detectCores()) stop("You requested more cores than are available on the machine.")
  
  #* Determine the weight proportion
  k <- sapply(weights, function(x) x[1] / sum(x))
  
  #* Extract the list of distributions
  dist.list <- as.character(substitute(list(...)))[-1]
  
  #* Extract distribution names
  dist.fn <- lapply(dist.list, function(x) substr(x, 1, regexpr("[(]", x)-1))
  names(dist.fn) <- paste0("pop", 1:length(dist.fn), "_dist")
  
  #* Extract distribution arguments
  dist.args <- lapply(dist.list, 
                      function(x){
                        x <- paste0("list(", substr(x, regexpr("[(]", x) + 1, nchar(x) - 1), ")")
                        x <- eval(parse(text=x))
                        x <- expand.grid(x)
                        for (i in 1:length(x)){
                          x[i] <- paste(names(x)[i], x[, i], sep="=")
                        }
                        x <- apply(x, 1, paste, collapse=", ")
                        return(x)
                      })
  names(dist.args) <- paste0("pop", 1:length(dist.args), "_param")
  
  #* Generate the data frame for storing results
  .param <- expand.grid(c(list(n_total=n,
                               n1 = NA,
                               n2 = NA,
                               k = k,
                               alpha = alpha,
                               power = NA,
                               nsim = nsim),
                          dist.args),
                        stringsAsFactors=FALSE)
  .param$n1 <- with(.param, round(n_total * k))
  .param$n2 <- with(.param, n_total - n1)
  .param <- cbind(.param, dist.fn, stringsAsFactors=FALSE)
  
  simulate <- function(r, .param, nsim){
    x.param <- c(list(n = .param$n1[r]), 
                 eval(parse(text=paste0("list(", .param$pop1_param[r], ")"))))
    x <- lapply(1:nsim, function(x, r) do.call(.param$pop1_dist[r], x.param), r)
    
    y.param <- c(list(n = .param$n2[r]),
                 eval(parse(text=paste0("list(", .param$pop2_param[r], ")"))))
    y <- lapply(1:nsim, function(y, r) do.call(.param$pop2_dist[r], y.param), r)
    
    signif <- sapply(1:nsim, function(s, .param) wilcox.test(x[[s]], y[[s]], data=.param)$p.value, .param)
    
    sum(signif <= 0.05) / nsim
  }
  
  if (ncores <= 1){
    if (!is.null(set.seed)) set.seed(seed)
    .param$power <- sapply(1:nrow(.param), simulate, .param, nsim)
  }
  else{
    cl <- makeCluster(ncores)
    if (!is.null(seed)) clusterSetRNGStream(cl, seed)
    .param$power <- parSapply(cl, 1:nrow(.param), simulate, .param, nsim)
    stopCluster(cl)
  }
  
  return(.param)
}
