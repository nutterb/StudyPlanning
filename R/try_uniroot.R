#' @name try_uniroot
#' @title Wrapper Function for \code{uniroot} Within \code{tryCatch]}
#' 
#' @description When \code{uniroot} cannot find a solution within the given interval,
#'   it casts an error the causes the entire sample size estimation to fail.
#'   This strikes me as ungraceful, and it would be preferable to return a
#'   \code{NA} and allow calculations to succeed where possible.
#'   
#' @param ... Additional named or unnamed arguments to be passed to \code{f}
#' @param f The function for which the root is sought.
#' @param interval A vector containing the end-points of the interval to be 
#'   searched for the root.
#'   
#' @seealso 
#' \code{\link{uniroot}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return \code{NA} when \code{uniroot} casts the error 
#'     "f() values at end points not of opposite sign". Other errors cause 
#'     the function to fail. 
#' }
#' 

try_uniroot <- function(..., f, interval)
{
  tryCatch(
    uniroot(f = f,
            interval = interval,
            ...),
    error = function(cond)
    {
      if (grepl("f[(][)] values at end points not of opposite sign", cond))
      {
        list(root = NA)
      }
      else
      {
        stop(cond)
      }
    }
  )
}