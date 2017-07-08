#' @name remove_limit
#' @title Remove Limits From a Vector
#' 
#' @description Some vectors in \code{StudyPlanning} permit the user to 
#'   include limits that are not computationally sound in the arguments.
#'   The most common examples are \code{alpha} and \code{power} arguments, 
#'   which accept values on the interval [0, 1], but the endpoints of the 
#'   intervals will fail in computations.  This function removes the endpoints
#'   if they are found in the argument.
#' 
#' @param x Numeric vector
#' @param lower \code{numeric(1)} Lower endpoint of the acceptable interval
#' @param upper \code{numeric(1)} Upper endpoint of the acceptable interval
#' @param coll An \code{AssertCollection} object. If \code{NULL}, a
#'   new object is created and used in this function.
#' @param error_none_left \code{logical(1)}. When \code{TRUE}, an error is 
#'   cast if no values remained after removing the endpoints.
#' @param assert_between \code{logical(1)}. When \code{TRUE}, an assertion 
#'   is conducted to require all remaning values in \code{x} to be in the
#'   interval (\code{lower}, \code{upper})
#' @param .var.name \code{character(1)}, an optional character vector giving 
#'   the name of the variable being checked.
#'
#' @details If the endpoint of the interval is found in \code{x}, the endpoint
#'   is removed from \code{x} and the remaining values are returned. An 
#'   error is cast if there are no remaining values after endpoint removal.
#'   
#' @return Returns a numeric vector.
#' 
#' @author Benjamin Nutter
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return a vector of any elements that are not equal to \code{lower}
#'    or \code{upper}
#'  \item Cast a message if any elements of \code{x} are removed.
#'  \item If \code{error_none_left = TRUE}, cast an error if there are no 
#'    values in \code{x} after the endpoints are removed.
#'  \item If \code{error_none_left = FALSE}, return an empty vector if there
#'    are no values in \code{x} after the endpoints are removed.
#'  \item If \code{assert_between = TRUE}, cast an error if any values in 
#'    \code{x} are less than \code{lower} or greater than \code{upper}.
#'  \item If \code{assert_between = FALSE}, return \code{x} after the 
#'    endpoints are removed.
#'  \item Cast an error if \code{x} is not numeric.
#'  \item Cast an error if \code{lower} is not \code{numeric(1)}
#'  \item Cast an error if \code{upper} is not \code{numeric(1)}
#'  \item Cast an error if \code{coll} is not an \code{AssertCollection}
#'  \item Cast an error if \code{error_none_left} is not \code{logical(1)}
#'  \item Cast an error if \code{assert_between} is not \code{logical(1)}
#'  \item Cast an error if \code{.var.name} is not \code{character(1)}
#' }
#'

remove_limit <- function(x, lower = 0, upper = 1,
                         coll = NULL, error_none_left = TRUE,
                         assert_between = TRUE,
                         .var.name = "x")
{
  if (!is.null(coll))
  {
    checkmate::assert_class(x = coll,
                            classes = "AssertCollection")
  }
  else  
  {
    coll <- checkmate::makeAssertCollection()
  }
  
  checkmate::assert_numeric(x = x,
                            add = coll)
  
  checkmate::assert_numeric(x = lower,
                            len = 1,
                            add = coll)
  
  checkmate::assert_numeric(x = upper,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = error_none_left,
                            len= 1,
                            add = coll)
  
  checkmate::assert_logical(x = assert_between,
                            len = 1,
                            add = coll)
  
  checkmate::assert_character(x = .var.name,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  on_endpoint <- x == lower | x == upper
  
  if (any(on_endpoint))
  {
    message(
      sprintf(
        paste0("`%s` only accepts values on the interval (%s, %s). ",
               "Values equal to %s or %s have been removed."),
        .var.name, lower, upper, lower, upper)
    )
    x <- x[!on_endpoint]
  }

  checkmate::assert_numeric(
    x = x,
    min.len = if (error_none_left) 1 else NULL,
    lower = if (assert_between) lower else -Inf,
    upper = if (assert_between) upper else Inf,
    add = coll,
    .var.name = .var.name
  )
  
  checkmate::reportAssertions(coll)
  
  x
}
