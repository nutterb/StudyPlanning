#' @name uniroot.integer
#' @title Find the root of a function to the nearest integer
#' 
#' @description Let \code{f} be a monotonic function that changes sign within 
#' the interval specified. If \code{f(i) = 0} for some \code{i} within the 
#' interval specified (including the ends of the interval), 
#' then the root is \code{i}. Otherwise if \code{pos.side = TRUE} 
#' (or \code{FALSE}) then \code{uniroot.integer} finds the integer \code{i} 
#' such that \code{f(i)} is closest to the sign change and is positive 
#' (or negative).
#' 
#' This function is borrowed from the \code{ssanv} package by Michael P. Fay.
#' It is copied to avoid adding unnecessary dependencies.
#' 
#' @param f function for which a root is needed
#' @param interval an interval giving minimum and maximum allowable values for root
#' @param lower	minimum allowable root
#' @param upper	maximum allowable root
#' @param step.power initial step size is \code{2 ^ step.power}
#' @param step.up	if \code{TRUE} steps up from \code{'lower'}, 
#'   if \code{FALSE} steps down from \code{'upper'}
#' @param pos.side if \code{TRUE} finds integer, \code{i}, 
#'   closest to the root such that \code{f(i) > zero}
#' @param print.steps	if \code{TRUE}, prints iterations
#' @param maxiter	maximum number of iterations
#' @param ...	additional arguments to \code{f}.
#' 
#' @details The algorithm evaluates \code{f(i)} iteratively, increasing 
#' (or decreasing if \code{step.up = FALSE}) \code{i} by \code{2 ^ step.power} 
#' until either \code{f(i) = 0} or \code{f(i)} switches sign. 
#' If \code{f(i)=0}, then stop. If \code{f(i)} switches sign, then the 
#' change in \code{i} is halved each iteration until convergence.
#' 
#' @return 
#' A list of the following elements:
#' \itemize{
#' \item{root } the integer on the correct side of the root
#' \item{f.root	} value of f at root
#' \item{iter } number of times f was evaluated
#' }
#' 
#' @note Unlike \code{uniroot}, the function is not automatically evaluated 
#'   at both extremes. This makes \code{uniroot.integer} an efficient method 
#'   to use when the calculation time of \code{f(i)} increases with the value 
#'   of \code{i}. For an example of the importance of this see 
#'   \code{ss.fromdata.pois} (in the \code{ssanv} package).
#'   
#' @author Michael P. Fay
#' 
#' @source
#' Fay, M.P.,Follmann, D.A., Halloran, M.E. (2007). Accounting for
#' variability in sample size estimation with applications to nonadherence
#' and estimation of variance and effect size Biometrics 63: 465--474.
#' 
#' \code{ssanv} package: \url{https://CRAN.R-project.org/package=ssanv}
#' 
#' @seealso \code{\link{uniroot}}
#' 
#' @examples 
#' root.func<-function(i) i - 500.1 
#' ## initial step sizes = 2^2 =4
#' uniroot.integer(root.func,c(0,Inf),step.power=2)
#' ## more efficient to use bigger initial step sizes = 2^10 =1024
#' uniroot.integer(root.func,c(0,Inf),step.power=10,print.steps=TRUE)

uniroot.integer <- function (f, interval, 
                             lower = min(interval), 
                             upper = max(interval), 
                             step.power = 6, step.up = TRUE,
                             pos.side = FALSE, print.steps = FALSE,
                             maxiter = 1000, ...) 
{
  ## iter counts how many times f is evaluated 
  iter <- 0
  if (!is.numeric(lower) || !is.numeric(upper) || lower >= upper) 
  {
    stop("lower < upper  is not fulfilled")
  }
  
  if (lower == -Inf && step.up == TRUE)
  {
    stop("lower cannot be -Inf when step.up=TRUE")
  }
  
  if (upper == Inf && step.up == FALSE)
  {
    stop("upper cannot be Inf when step.up=FALSE")
  }
  
  if (step.up)
  {
    f.old <- f(lower, ...)
    iter <- iter+1
    sign <- 1
    xold <- lower 
  }
  else
  {
    f.old <- f(upper, ...)
    iter <- iter + 1
    sign <- -1 
    xold <- upper
  }
  if (print.steps)
  { 
    print(paste("x=", xold, " f(x)=", f.old)) 
  }
  
  ever.switched <- FALSE
  tried.extreme <- FALSE
  
  while (step.power > -1)
  {
    # Jun-22-2015: fix problems when f(i)=0 exactly for some i
    # break out when f.old=0, since xold will be the root 
    if (f.old==0) 
    {
      break()
    }
    
    if (iter>=maxiter) 
    {
      stop("reached maxiter without a solution")
    }
    
    xnew <- xold + sign * 2 ^ step.power
    if ((step.up & xnew < upper) || (!step.up & xnew > lower) )
    {
      f.new <- f(xnew,...) 
      iter <- iter + 1
      if (print.steps)
      { 
        print(paste("x=",xnew," f(x)=",f.new)) 
      }
    }
    else
    { 
      #### Since stepped beyond extreme, move x back to xold
      xnew <- xold
      #### define f.new for the `if' statements following
      f.new <- f.old
      #### Decrease the step size if you step beyond the extreme
      step.power <- step.power-1 
      #### Only run the f(extreme) once, and test that you have opposite ends at both extremes
      if (tried.extreme==FALSE)
      {
        if (step.up)
        { 
          f.extreme <- f(upper,...)
          iter <- iter + 1
          x.extreme <- upper 
        }
        else
        { 
          f.extreme <- f(lower, ...)
          iter <- iter + 1
          x.extreme <- lower 
        }
        
        tried.extreme <- TRUE 
        xswitch <- x.extreme
        f.switch <- f.extreme
        if (print.steps)
        { 
          print(paste("x=", x.extreme, " f(x)=", f.extreme)) 
        }
        # Jun-22-2015: fix problems when f(i)=0 exactly for some i
        # break out when f=0, since then x will be the root
        if (f.extreme == 0)
        {
          # set xold to root and f.old=f(root) 
          xold <- x.extreme
          f.old <- f.extreme
          break()
        }
        if (f.old * f.extreme >= 0)
        {
          stop("f() at extremes not of opposite sign")
        }
      }
    }
    
    if (f.old * f.new < 0)
    { 
      sign <- sign * (-1)
      ever.switched <- TRUE
      xswitch <- xold
      f.switch <- f.old
    }
    if (ever.switched)
    { 
      #### Only decrease the step size if you have already either switched directions
      #### (implying that the root is before the switch)
      #### Previously, we had decreased the step size if we had stepped beyond the extreme
      step.power <- step.power - 1 
      
      if (step.power == -1)
      { 
        break()
      } 
    }
    
    xold <- xnew 
    f.old <- f.new
    
  }
  # Jun-22-2015: fix problems when f(i)=0 exactly for some i
  # for breaks out of while loop for f.old==0, make sure 
  # get the right root
  if (f.old == 0)
  {
    root <- xold
    f.root <- f.old
  } 
  else if (f.new == 0)
  {
    root <- xnew
    f.root <- f.new
  } 
  else if (f.switch==0)
  {
    root<-xswitch
    f.root<-f.switch
  } 
  else if (pos.side)
  {  
    root <- ifelse(f.new > 0,
                   xnew,
                   xswitch) 
    f.root <- ifelse(f.new > 0,
                     f.new,
                     f.switch) 
  } 
  else 
  { 
    root <- ifelse(f.new < 0,
                   xnew,
                   xswitch)
    f.root <- ifelse(f.new < 0,
                     f.new,
                     f.switch) 
  }  
  # list(xold=xold,f.old=f.old,xswitch=xswitch,f.switch=f.switch,xnew=xnew,f.new=f.new,root=root)
  list(iter = iter,
       f.root = f.root,
       root = root)
}